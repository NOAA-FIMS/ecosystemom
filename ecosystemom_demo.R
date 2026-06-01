knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

library(ecosystemom)
library(FIMS)

# Display setup for tables
scroll_table <- function(data) {
  data |>
    knitr::kable() |>
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed")
    ) |>
    kableExtra::scroll_box(
      height = "300px",
      width = "100%"
    )
}

# Locate package internal files
ewe_nwatlantic_path <- system.file(
  "extdata", "ewe_ecosim_base_nwatlantic",
  package = "ecosystemom"
)

model_years <- 1985:2017

# Load functional groups
functional_groups <- get_functional_groups(
  file_path = fs::path(ewe_nwatlantic_path, "basic_estimates.csv")
)

functional_groups |>
  scroll_table()

# Load and standardize EwE outputs
data_om <- load_model(
  directory = ewe_nwatlantic_path,
  functional_groups = functional_groups,
  type = "ewe_ecosim",
  unit = c(
    "biomass" = "1e^6 mt",
    "catch" = "1e^6 mt",
    "landings" = "1e^6 mt",
    "total_mortality" = "year^-1",
    "weight" = "kg"
  )
)

data_om |>
  head(n = 10) |>
  dplyr::mutate(file_name = "./biomass_monthly.csv") |>
  scroll_table()

data_om |> 
  dplyr::distinct(type, species) |>
  scroll_table()

# Extract "true" values for menhaden
truth_om <- calc_truth(
  data = data_om,
  species_name = "menhaden"
)

truth_om |>
  dplyr::select(-truth_om) |>
  scroll_table()

# Extract and unnest annual catch
catch_index_om <- truth_om |> 
  dplyr::filter(
    truth_label == "catch",
    truth_type == "index",
    truth_time_step == "yearly"
  ) |> 
  tidyr::unnest(cols = c(truth_om)) |>
  dplyr::mutate(
    truth_value = truth_value * 1000000, 
    truth_unit = "mt"
  ) 
  
catch_index_om |>
  scroll_table()

# Extract and unnest annual weight-at-age
weight_agecomp_om <- truth_om |>
  dplyr::filter(
    truth_label == "weight",
    truth_type == "agecomp",
    truth_time_step == "yearly"
  ) |> 
  tidyr::unnest(cols = c(truth_om)) |>
  dplyr::mutate(
    truth_value = truth_value / 1000,
    truth_unit = "mt"
  )

# Extract and unnest annual catch-at-age in numbers
catch_agecomp_om <- truth_om |> 
  dplyr::filter(
    truth_label == "catch",
    truth_type == "agecomp",
    truth_time_step == "yearly"
  ) |>
  tidyr::unnest(cols = c(truth_om)) |>
  dplyr::mutate(
    truth_value = truth_value * 1000000,
    truth_unit = "mt"
  ) |>
  dplyr::left_join(
    weight_agecomp_om |>
      dplyr::select(-species_name, -truth_label, -truth_type, -truth_time_step, -truth_unit), 
    by = c("truth_year", "truth_group"),
    suffix = c("_catch", "_weight")
  ) |>
  dplyr::mutate(
    truth_value = ceiling(truth_value_catch / truth_value_weight), 
    truth_unit = "numbers"
  ) |>
  dplyr::select(-truth_value_catch, -truth_value_weight)

# Extract and unnest annual biomass
biomass_index_om <- truth_om |>
  dplyr::filter(
    truth_label == "biomass",
    truth_type == "index",
    truth_time_step == "yearly"
  ) |> 
  tidyr::unnest(cols = c(truth_om)) |>
  dplyr::mutate(
    truth_value = truth_value * 1000000, 
    truth_unit = "mt"
  )

# Extract and unnest annual number-at-age
number_agecomp_om <- truth_om |>
  dplyr::filter(
    truth_label == "numbers",
    truth_type == "agecomp",
    truth_time_step == "yearly"
  ) |> 
  tidyr::unnest(cols = c(truth_om)) |>
  dplyr::mutate(
    truth_value = ceiling(truth_value * 1000000 / 1000),
    truth_unit = "numbers"
  )

# Extract and unnest annual natural mortality by age
natural_mortality_agecomp_om <- truth_om |>
  dplyr::filter(
    truth_label == "natural_mortality",
    truth_type == "agecomp",
    truth_time_step == "yearly"
  ) |> 
  tidyr::unnest(cols = c(truth_om))

# Extract and unnest annual fishing mortality by age
fishing_mortality_agecomp_om <- truth_om |>
  dplyr::filter(
    truth_label == "fishing_mortality",
    truth_type == "agecomp",
    truth_time_step == "yearly"
  ) |> 
  tidyr::unnest(cols = c(truth_om))

# Extract and unnest annual fishing mortality: apical F
fishing_mortality_index_om <- truth_om |>
  dplyr::filter(
    truth_label == "fishing_mortality",
    truth_type == "index",
    truth_time_step == "yearly"
  ) |>
  tidyr::unnest(cols = c(truth_om))


catch_index_sd <- 0.05
catch_index_sampled <- catch_index_om |> 
  dplyr::mutate(
    sampled_value = sample_lognormal(
      x = truth_value, 
      sd = catch_index_sd
    )
  )

catch_index_sampled |>
  scroll_table()

catch_agecomp_sample_size <- 200
catch_agecomp_sampled <- catch_agecomp_om |> 
  dplyr::group_by(truth_year) |> 
  dplyr::mutate(
    sampled_value = sample_multinomial(
      x = truth_value,
      sample_size = catch_agecomp_sample_size
    )
  ) |> 
  dplyr::ungroup()

ages <- 0:6
names(ages) <- functional_groups |>
  dplyr::filter(species == "menhaden") |>
  dplyr::pull(group)

# Define explicit survey catchability
catchability_survey <- 0.05
selectivity_inflection_point_asc <- 1.2
selectivity_slope_asc <- 2.1
selectivity_inflection_point_desc <- 3.8
selectivity_slope_desc <- 1.2
selectivity_ascending  <- 1 / (1 + exp(-selectivity_slope_asc * (ages - selectivity_inflection_point_asc)))
selectivity_descending <- 1 / (1 + exp(-selectivity_slope_desc * (ages - selectivity_inflection_point_desc)))
selectivity_survey   <- selectivity_ascending * (1 - selectivity_descending)
names(selectivity_survey) <- functional_groups |>
  dplyr::filter(species == "menhaden") |>
  dplyr::pull(group)

# Survey index
survey_data <- number_agecomp_om |>
  dplyr::left_join(
    weight_agecomp_om |>
      dplyr::select(-species_name, -truth_label, -truth_type, -truth_time_step, -truth_unit), 
    by = c("truth_year", "truth_group"),
    suffix = c("_number", "_weight")
  ) |>
  dplyr::mutate(
    selectivity = selectivity_survey[truth_group],
    truth_value_selected_number = ceiling(truth_value_number * selectivity * catchability_survey),
    truth_value_selected_biomass = truth_value_selected_number * truth_value_weight
  )

# Generate observed survey biomass indices with lognormal error (SD = 0.1)
survey_index_sd <- 0.1
survey_index_sampled <- survey_data |> 
  dplyr::select(
    -truth_value_number, -truth_value_weight, -selectivity, -truth_value_selected_number
  ) |>
  dplyr::mutate(
    truth_label = "biomass",
    truth_unit = "mt"
  ) |>
  # Aggregate all ages/groups into one annual value
  dplyr::group_by(truth_year) |>
  dplyr::summarise(
    truth_value = sum(truth_value_selected_biomass),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    species_name = "menhaden",
    truth_label = "biomass",
    truth_type = "index",
    truth_time_step = "yearly",
    truth_unit = "mt"
  ) |>
  dplyr::mutate(
    sampled_value = sample_lognormal(
      x = truth_value, 
      sd = survey_index_sd
    )
  )

# Survey agecomp
survey_agecomp_sample_size <- 200
survey_agecomp_sampled <- survey_data |> 
  dplyr::group_by(truth_year) |> 
  dplyr::mutate(
    sampled_value = sample_multinomial(
      x = truth_value_selected_number,
      sample_size = survey_agecomp_sample_size
    )
  ) |> 
  dplyr::ungroup() |>
  dplyr::select(
    -truth_value_number, -truth_value_weight, -selectivity,
    -truth_value_selected_biomass
  ) |>
  dplyr::rename(truth_value = truth_value_selected_number)

fishing_fleet_name <- "fishing_fleet"
survey_fleet_name <- "survey_fleet"

landings_data <- data.frame(
  type = "landings",
  name = fishing_fleet_name,
  age = NA, 
  timing = model_years,
  value = catch_index_sampled[["sampled_value"]],
  unit = "mt",
  uncertainty = catch_index_sd
)

index_data <- data.frame(
  type = "index",
  name = survey_fleet_name,
  age = NA,
  timing = model_years,
  value = survey_index_sampled[["sampled_value"]],
  unit = "mt",
  uncertainty = survey_index_sd
)

age_data <- rbind(
  data.frame(
    type = "age_comp",
    name = fishing_fleet_name,
    age = unname(ages[catch_agecomp_sampled[["truth_group"]]]),
    timing = catch_agecomp_sampled[["truth_year"]],
    value = catch_agecomp_sampled[["sampled_value"]],
    unit = "number",
    uncertainty = catch_agecomp_sample_size
  ),
  data.frame(
    type = "age_comp",
    name = survey_fleet_name,
    age = unname(ages[survey_agecomp_sampled[["truth_group"]]]),
    timing = survey_agecomp_sampled[["truth_year"]],
    value = survey_agecomp_sampled[["sampled_value"]],
    unit = "number",
    uncertainty = survey_agecomp_sample_size
  )
)

weight_at_age <- data.frame(
  type = "weight_at_age",
  name = fishing_fleet_name,
  age = unname(ages[weight_agecomp_om[["truth_group"]]]),
  timing = weight_agecomp_om[["truth_year"]],
  value = weight_agecomp_om[["truth_value"]],
  unit = "mt",
  uncertainty = NA
)

weight_year_plus <- weight_at_age |>
  dplyr::filter(timing == max(model_years)) |>
  dplyr::mutate(timing = timing + 1)

weight_at_age_data <- dplyr::bind_rows(
  weight_at_age, 
  weight_year_plus
)

data_fims <- rbind(landings_data, index_data, age_data, weight_at_age_data) |>
  dplyr::mutate(
    length = NA, 
    .after = "age"
  ) |>
  FIMS::FIMSFrame()

methods::show(data_fims)

plot(data_fims)

# Generate default FIMS model configurations
default_configurations <- FIMS::create_default_configurations(
  data = data_fims
)

default_configurations |>
  tidyr::unnest(cols = data) |>
  scroll_table()

# Replace the default selectivity model with a double-logistic function
updated_configurations <- default_configurations |>
  tidyr::unnest(cols = data) |>
  dplyr::rows_update(
    y = tibble::tibble(
      module_name = "Selectivity",
      module_type = "DoubleLogistic"
    ),
    by = c("module_name")
  )

updated_configurations |>
  scroll_table()

# Create default parameter values from the updated model configuration
default_parameters <- FIMS::create_default_parameters(
  configurations = updated_configurations,
  data = data_fims
) |>
  tidyr::unnest(cols = data)

# Fishing fleet selectivity
# Option 1: Estimate selectivity from OM fishing mortality-at-age
# Mismatch note: the ecosystem model OM scales fishing selectivity (double 
# logistic) to a maximum of 1, where FIMS does not.
catch_selectivity <- estimate_true_selectivity(
  data = fishing_mortality_agecomp_om,
  ages = ages,
  functional_form = "double_logistic"
) |>
  dplyr::mutate(fleet_name = fishing_fleet_name)

# Option 2: Explicit fishing fleet selectivity parameter values
catch_selectivity_inflection_point_asc <- 1.8
catch_selectivity_slope_asc <- 3.1
catch_selectivity_inflection_point_desc <- 0.01
catch_selectivity_slope_desc <- 0.88

# Estimate maturity parameters
maturity_parameters <- estimate_true_maturity(
  ages = ages,
  spawning_proportion = c(0, 0.1, 0.5, 0.9, 1, 1, 1),
  functional_form = "logistic"
)

# Update parameter values using OM-derived truth information
updated_parameters <- default_parameters |>
  dplyr::filter(!(module_name == "Selectivity" & fleet_name == fishing_fleet_name)) |>
  dplyr::bind_rows(catch_selectivity) |>
  # dplyr::rows_update(
  #   y = tibble::tibble(
  #     fleet_name = fishing_fleet_name,
  #     label = c(
  #       "inflection_point_asc", "slope_asc", 
  #       "inflection_point_desc", "slope_desc"
  #     ),
  #     value = c(
  #       catch_selectivity_inflection_point_asc,
  #       catch_selectivity_slope_asc,
  #       catch_selectivity_inflection_point_desc,
  #       catch_selectivity_slope_desc
  #     )
  #   ),
  #   by = c("fleet_name", "label")
  # ) |>
  dplyr::rows_update(
    y = tibble::tibble(
      fleet_name = fishing_fleet_name,
      label = "log_Fmort",
      time = fishing_mortality_index_om[["truth_year"]],
      value = fishing_mortality_index_om[["truth_value"]] |>
        log()
    ), 
    by = c("fleet_name", "label", "time")
  ) |> 
  dplyr::rows_update(
    y = tibble::tibble(
      fleet_name = survey_fleet_name,
      label = c(
        "inflection_point_asc", "slope_asc", 
        "inflection_point_desc", "slope_desc", 
        "log_q"
      ),
      value = c(
        selectivity_inflection_point_asc,
        selectivity_slope_asc,
        selectivity_inflection_point_desc,
        selectivity_slope_desc,
        log(catchability_survey)
      )
    ),
    by = c("fleet_name", "label")
  ) |>
  dplyr::rows_update(
    y = tibble::tibble(
      label = "log_rzero", 
      module_type = "BevertonHolt",
      value = number_agecomp_om |>
        dplyr::filter(truth_group  == "0", truth_year == model_years[1]) |>
        dplyr::pull(truth_value) |>
        log()
    ),
    by = c("label", "module_type")
  ) |>
  dplyr::rows_update(
    y = tibble::tibble(
      label = "logit_steep", 
      module_type = "BevertonHolt",
      # calculate from vulnerability matrix: v / (v + 1)
      # v = 411.23 + 1.02 + 191.58 + 2 + 1016.36 + 12.18 + 2 + 403.26 = 2039.63
      # h = v / (v + 1) = 0.99
      value = -log(1.0 - 0.99) + log(0.99 - 0.2),
      estimation_type = "fixed_effects"
    ),
    by = c("label", "module_type")
  ) |>
  dplyr::filter(!(module_name == "Maturity")) |>
  dplyr::bind_rows(maturity_parameters) |>
  # dplyr::rows_update(
  #   y = tibble::tibble(
  #     label = "log_M", 
  #     age = unname(ages[natural_mortality_agecomp_om[["truth_group"]]]),
  #     time = natural_mortality_agecomp_om[["truth_year"]],
  #     value = log(natural_mortality_agecomp_om[["truth_value"]])
  #   ),
  #   by = c("label", "age", "time")
  # ) |>
  dplyr::rows_update(
    y = tibble::tibble(
      label = "log_init_naa",
      age = number_agecomp_om |>
        dplyr::filter(truth_year == model_years[1]) |>
        dplyr::pull(truth_group) |>
        (\(x) unname(ages[x]))(),
      value = number_agecomp_om |>
        dplyr::filter(truth_year == model_years[1]) |>
        dplyr::pull(truth_value) |>
        log()*2
    ),
    by = c("label", "age")
  )

# Display updated parameter table
updated_parameters |>
  scroll_table()

# Initialize and fit the FIMS estimation model
fit_fims <- updated_parameters |>
  FIMS::initialize_fims((data = data_fims)) |>
  FIMS::fit_fims(optimize = TRUE)

# Extract estimates
estimates_fims <- FIMS::get_estimates(fit_fims)

FIMS::clear()

subdata <- estimates_fims |>
  dplyr::filter(
    # module_name == "Selectivity"
    label == "landings_expected", 
    module_id == 1
  )

plot(
  subdata[["year_i"]], 
  subdata[["estimated"]], 
  type = "l", 
  col = "blue", 
  ylim = range(c(subdata[["estimated"]], subdata[["observed"]])),
  ylab = "Value", 
  xlab = "Time"
)
lines(subdata[["year_i"]], subdata[["observed"]], col = "red")



subdata <- estimates_fims |>
  dplyr::filter(
    # module_name == "Selectivity"
    label == "log_index_expected", 
    module_id == 2
  )

plot(
  subdata[["year_i"]], 
  exp(subdata[["estimated"]]), 
  type = "l", 
  col = "blue", 
  ylim = range(c(exp(subdata[["estimated"]]), survey_index_sampled[["sampled_value"]])),
  ylab = "Value", 
  xlab = "Time"
)
lines(subdata[["year_i"]], survey_index_sampled[["sampled_value"]], col = "red")


estimates_selectivity_inflection_point_asc <- estimates_fims |>
  dplyr::filter(module_id == 1, label == "inflection_point_asc") |>
  dplyr::pull(estimated)
estimates_selectivity_slope_asc <- estimates_fims |>
  dplyr::filter(module_id == 1, label == "slope_asc") |>
  dplyr::pull(estimated)
estimates_selectivity_inflection_point_desc <- estimates_fims |>
  dplyr::filter(module_id == 1, label == "inflection_point_desc") |>
  dplyr::pull(estimated)
estimates_selectivity_slope_desc <- estimates_fims |>
  dplyr::filter(module_id == 1, label == "slope_desc") |>
  dplyr::pull(estimated)
estimates_selectivity_ascending  <- 1 / (1 + exp(-estimates_selectivity_slope_asc * (ages - estimates_selectivity_inflection_point_asc)))
estimates_selectivity_descending <- 1 / (1 + exp(-estimates_selectivity_slope_desc * (ages - estimates_selectivity_inflection_point_desc)))
estimates_selectivity_max <- max(estimates_selectivity_descending * (1 - estimates_selectivity_descending))

biomass_fims <- estimates_fims |>
  dplyr::filter(label == "biomass") |>
  dplyr::mutate(year = model_years[year_i]) |>
  dplyr::select(year, FIMS = estimated) # |>
  # dplyr::mutate(FIMS = FIMS * estimates_selectivity_max)

biomass_om <- biomass_index_om |>
  dplyr::select(year = truth_year, OM = truth_value)

ratio <- max(biomass_fims[["FIMS"]], na.rm = TRUE) / 
         max(biomass_om[["OM"]], na.rm = TRUE)

biomass_fims |> 
  dplyr::left_join(biomass_om, by = "year") |> 
  dplyr::filter(year %in% model_years) |>
  ggplot2::ggplot(ggplot2::aes(x = year)) +
  ggplot2::geom_line(ggplot2::aes(y = FIMS, color = "FIMS"), linewidth = 1.2) +
  ggplot2::geom_line(ggplot2::aes(y = OM * ratio, color = "OM"), linewidth = 1.2, linetype = "dashed") +
  ggplot2::scale_y_continuous(
    name = "FIMS Biomass",
    sec.axis = ggplot2::sec_axis(~ . / ratio, name = "OM Biomass")
  ) +
  ggplot2::scale_color_manual(values = c("FIMS" = "#1f77b4", "OM" = "#ff7f0e")) +
  ggplot2::theme_minimal(base_size = 14) +
  ggplot2::labs(
    x = "Model Year",
    color = "Source"
  )

recruitment_fims <- estimates_fims |>
  dplyr::filter(label == "expected_recruitment") |>
  dplyr::mutate(year = model_years[year_i]) |>
  dplyr::select(year, FIMS = estimated) # |>
  # dplyr::mutate(FIMS = FIMS * estimates_selectivity_max)

recruitment_om <- number_agecomp_om |>
  dplyr::filter(truth_group == "0") |>
  dplyr::select(year = truth_year, OM = truth_value)

ratio <- max(recruitment_fims[["FIMS"]], na.rm = TRUE) / 
         max(recruitment_om[["OM"]], na.rm = TRUE)

recruitment_fims |> 
  dplyr::left_join(recruitment_om, by = "year") |> 
  dplyr::filter(year %in% model_years) |>
  ggplot2::ggplot(ggplot2::aes(x = year)) +
  ggplot2::geom_line(ggplot2::aes(y = FIMS, color = "FIMS"), linewidth = 1.2) +
  ggplot2::geom_line(ggplot2::aes(y = OM * ratio, color = "OM"), linewidth = 1.2, linetype = "dashed") +
  ggplot2::scale_y_continuous(
    name = "FIMS Recruitment",
    sec.axis = ggplot2::sec_axis(~ . / ratio, name = "OM Recruitment")
  ) +
  ggplot2::scale_color_manual(values = c("FIMS" = "#1f77b4", "OM" = "#ff7f0e")) +
  ggplot2::theme_minimal(base_size = 14) +
  ggplot2::labs(
    x = "Model Year",
    color = "Source"
  )

f_fims <- estimates_fims |>
  dplyr::filter(label == "log_Fmort",  module_id == 1) |>
  dplyr::mutate(year = model_years[year_i]) |>
  dplyr::select(year, FIMS = estimated) |>
  dplyr::mutate(FIMS = exp(FIMS)) # |>
  # dplyr::mutate(FIMS = FIMS * estimates_selectivity_max)

f_om <- fishing_mortality_index_om |>
  dplyr::select(year = truth_year, OM = truth_value)

ratio <- max(f_fims[["FIMS"]], na.rm = TRUE) / 
         max(f_om[["OM"]], na.rm = TRUE)

f_fims |> 
  dplyr::left_join(f_om, by = "year") |> 
  dplyr::filter(year %in% model_years) |>
  ggplot2::ggplot(ggplot2::aes(x = year)) +
  ggplot2::geom_line(ggplot2::aes(y = FIMS, color = "FIMS"), linewidth = 1.2) +
  ggplot2::geom_line(ggplot2::aes(y = OM * ratio, color = "OM"), linewidth = 1.2, linetype = "dashed") +
  ggplot2::scale_y_continuous(
    name = "FIMS Fishing Mortality",
    sec.axis = ggplot2::sec_axis(~ . / ratio, name = "OM Fishing Mortality")
  ) +
  ggplot2::scale_color_manual(values = c("FIMS" = "#1f77b4", "OM" = "#ff7f0e")) +
  ggplot2::theme_minimal(base_size = 14) +
  ggplot2::labs(
    x = "Model Year"
  )

# Load diet composition data
data_diet_composition <- load_diet_composition(
  file.path(ewe_nwatlantic_path , "diet_composition.csv")
)

data <- tibble::tibble(
  data_om = list(data_om),
  data_diet_composition = list(data_diet_composition)
)

sem <- create_dsem_inputs(
  data = data,
  focal_functional_group = "menhaden 0",
  diet_composition_threshold = 0.05
)

sem[["sem_tibble"]][[1]] |>
  scroll_table()

sem[["sem_lines"]]

# Fit Dynamic Structural Equation Model (DSEM)
fit_dsem <- dsem::dsem(
  sem = sem[["sem_lines"]],
  tsdata = sem[["data_time_series_sem"]][[1]],
  control = dsem::dsem_control(quiet = TRUE)
)

# Display model summary
fit_dsem |>
  summary() |>
  dplyr::select(path, Estimate, Std_Error, p_value) |>
  knitr::kable(digits = 3)
