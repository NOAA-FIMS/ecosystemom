# R/functions.R
library(ecosystemom)
library(FIMS)
library(dplyr)
library(tidyr)
library(fs)

# --- 1. Load OM ---
load_om <- function() {
  ewe_nwatlantic_path <- system.file(
    "extdata", "ewe_ecosim_base_nwatlantic",
    package = "ecosystemom"
  )
  
  functional_groups <- get_functional_groups(
    file_path = fs::path(ewe_nwatlantic_path, "basic_estimates.csv")
  )
  
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
  
  list(
    data = data_om, 
    fg = functional_groups, 
    model_years = 1985:2017
  )
}

# --- 2. Extract Truth ---
extract_om_truth <- function(om_setup, weight_scalar) {
  truth_om <- get_truth(
    data = om_setup[["data"]],
    species_name = "menhaden"
  )
  
  catch_index_om <- truth_om |>
    dplyr::filter(truth_label == "catch", truth_type == "index", truth_time_step == "yearly") |>
    tidyr::unnest(cols = c(truth_om)) |>
    dplyr::mutate(truth_value = truth_value * 1000000, truth_unit = "mt")
  
  weight_agecomp_om <- truth_om |>
    dplyr::filter(truth_label == "weight", truth_type == "agecomp", truth_time_step == "yearly") |>
    tidyr::unnest(cols = c(truth_om)) |>
    dplyr::mutate(truth_value = truth_value / weight_scalar, truth_unit = "mt")
  
  catch_agecomp_om <- truth_om |>
    dplyr::filter(truth_label == "catch", truth_type == "agecomp", truth_time_step == "yearly") |>
    tidyr::unnest(cols = c(truth_om)) |>
    dplyr::mutate(truth_value = truth_value * 1000000, truth_unit = "mt") |>
    dplyr::left_join(
      weight_agecomp_om |> 
        dplyr::select(-species_name, -truth_label, -truth_type, -truth_time_step, -truth_unit),
      by = c("truth_year", "truth_group"),
      suffix = c("_catch", "_weight")
    ) |>
    dplyr::mutate(truth_value = ceiling(truth_value_catch / truth_value_weight), truth_unit = "numbers") |>
    dplyr::select(-truth_value_catch, -truth_value_weight)
  
  biomass_index_om <- truth_om |>
    dplyr::filter(truth_label == "biomass", truth_type == "index", truth_time_step == "yearly") |>
    tidyr::unnest(cols = c(truth_om)) |>
    dplyr::mutate(truth_value = truth_value * 1000000, truth_unit = "mt")
  
  number_agecomp_om <- truth_om |>
    dplyr::filter(truth_label == "numbers", truth_type == "agecomp", truth_time_step == "yearly") |>
    tidyr::unnest(cols = c(truth_om)) |>
    dplyr::mutate(truth_value = ceiling(truth_value * 1000000 * weight_scalar), truth_unit = "numbers")
  
  natural_mortality_agecomp_om <- truth_om |>
    dplyr::filter(truth_label == "natural_mortality", truth_type == "agecomp", truth_time_step == "yearly") |>
    tidyr::unnest(cols = c(truth_om))
  
  fishing_mortality_agecomp_om <- truth_om |>
    dplyr::filter(truth_label == "fishing_mortality", truth_type == "agecomp", truth_time_step == "yearly") |>
    tidyr::unnest(cols = c(truth_om))
  
  fishing_mortality_index_om <- truth_om |>
    dplyr::filter(truth_label == "fishing_mortality", truth_type == "index", truth_time_step == "yearly") |>
    tidyr::unnest(cols = c(truth_om))
  
  list(
    catch_index_om = catch_index_om,
    weight_agecomp_om = weight_agecomp_om,
    catch_agecomp_om = catch_agecomp_om,
    biomass_index_om = biomass_index_om,
    number_agecomp_om = number_agecomp_om,
    natural_mortality_agecomp_om = natural_mortality_agecomp_om,
    fishing_mortality_agecomp_om = fishing_mortality_agecomp_om,
    fishing_mortality_index_om = fishing_mortality_index_om
  )
}

# --- 3. Simulate Sampled Data ---
simulate_data <- function(om_setup, truth_list, catch_ss, survey_ss, index_sd) {
  ages <- 0:6
  names(ages) <- om_setup[["fg"]] |>
    dplyr::filter(species == "menhaden") |>
    dplyr::pull(group)
  
  catch_index_sampled <- truth_list[["catch_index_om"]] |>
    dplyr::mutate(sampled_value = sample_lognormal(x = truth_value, sd = index_sd))
  
  catch_agecomp_sampled <- truth_list[["catch_agecomp_om"]] |>
    dplyr::group_by(truth_year) |>
    dplyr::mutate(sampled_value = sample_multinomial(x = truth_value, sample_size = catch_ss)) |>
    dplyr::ungroup()
  
  catchability_survey <- 0.05
  selectivity_inflection_point_asc <- -5
  selectivity_slope_asc <- 5
  selectivity_inflection_point_desc <- 0.55
  selectivity_slope_desc <- 3.30
  
  selectivity_ascending  <- 1 / (1 + exp(-selectivity_slope_asc * (ages - selectivity_inflection_point_asc)))
  selectivity_descending <- 1 / (1 + exp(-selectivity_slope_desc * (ages - selectivity_inflection_point_desc)))
  selectivity_survey <- selectivity_ascending * (1 - selectivity_descending)
  names(selectivity_survey) <- names(ages)
  
  survey_data <- truth_list[["number_agecomp_om"]] |>
    dplyr::left_join(
      truth_list[["weight_agecomp_om"]] |> 
        dplyr::select(-species_name, -truth_label, -truth_type, -truth_time_step, -truth_unit),
      by = c("truth_year", "truth_group"),
      suffix = c("_number", "_weight")
    ) |>
    dplyr::mutate(
      selectivity = selectivity_survey[truth_group],
      truth_value_selected_number = ceiling(truth_value_number * selectivity * catchability_survey),
      truth_value_selected_biomass = truth_value_selected_number * truth_value_weight
    )
  
  survey_index_sampled <- survey_data |>
    dplyr::select(-truth_value_number, -truth_value_weight, -selectivity, -truth_value_selected_number) |>
    dplyr::group_by(truth_year) |>
    dplyr::summarise(truth_value = sum(truth_value_selected_biomass), .groups = "drop") |>
    dplyr::mutate(
      species_name = "menhaden", truth_label = "biomass", truth_type = "index",
      truth_time_step = "yearly", truth_unit = "mt"
    ) |>
    dplyr::mutate(sampled_value = sample_lognormal(x = truth_value, sd = 0.1)) 
  
  survey_agecomp_sampled <- survey_data |>
    dplyr::group_by(truth_year) |>
    dplyr::mutate(sampled_value = sample_multinomial(x = truth_value_selected_number, sample_size = survey_ss)) |>
    dplyr::ungroup() |>
    dplyr::select(-truth_value_number, -truth_value_weight, -selectivity, -truth_value_selected_biomass) |>
    dplyr::rename(truth_value = truth_value_selected_number)
  
  list(
    catch_index = catch_index_sampled,
    catch_agecomp = catch_agecomp_sampled,
    survey_index = survey_index_sampled,
    survey_agecomp = survey_agecomp_sampled,
    ages = ages,
    catchability_survey = catchability_survey,
    selectivity_inflection_point_asc = selectivity_inflection_point_asc,
    selectivity_slope_asc = selectivity_slope_asc,
    selectivity_inflection_point_desc = selectivity_inflection_point_desc,
    selectivity_slope_desc = selectivity_slope_desc
  )
}

# --- 4. Build & Fit FIMS Scenario ---
fit_fims_scenario <- function(om_setup, truth_list, sim_list, index_sd, catch_ss, survey_ss, fish_sel_vec, srv_sel_vec, init_naa_vec, log_sd_proxy) {
  fish_sel_vec <- unlist(fish_sel_vec)
  srv_sel_vec  <- unlist(srv_sel_vec)
  init_naa_vec <- unlist(init_naa_vec)

  model_years <- om_setup[["model_years"]]
  ages <- sim_list[["ages"]]
  fishing_fleet_name <- "fishing_fleet"
  survey_fleet_name <- "survey_fleet"
  
  # Prepare FIMS-compatible data
  landings_data <- data.frame(
    type = "landings", name = fishing_fleet_name, age = NA, timing = model_years,
    value = sim_list[["catch_index"]][["sampled_value"]], unit = "mt", uncertainty = index_sd
  )
  
  index_data <- data.frame(
    type = "index", name = survey_fleet_name, age = NA, timing = model_years,
    value = sim_list[["survey_index"]][["sampled_value"]], unit = "mt", uncertainty = 0.1
  )
  
  age_data <- rbind(
    data.frame(
      type = "age_comp", name = fishing_fleet_name, age = unname(ages[sim_list[["catch_agecomp"]][["truth_group"]]]),
      timing = sim_list[["catch_agecomp"]][["truth_year"]], value = sim_list[["catch_agecomp"]][["sampled_value"]],
      unit = "number", uncertainty = catch_ss
    ),
    data.frame(
      type = "age_comp", name = survey_fleet_name, age = unname(ages[sim_list[["survey_agecomp"]][["truth_group"]]]),
      timing = sim_list[["survey_agecomp"]][["truth_year"]], value = sim_list[["survey_agecomp"]][["sampled_value"]],
      unit = "number", uncertainty = survey_ss
    )
  )
  
  weight_at_age <- data.frame(
    type = "weight_at_age", name = fishing_fleet_name, age = unname(ages[truth_list[["weight_agecomp_om"]][["truth_group"]]]),
    timing = truth_list[["weight_agecomp_om"]][["truth_year"]], value = truth_list[["weight_agecomp_om"]][["truth_value"]],
    unit = "mt", uncertainty = NA
  )
  
  weight_year_plus <- weight_at_age |>
    dplyr::filter(timing == max(model_years)) |>
    dplyr::mutate(timing = timing + 1)
  
  weight_at_age_data <- dplyr::bind_rows(weight_at_age, weight_year_plus)
  
  data_fims <- rbind(landings_data, index_data, age_data, weight_at_age_data) |>
    dplyr::mutate(length = NA, .after = "age") |>
    FIMS::FIMSFrame()
  
  # Configure Parameters
  default_configurations <- FIMS::create_default_configurations(data = data_fims)
  updated_configurations <- default_configurations |>
    tidyr::unnest(cols = data) |>
    dplyr::rows_update(
      y = tibble::tibble(module_name = "Selectivity", module_type = "DoubleLogistic"),
      by = c("module_name")
    )
  
  default_parameters <- FIMS::create_default_parameters(
    configurations = updated_configurations, data = data_fims
  ) |> tidyr::unnest(cols = data)
  
  catch_selectivity_inflection_point_asc <- 1.8
  catch_selectivity_slope_asc <- 3.1
  catch_selectivity_inflection_point_desc <- 0.01
  catch_selectivity_slope_desc <- 0.88

  selectivity_ascending  <- 1 / (1 + exp(-catch_selectivity_slope_asc * (ages - catch_selectivity_inflection_point_asc)))
  selectivity_descending <- 1 / (1 + exp(-catch_selectivity_slope_desc * (ages - catch_selectivity_inflection_point_desc)))
  selectivity_catch_unscaled <- selectivity_ascending * (1 - selectivity_descending)
  s_max_fims <- max(selectivity_catch_unscaled)
  
  maturity_parameters <- estimate_true_maturity(
    ages = ages, spawning_proportion = c(0, 0.1, 0.5, 0.9, 1, 1, 1), functional_form = "logistic"
  )
  
  recruitment_ewe <- truth_list[["number_agecomp_om"]] |>
    dplyr::filter(truth_group == "0", truth_year != model_years[1]) |>
    dplyr::pull(truth_value)
  #log_sd_proxy <- log(sd(log(recruitment_ewe) - mean(log(recruitment_ewe))))
  log_sd_proxy <- log_sd_proxy
  
  updated_parameters <- default_parameters |>
    dplyr::rows_update(
      y = tibble::tibble(
        fleet_name = fishing_fleet_name,
        label = c("inflection_point_asc", "slope_asc", "inflection_point_desc", "slope_desc"),
        estimation_type = fish_sel_vec,
        value = c(
          catch_selectivity_inflection_point_asc, 
          catch_selectivity_slope_asc, 
          catch_selectivity_inflection_point_desc, 
          catch_selectivity_slope_desc
        )
      ), by = c("fleet_name", "label")
    ) |>
    dplyr::rows_update(
      y = tibble::tibble(
        fleet_name = fishing_fleet_name, label = "log_Fmort",
        time = truth_list[["fishing_mortality_index_om"]][["truth_year"]],
        value = log(truth_list[["fishing_mortality_index_om"]][["truth_value"]])
      ), by = c("fleet_name", "label", "time")
    ) |>
    dplyr::rows_update(
      y = tibble::tibble(
        fleet_name = survey_fleet_name,
        label = c("inflection_point_asc", "slope_asc", "inflection_point_desc", "slope_desc", "log_q"),
        estimation_type = srv_sel_vec,
        value = c(
          sim_list[["selectivity_inflection_point_asc"]], 
          sim_list[["selectivity_slope_asc"]], 
          sim_list[["selectivity_inflection_point_desc"]], 
          sim_list[["selectivity_slope_desc"]], 
          log(sim_list[["catchability_survey"]])
        )
      ), by = c("fleet_name", "label")
    ) |>
    dplyr::rows_update(
      y = tibble::tibble(
        label = "log_rzero", module_type = "BevertonHolt",
        value = truth_list[["number_agecomp_om"]] |> dplyr::filter(truth_group == "0") |> dplyr::pull(truth_value) |> mean() |> log()
      ), by = c("label", "module_type")
    ) |>
    dplyr::rows_update(
      y = tibble::tibble(
        label = "logit_steep", module_type = "BevertonHolt",
        value = -log(1.0 - 0.99) + log(0.99 - 0.2)
      ), by = c("label", "module_type")
    ) |>
    dplyr::rows_update(
      y = tibble::tibble(
        label = "log_sd", module_type = "BevertonHolt", value = log_sd_proxy, estimation_type = "constant"
      ), by = c("label", "module_type")
    ) |>
    dplyr::rows_update(
      y = tibble::tibble(
        label = "log_devs", module_type = "BevertonHolt", estimation_type = "fixed_effects"
      ), by = c("label", "module_type")
    ) |>
    dplyr::filter(!(module_name == "Maturity")) |>
    dplyr::bind_rows(maturity_parameters) |>
    dplyr::rows_update(
      y = tibble::tibble(
        label = "log_M", age = unname(ages[truth_list[["natural_mortality_agecomp_om"]][["truth_group"]]]),
        time = truth_list[["natural_mortality_agecomp_om"]][["truth_year"]],
        value = log(truth_list[["natural_mortality_agecomp_om"]][["truth_value"]])
      ), by = c("label", "age", "time")
    ) |>
    dplyr::rows_update(
      y = tibble::tibble(
        label = "log_init_naa",
        age = truth_list[["number_agecomp_om"]] |> dplyr::filter(truth_year == model_years[1]) |> dplyr::pull(truth_group) |> (\(x) unname(ages[x]))(),
        value = log(truth_list[["number_agecomp_om"]] |> dplyr::filter(truth_year == model_years[1]) |> dplyr::pull(truth_value)),
        estimation_type = init_naa_vec
      ), by = c("label", "age")
    )
  
  # Try/Catch wrap for fitting to prevent pipeline crash if singular
  fit_results <- updated_parameters |>
    FIMS::initialize_fims((data = data_fims)) |>
    FIMS::fit_fims(
      optimize = TRUE,
      control = list(eval.max = 50000, iter.max = 30000, trace = 0)
    )
  
  # Extract values directly using S4 slot syntax (@) and built-in functions
  estimates_fims <- FIMS::get_estimates(fit_results) |>
    dplyr::mutate(
      estimated = dplyr::if_else(
        label == "log_Fmort" & module_id == 1,
        log(exp(estimated) * 0.09),
        estimated # Leaves all other rows exactly as they were
      )
    )
  max_grad <- FIMS::get_max_gradient(fit_results)
  converged <- fit_results@opt[["convergence"]] == 0
  
  FIMS::clear()

  list(
    converged    = converged,
    max_gradient = max_grad,
    estimates    = estimates_fims
  )
}

# --- 5. Plot Diagnostics ---
plot_scenario_diagnostics <- function(om_setup, truth_list, fit_results) {
  # Skip plotting if the model failed or if it is a Bayesian run without MLE estimates
  if (is.null(fit_results[["estimates"]])) {
    return(NULL)
  }
  
  model_years <- om_setup[["model_years"]]
  estimates_fims <- fit_results[["estimates"]]
  
  # Ensure 'year' exists in the estimates data frame by mapping from 'year_i'
  year_lookup <- data.frame(
    year_i = 1:(length(model_years) + 1),
    year = c(model_years, max(model_years) + 1)
  )
  
  if (!"year" %in% colnames(estimates_fims)) {
    estimates_fims <- estimates_fims |>
      dplyr::left_join(year_lookup, by = "year_i") |>
      dplyr::mutate(
        uncertainty_label = "se",  # Required by stockplotr
        estimate = estimated,      # Required by stockplotr
        age = age_i                # Required by stockplotr
      )
  }
  
  shared_scales <- list(
    ggplot2::scale_linetype_manual(
      name = "Model",
      labels = c("EM", "OM"),
      values = c("solid", "dashed") 
    ),
    ggplot2::scale_color_manual(
      name = "Model",
      labels = c("EM", "OM"),
      values = c(
        "EM" = "black",
        "OM" = "#003087"
      )
    )
  )
  
  # --- Plot 1: Biomass ---
  biomass_om <- truth_list[["biomass_index_om"]] |>
    dplyr::select(year = truth_year, OM = truth_value)
  
  biomass_em <- stockplotr::filter_data(
    estimates_fims |>
      dplyr::filter(
        label == "biomass",
        year %in% model_years
      ),
    label_name = "biomass",
    geom = "line"
  ) |>
    dplyr::mutate(group_var = "EM")
  
  p_biomass <- stockplotr::plot_timeseries(
    biomass_em,
    x = "year",
    y = "estimate",
    ylab = "biomass (metric ton)"
  ) +
    stockplotr::theme_noaa() +
    ggplot2::geom_line(
      data = biomass_om, 
      ggplot2::aes(x = year, y = OM, color = "OM"),
      linetype = "dashed"
    ) +
    shared_scales
  
  # --- Plot 2: Recruitment ---
  recruitment_om <- truth_list[["number_agecomp_om"]] |>
    dplyr::filter(truth_group == "0") |>
    dplyr::select(year = truth_year, OM = truth_value)
  
  recruitment_em <- stockplotr::filter_data(
    estimates_fims |>
      dplyr::filter(
        label == "expected_recruitment",
        year %in% model_years
      ),
    label_name = "expected_recruitment",
    geom = "line"
  ) |>
    dplyr::mutate(group_var = "EM")
  
  p_recruitment <- stockplotr::plot_timeseries(
    recruitment_em,
    x = "year",
    y = "estimate",
    ylab = "recruitment (number)"
  ) +
    stockplotr::theme_noaa() +
    ggplot2::geom_line(
      data = recruitment_om, 
      ggplot2::aes(x = year, y = OM, color = "OM"),
      linetype = "dashed"
    ) +
    shared_scales
  
  # --- Plot 3: Fishing Mortality ---
  f_om <- truth_list[["fishing_mortality_index_om"]] |>
    dplyr::select(year = truth_year, OM = truth_value) |>
    dplyr::mutate(OM = log(OM))
  
  f_em <- stockplotr::filter_data(
    estimates_fims |> 
      dplyr::filter(module_id == 1),
    label_name = "log_Fmort$",
    geom = "line"
  ) |>
    dplyr::mutate(group_var = "EM")
  
  p_fmort <- stockplotr::plot_timeseries(
    f_em,
    x = "year",
    y = "estimate",
    ylab = "natural log of Fishing Mortality"
  ) +
    stockplotr::theme_noaa() +
    ggplot2::geom_line(
      data = f_om, 
      ggplot2::aes(x = year, y = OM, color = "OM"),
      linetype = "dashed"
    ) +
    ggplot2::scale_linetype_manual(
      name = "Model",
      labels = c("EM", "OM"),
      values = c("solid", "dashed") 
    ) +
    ggplot2::scale_color_manual(
      name = "Model",
      labels = c("OM", "EM"),
      values = c(
        "OM" = "#003087",
        "EM" = "black"
      )
    )
  
  # Return the figures as a named list
  list(
    biomass = p_biomass,
    recruitment = p_recruitment,
    fmort = p_fmort
  )
}

calculate_scenario_mismatches <- function(om_setup, truth_list, fit_results) {
  if (is.null(fit_results[["estimates"]])) {
    return(tibble::tibble(metric = c("biomass", "recruitment", "fmort"), rmse = NA_real_, mare = NA_real_))
  }
  
  model_years <- om_setup[["model_years"]]
  estimates_fims <- fit_results[["estimates"]]
  
  year_lookup <- data.frame(year_i = 1:(length(model_years) + 1), year = c(model_years, max(model_years) + 1))
  if (!"year" %in% colnames(estimates_fims)) {
    estimates_fims <- estimates_fims |> dplyr::left_join(year_lookup, by = "year_i")
  }
  
  biomass_om <- truth_list[["biomass_index_om"]] |> dplyr::select(year = truth_year, true_val = truth_value)
  biomass_em <- estimates_fims |> dplyr::filter(label == "biomass", year %in% model_years) |> dplyr::select(year, est_val = estimated)
  biomass_joined <- dplyr::inner_join(biomass_em, biomass_om, by = "year")
  
  recruitment_om <- truth_list[["number_agecomp_om"]] |> dplyr::filter(truth_group == "0") |> dplyr::select(year = truth_year, true_val = truth_value)
  recruitment_em <- estimates_fims |> dplyr::filter(label == "expected_recruitment", year %in% model_years) |> dplyr::select(year, est_val = estimated)
  recruitment_joined <- dplyr::inner_join(recruitment_em, recruitment_om, by = "year")
  
  f_om <- truth_list[["fishing_mortality_index_om"]] |> dplyr::select(year = truth_year, true_val = truth_value) |> dplyr::mutate(true_val = log(true_val))
  f_em <- estimates_fims |> dplyr::filter(module_id == 1, grepl("log_Fmort", label)) |> dplyr::select(year, est_val = estimated)
  f_joined <- dplyr::inner_join(f_em, f_om, by = "year")
  
  compute_summary <- function(df, label_name) {
    df |>
      dplyr::mutate(
        raw_error = est_val - true_val,
        rel_error = (est_val - true_val) / true_val
      ) |>
      dplyr::summarise(
        metric = label_name,
        rmse   = sqrt(mean(raw_error^2, na.rm = TRUE)),
        mare   = median(abs(rel_error), na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  dplyr::bind_rows(
    compute_summary(biomass_joined, "biomass"),
    compute_summary(recruitment_joined, "recruitment"),
    compute_summary(f_joined, "fmort")
  )
}