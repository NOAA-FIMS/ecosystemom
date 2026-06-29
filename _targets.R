# _targets.R
library(targets)
library(tarchetypes)

# Source all custom functions
tar_source("Rscripts/targets_helper.R")

# Configure target settings
tar_option_set(
  packages = c("ecosystemom", "FIMS", "dplyr", "tidyr", "fs"),
  seed = 1234
)

# Helper function to generate all combinations of "constant" and "fixed_effects"
get_est_combos <- function(n) {
  # Create a matrix of all possible pairs of indices from 1 to n
  idx_matrix <- utils::combn(n, 0)
  
  # For each column (pair) in the matrix, generate the parameter vector
  combos <- purrr::map(seq_len(ncol(idx_matrix)), function(col_idx) {
    # Start with a base vector of all constants
    vec <- rep("constant", n)
    # Turn exactly two positions into fixed_effects based on the combination index
    vec[idx_matrix[, col_idx]] <- "fixed_effects"
    return(vec)
  })
  
  return(combos)
}

# Define the scenario grid to test various sample sizes and errors
scenario_grid <- tidyr::expand_grid(
  catch_ss          = c(150, 200),
  survey_ss         = c(150, 200),
  index_sd          = c(0.1),
  weight_scalar     = c(100, 1000),
  log_sd_proxy = seq(0.5, 1.5, 0.1) |> log(),
  fish_sel_vec  = get_est_combos(4), 
  # fish_sel_vec  = get_est_combos(4)[c(-length(get_est_combos(4)))]
  # Generates 32 vectors (all combos of 5 params)
  srv_sel_vec   = get_est_combos(5),
  # Kept static for the example to prevent 131,000+ models. 
  # Change to get_est_combos(7) to test all 128 combinations!
  init_naa_vec  = get_est_combos(7)
) |>
  dplyr::mutate(
    scenario_id = dplyr::row_number(),
    .before = 1
  )

list(
  # 1. Load OM data
  tar_target(
    name = om_data,
    command = load_om()
  ),

  # 2. Supply the grid configuration
  tar_target(
    name = scenarios,
    command = scenario_grid
  ),
  
  # 3. Extract specific truths
  tar_target(
    name = om_truth,
    command = extract_om_truth(om_setup = om_data, weight_scalar = scenarios[["weight_scalar"]]),
    pattern = map(scenarios),
    iteration = "list"
  ),
  
  # 4. Generate dynamic simulated data for each scenario
  tar_target(
    name = sim_data,
    command = simulate_data(
      om_setup = om_data,
      truth_list = om_truth,
      catch_ss = scenarios[["catch_ss"]],
      survey_ss = scenarios[["survey_ss"]],
      index_sd = scenarios[["index_sd"]]
    ),
    pattern = map(scenarios, om_truth),
    iteration = "list"
  ),
  
  # 5. Fit the models mapping over each scenario's data
  tar_target(
    name = fims_fits,
    command = fit_fims_scenario(
      om_setup = om_data,
      truth_list = om_truth,
      sim_list = sim_data,
      catch_ss = scenarios[["catch_ss"]],
      survey_ss = scenarios[["survey_ss"]],
      index_sd = scenarios[["index_sd"]],
      log_sd_proxy = scenarios[["log_sd_proxy"]],
      fish_sel_vec  = scenarios[["fish_sel_vec"]],   
      srv_sel_vec   = scenarios[["srv_sel_vec"]],    
      init_naa_vec  = scenarios[["init_naa_vec"]]
    ),
    pattern = map(scenarios, sim_data, om_truth),
    iteration = "list"
  ),

  # 6. Generate diagnostic plots for each scenario
  tar_target(
    name = scenario_plots,
    command = plot_scenario_diagnostics(
      om_setup = om_data,
      truth_list = om_truth,
      fit_results = fims_fits
    ),
    pattern = map(om_truth, fims_fits),
    iteration = "list"
  ),
  
  tar_target(
    name = scenario_mismatches,
    command = calculate_scenario_mismatches(om_setup = om_data, truth_list = om_truth, fit_results = fims_fits),
    pattern = map(om_truth, fims_fits),
    iteration = "list"
  ),

  # 7. Synthesize the convergence diagnostics cleanly
  tar_target(
    name = convergence_summary,
    command = {
      tibble::tibble(
        scenario_id = scenarios[["scenario_id"]],
        catch_ss = scenarios[["catch_ss"]],
        survey_ss = scenarios[["survey_ss"]],
        index_sd = scenarios[["index_sd"]],
        weight_scalar = scenarios[["weight_scalar"]],
        log_sd_proxy = scenarios[["log_sd_proxy"]],
        fish_sel_setup = purrr::map_chr(scenarios[["fish_sel_vec"]], ~ paste(.x, collapse = ", ")),
        srv_sel_setup = purrr::map_chr(scenarios[["srv_sel_vec"]], ~ paste(.x, collapse = ", ")),
        init_naa_setup = purrr::map_chr(scenarios[["init_naa_vec"]], ~ paste(.x, collapse = ", ")),
        converged = purrr::map_lgl(fims_fits, "converged", .default = FALSE),
        max_gradient = purrr::map_dbl(fims_fits, "max_gradient", .default = NA),
        biomass_error      = purrr::map_dbl(scenario_mismatches, ~ .x |> dplyr::filter(metric == "biomass") |> dplyr::pull(rmse)),
        recruitment_error  = purrr::map_dbl(scenario_mismatches, ~ .x |> dplyr::filter(metric == "recruitment") |> dplyr::pull(rmse)),
        fmort_error        = purrr::map_dbl(scenario_mismatches, ~ .x |> dplyr::filter(metric == "fmort") |> dplyr::pull(rmse))
      )
    }
  )
)

