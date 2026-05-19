utils::globalVariables(c(
  "driver", "equation", "functional_group_snake_case", "impacted_group",
  "impacted_group_snake_case", "index", "lag", "lag_months", "param_name",
  "predator", "predator_snake_case", "prey", "prey_snake_case", "proportion",
  "target", "threshold"
))

#' Create Structural Equation Model (SEM) Equations and Time Series Data
#'
#' This function prepares the inputs for a Dynamic Structural Equation Model (DSEM).
#' It generates the specific path equations (drivers to targets) and compiles
#' the corresponding time-series data based on environmental links and
#' diet composition thresholds.
#'
#' @param data A tibble/list containing `data_om`, `data_environment`,
#'   and `data_diet_composition`.
#' @param focal_functional_groups Character vector specifying one or more focal
#'   groups of interest (e.g., `c("menhaden 0", "meanhaden 1")`).
#' @param diet_composition_threshold Numeric. The minimum proportion of diet
#'   required to include a predator-prey link in the SEM. Default is `0.1`, which
#'   represent 10% of the diet.
#'
#' @return A tibble with four columns:
#' \describe{
#'   \item{data_time_series_all}{The complete matrix of available time series.}
#'   \item{data_time_series_sem}{The subset of time series used specifically in the SEM.}
#'   \item{sem_tibble}{A detailed tibble of drivers, targets, lags, and link types.}
#'   \item{sem_lines}{A character string formatted for the \code{dsem} package.}
#' }
#'
#' @details
#' The function constructs three types of linkages:
#' \enumerate{
#'   \item \strong{Environmental}: Links environmental indices to impacted functional groups.
#'   \item \strong{Bottom-up}: Links prey biomass to the focal functional group.
#'   \item \strong{Top-down}: Links predator biomass to the focal functional group.
#' }
#'
#' @export
#' @examples
#' # Create mock data for demonstration
#' data_om <- tibble::tibble(
#'   year = rep(1984, 2),
#'   month = rep(1, 2),
#'   functional_group = c("Phytoplankton", "Menhaden (0yr)"),
#'   functional_group_snake_case = c("phytoplankton", "menhaden_0yr"),
#'   type = "biomass",
#'   value = c(100, 50)
#' )
#'
#' data_environment <- tibble::tibble(
#'   index = "amo",
#'   year = 1984,
#'   month = 1,
#'   value = 0.679,
#'   unit = "NA",
#'   lag_months = 1,
#'   impacted_group = "Menhaden (0yr)",
#'   impacted_group_snake_case = "menhaden_0yr"
#' )
#'
#' data_diet_composition <- tibble::tibble(
#'   prey = "Phytoplankton",
#'   predator = "Menhaden (0yr)",
#'   proportion = 0.5,
#'   prey_snake_case = "phytoplankton",
#'   predator_snake_case = "menhaden_0yr"
#' )
#'
#' data <- list(data_om = list(data_om), data_environment = list(data_environment),
#'              data_diet_composition = list(data_diet_composition))
#'
#' # Generate DSEM inputs
#' dsem_inputs <- create_dsem_inputs(data, "Menhaden (0yr)", 0.1)
#'
#' # Extract the SEM equations for use with the dsem package
#' cat(dsem_inputs$sem_lines)
#'
#' # Extract the time series data required for the SEM
#' print(dsem_inputs$data_time_series_sem[[1]])
create_dsem_inputs <- function(
    data,
    focal_functional_groups,
    diet_composition_threshold = 0.1
) {

  # Validate diet_composition_threshold
  if (length(diet_composition_threshold) > 1 && length(diet_composition_threshold) != length(focal_functional_groups)) {
    cli::cli_abort("If 'diet_composition_threshold' is a vector, its length must match the length of 'focal_functional_groups'.")
  }

  # Initialize variables to avoid NULL issues later
  impacted_group_from_environment <- time_series_environment <- NULL
  impacted_group_from_diet <- NULL

  # Prepare environmental time series ---
  # Pivot environmental data to a wide format for joining
  if (!is.null(data[["data_environment"]])){
    time_series_environment <- data[["data_environment"]][[1]] |>
      dplyr::select(index, year, month, value) |>
      tidyr::pivot_wider(names_from = index, values_from = value)

    impacted_group_from_environment <- data[["data_environment"]][[1]] |>
      dplyr::pull(impacted_group) |>
      unique()
  }

  # Prepare diet composition tibble
  if(!is.null(data[["data_diet_composition"]])){
    impacted_group_from_diet <- data[["data_diet_composition"]][[1]] |>
      dplyr::select(prey, predator) |>
      unlist() |>
      unique()
  }

  # Get a unique list of all functional groups involved in any interaction
  all_impacted_groups <- unique(c(impacted_group_from_environment, impacted_group_from_diet))

  # Filter the main operating model data for biomass of impacted groups and pivot to wide format
  time_series_all <- data[["data_om"]][[1]] |>
    dplyr::filter(type == "biomass" & functional_group %in% all_impacted_groups) |>
    dplyr::select(year, month, functional_group_snake_case, value) |>
    tidyr::pivot_wider(names_from = functional_group_snake_case, values_from = value)

  # Join environmental data with diet composition
  if (!is.null(time_series_environment)){
    time_series_all <- time_series_all |>
      dplyr::full_join(time_series_environment, by = c("year", "month"))
  }
  
  # Convert the final data frame to a time series matrix object
  if (nrow(time_series_all) > 0) {
    time_series_all <- time_series_all |>
      dplyr::arrange(year, month) |>
      as.matrix() |>
      stats::ts()
  }

  # Generate SEM Equations
  # Environmental links
  # Initialize empty tibble for cases with no data
  sem_environment <- tibble::tibble(
    driver = character(),
    target = character(),
    lag = numeric(),
    type = character()
  )
  if (!is.null(data[["data_environment"]])) {
    sem_environment <- data[["data_environment"]][[1]] |>
      dplyr::mutate(
        target = impacted_group_snake_case,
        driver = index,
        lag = lag_months,
        type = "environmental"
      ) |>
      dplyr::select(driver, target, lag, type) |>
      dplyr::distinct()
  }

  # Bottom-up Links (Prey -> Focal Group)
  # Initialize empty tibble
  sem_prey <- tibble::tibble( 
    driver = character(),
    target = character(),
    lag = numeric(),
    type = character()
  )
  if (!is.null(data[["data_diet_composition"]])) {
    threshold_map <- tibble::tibble(
      predator = focal_functional_groups,
      threshold = diet_composition_threshold
    )

    # Filter diet data to include only links to the focal group(s) that meet the threshold
    filtered_prey <- data[["data_diet_composition"]][[1]] |>
      dplyr::inner_join(threshold_map, by = "predator") |>
      dplyr::filter(is.na(threshold) | proportion >= threshold)

    if (nrow(filtered_prey) > 0) {
      sem_prey <- filtered_prey |>
        dplyr::mutate(
          driver = prey_snake_case,
          target = predator_snake_case,
          lag = 0,
          type = "bottom_up"
        ) |>
        dplyr::select(driver, target, lag, type) |>
        dplyr::distinct()
    }
  }

  # Top-down Links (Predator -> Focal Group)
  # Initialize empty tibble
  sem_predator <- tibble::tibble( 
    driver = character(),
    target = character(),
    lag = numeric(),
    type = character()
  )
  if (!is.null(data[["data_diet_composition"]])) {
    threshold_map <- tibble::tibble(
      prey = focal_functional_groups,
      threshold = diet_composition_threshold
    )

    # Filter diet data to find predators of the focal group(s) that meet the threshold
    filtered_predator <- data[["data_diet_composition"]][[1]] |>
      dplyr::inner_join(threshold_map, by = "prey") |>
      dplyr::filter(is.na(threshold) | proportion >= threshold)

    if (nrow(filtered_predator) > 0) {
      sem_predator <- filtered_predator |>
        dplyr::mutate(
          driver = predator_snake_case,
          target = prey_snake_case,
          lag = 0,
          type = "top_down"
        ) |>
        dplyr::select(driver, target, lag, type) |>
        dplyr::distinct()
    }
  }

  # Finalize SEM Inputs
  # Combine all link types into a single specification tibble
  sem_tibble <- dplyr::bind_rows(sem_environment, sem_prey, sem_predator) |>
    dplyr::distinct(driver, target, lag, type) |>
    dplyr::mutate(param_name = paste0(driver, "_", target))
  # Create the formatted equation lines required by the dsem package
  sem_lines <- sem_tibble |>
    dplyr::mutate(equation = sprintf("%s -> %s, %d, %s", driver, target, lag, param_name)) |>
    dplyr::pull(equation) |>
    paste(collapse = "\n")

  # Subset the full time series to include only columns needed for the SEM
  sem_drivers <- sem_tibble[["driver"]]
  sem_columns <- unique(c("year", "month", sem_drivers, to_snake_case(focal_functional_groups)))
  sem_columns <- intersect(sem_columns, colnames(time_series_all))
  time_series_sem <- time_series_all[, sem_columns, drop = FALSE]

  sem <- tibble::tibble(
    data_time_series_all = list(time_series_all),
    data_time_series_sem = list(time_series_sem),
    sem_tibble = list(sem_tibble),
    sem_lines = sem_lines
  )
}

#' Create SEM Equations from a Tibble
#'
#' This helper function generates a character string of SEM equations formatted
#' for the \code{dsem} package from a tibble of SEM specifications.
#'
#' @param sem_tibble A tibble with columns `driver`, `target`, `lag`, and `param_name`.
#'
#' @return A character string with each SEM equation on a new line.
#' @export
#'
#' @examples
#' sem_tibble <- tibble::tibble(
#'   driver = "sst", target = "menhaden_0yr", lag = 12, param_name = "sst_menhaden_0yr"
#' )
#' create_sem_equations(sem_tibble)
create_sem_equations <- function(sem_tibble) {
  sem_tibble |>
    dplyr::mutate(equation = sprintf("%s -> %s, %d, %s", driver, target, lag, param_name)) |>
    dplyr::pull(equation) |>
    paste(collapse = "\n")
}
