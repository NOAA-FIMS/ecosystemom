utils::globalVariables(c(
  "timestep", "group", "fleet", "month", "type", "year", "value",
  "Year", "biomass_consumed", "predator", "prey", "proportion",
  "prey_snake_case", "predator_snake_case"
))

#' Align Ecospace Catch Data Headers
#'
#' A helper function to clean up column headers from Ecospace catch CSVs and
#' align them with standardized functional group names.
#'
#' @param ecospace_catch_output A data frame of Ecospace catch data.
#' @param functional_group_names A character vector of functional group names.
#' @param verbose Logical. If `TRUE`, prints informative messages.
#' @return A data frame with cleaned and ordered column names.
#' @noRd
align_ecospace_catch_data <- function(ecospace_catch_output, functional_group_names, verbose) {
  # Clean and match the CSV headers to the functional group names
  current_headers <- colnames(ecospace_catch_output)

  cleaned_headers <- sapply(current_headers, function(header_name) {
    # Keep the 'timestep' column as is
    if (header_name == "timestep") {
      return("timestep")
    }

    # Simplify the header by converting to lowercase, replacing non-alphanumeric
    # characters with underscores, and removing leading/trailing underscores.
    # e.g., "X.Blacktip.Shark.Blacktip.shark." becomes "blacktip_shark_blacktip_shark"
    # When reading CSVs, R's `read.csv` function (with the default
    # `check.names = TRUE`) can prefix headers with "X" if they are not
    # syntactically valid variable names (e.g., if they start with a
    # number). This can be the case with some EwE outputs. We remove this
    # potential prefix to standardize the header before matching against
    # functional group names.
    simplified_header <- gsub("^X", "", header_name)
    simplified_header <- gsub("[^[:alnum:]]+", "_", tolower(simplified_header))
    simplified_header <- gsub("^_+|_+$", "", simplified_header)

    # Simplify the clean group names list similarly for matching.
    # e.g., "Blacktip shark" becomes "blacktip_shark"
    simplified_groups <- gsub("[^[:alnum:]]+", "_", tolower(functional_group_names))
    simplified_groups <- gsub("^_+|_+$", "", simplified_groups)

    # Check each group name in the list against the current header
    # Use vapply to check every group name in your list one by one
    matches <- vapply(simplified_groups, function(group_key) {
      grepl(group_key, simplified_header, fixed = TRUE)
    }, logical(1))

    # Find the positions where a match was TRUE
    match_idx <- which(matches)

    if (length(match_idx) > 0) {
      # Return the pretty name from your list
      # If multiple matches occur, pick the longest one (more specific)
      best_match <- match_idx[which.max(nchar(functional_group_names[match_idx]))]
      return(functional_group_names[best_match])
    } else {
      return(header_name) # Return original if no match found
    }
  })

  # Update the column names in the data frame
  colnames(ecospace_catch_output) <- unname(cleaned_headers)

  # Identify any groups in your list that are missing from the data
  missing_groups <- setdiff(functional_group_names, colnames(ecospace_catch_output))

  # Add missing groups as new columns filled with 0.0
  if (length(missing_groups) > 0) {
    for (group in missing_groups) {
      ecospace_catch_output[[group]] <- 0.0
    }
    if (verbose) {
      cli::cli_alert_info("Added {length(missing_groups)} missing columns as zeros to catch output.")
    }
  }

  # Reorder columns to ensure 'timestep' is first,
  # followed by the groups in the exact order of functional_group_names
  final_column_order <- c("timestep", functional_group_names)
  ecospace_catch_output[, final_column_order]
}

#' Load in EwE monthly output data
#'
#' @description
#' This function loads and standardizes output data from Ecopath with Ecosim (EwE) models.
#'
#' @param file_path The path to the CSV file containing the EwE output data.
#' @param model_years A numeric vector of years corresponding to the model run.
#'   TODO: This could be derived automatically from an annual data file in the future.
#' @param functional_groups A vector of names of the functional groups in the model.
#' @param verbose Logical. If `TRUE` (default), prints informative messages.
#'
#' @export
#' @examples
#' \dontrun{
#' # The following example is not run by default because these files are only
#' # included in the GitHub clone of the repository and not in the package data
#' load_csv_ewe(
#'   file_path = fs::path(
#'     "data-raw", "ewe_nwatlantic", "base_run", "biomass_monthly.csv"
#'   ),
#'   model_years = 1985:2017,
#'   functional_groups = get_functional_groups(
#'     file_path = fs::path(
#'       base_run_dir, "basic_estimates.csv"
#'     )
#'   )
#' )
#' }
# TODO: double check that average of monthly data matches annual data for more than just biomass
load_csv_ewe <- function(file_path, model_years, functional_groups, verbose = TRUE) {
  # Load the EwE data file and extract the data
  data <- read_n_skip(file_path)
  functional_group_names <- functional_groups[["functional_group"]]

  if (NCOL(data) >= 1) {
    colnames(data)[1] <- "timestep"
  }

  is_numeric_group_header <-
    NCOL(data) > 1 && all(grepl("^(X)?[0-9]+$", colnames(data)[-1]))
  has_fleet_group_columns <- all(c("fleet", "group") %in% colnames(data))

  # Process data based on detected CSV structure
  if (
    NCOL(data) == NROW(functional_groups) + 1 &&
      is_numeric_group_header
  ) {
    # Case 1: Standard Ecosim format.
    # This format has a 'timestep' column and numeric headers for functional
    # groups.
    colnames(data) <- c("timestep", functional_group_names)
    # Read the data into a data frame and add year and month columns
    out <- data |>
      dplyr::mutate(
        year = rep(model_years, each = 12),
        month = rep(1:12, times = length(model_years))
      ) |>
      tidyr::pivot_longer(
        cols = tidyselect::all_of(functional_group_names),
        names_to = "functional_group",
        values_to = "value"
      ) |>
      dplyr::select(-timestep)
  } else if (!has_fleet_group_columns) {
    # Case 2: Ecospace catch format.
    # This format lacks 'fleet' and 'group' columns and has unstructured,
    # descriptive column headers that need alignment.
    out <- data |>
      align_ecospace_catch_data(functional_group_names, verbose = verbose) |>
      dplyr::select(timestep, tidyselect::all_of(functional_group_names)) |>
      dplyr::mutate(
        year = rep(model_years, each = 12),
        month = rep(1:12, times = length(model_years))
      ) |>
      tidyr::pivot_longer(
        cols = tidyselect::all_of(functional_group_names),
        names_to = "functional_group",
        values_to = "value"
      ) |>
      dplyr::select(-timestep)
  } else {
    # Case 3: Fleet-specific format.
    # This format includes 'fleet' and 'group' columns, used for outputs
    # like catch data broken down by fishing fleet.
    out <- data |>
      dplyr::rename(
        timestep = dplyr::starts_with("timestep")
      ) |>
      dplyr::mutate(
        reference = (timestep %/% 12) + 1,
        year = model_years[(timestep %/% 12) + 1],
        month = timestep %% 12,
        month = ifelse(month == 0, 12, month)
      ) |>
      dplyr::group_by(fleet, group) |>
      dplyr::mutate(
        year = rep(model_years, each = 12)
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        functional_group = functional_group_names[group]
      ) |>
      dplyr::select(-group, -timestep)
  }
  # Add 'type' column based on filename and join functional group details
  out |>
    dplyr::mutate(
      type = get_type_from_file(file_path)
    ) |>
    dplyr::left_join(
      functional_groups,
      by = "functional_group"
    ) |>
    dplyr::select(type, year, month, dplyr::everything())
}

#' Load an ecosystem model
#'
#' Load the necessary files from an ecosystem model and return a single, long
#' data frame of information.
#'
#' @param ... Arguments that are passed onto lower level `load_model_()*`
#'   functions. Such as those needed for `load_model_ewe_ecosim()`, which are
#'   `directory` and `functional_groups`.
#' @param type A string indicating which type of model data you want to load.
#'   The default is `r toString(formals(load_model)[["type"]][2])`. Strings
#'   should be all lower case text.
#' @param verbose Logical. If `TRUE` (default), prints informative messages.
#'
#' @export
#' @return
#' A tibble is returned that matches the structure of [ewe_ecosim_base_nwatlantic].
#'
load_model <- function(..., type = c("ewe_ecosim", "ewe_ecospace", "atlantis"), verbose = TRUE) {
  type <- tolower(type)
  type <- rlang::arg_match(type)
  model <- switch(type,
    "ewe_ecosim"   = load_model_ewe_ecosim(..., verbose = verbose),
    "ewe_ecospace" = load_model_ewe_ecospace(..., verbose = verbose),
    "atlantis"     = cli::cli_abort("{type} is not yet configured for {.fn load_model}")
  )
}

load_model_ewe_ecosim <- function(
  directory,
  functional_groups,
  verbose = TRUE,
  unit = c(
    "biomass" = "mt",
    "catch" = "mt",
    "landings" = "mt",
    "mortality" = "year^-1",
    "weight" = "mt"
  )
) {
  # Validate that the expected files exist before attemping to read them
  expected_files <- c("biomass_monthly.csv", "catch_monthly.csv", "weight_monthly.csv")
  missing <- setdiff(expected_files, list.files(directory))
  if (length(missing) > 0) {
    cli::cli_abort("Missing required files: {.val {missing}}")
  }
  # Determine the number of years in the model
  years <- read_n_skip(
    file_path = fs::path(directory, "biomass_annual.csv"),
    keyword = "year"
  ) |>
    dplyr::pull(1)

  # Load monthly data
  terms <- c("biomass", "catch", "landings", "mortality", "weight")
  monthly_files <- fs::dir_ls(
    path = directory,
    regexp = paste(.Platform[["file.sep"]], terms, "_monthly", sep = "", collapse = "|"),
    type = "file"
  )

  data_monthly <- purrr::map_df(
    monthly_files,
    load_csv_ewe,
    functional_groups = functional_groups,
    model_years = years,
    .id = "file_name",
    verbose = verbose
  )

  # TODO: build up this data set
  data_output <- data_monthly |>
    tibble::as_tibble() |>
    dplyr::mutate(
      unit = unit[type]
    )
}

load_model_ewe_ecospace <- function(
  directory,
  functional_groups,
  verbose = TRUE,
  unit = c(
    "biomass" = "mt_per_square_kilometer_per_year",
    "catch" = "mt_per_square_kilometer_per_year"
  )
) {

  # Determine the number of years in the model
  years <- read_n_skip(
    file_path = fs::path(directory, "Ecospace_Annual_Average_Biomass.csv"),
    keyword = "Year"
  ) |>
    dplyr::pull(1)

  # Load monthly data
  terms <- c("Biomass", "Catch")
  monthly_files <- fs::dir_ls(
    path = directory,
    regexp = paste(.Platform[["file.sep"]], "Ecospace_Average_", terms, sep = "", collapse = "|"),
    type = "file"
  )

  data_monthly <- purrr::map_df(
    monthly_files,
    load_csv_ewe,
    functional_groups = functional_groups,
    model_years = years,
    .id = "file_name",
    verbose = verbose
  )

  # TODO: build up this data set
  data_output <- data_monthly |>
    tibble::as_tibble() |>
    dplyr::mutate(
      unit = unit[type]
    )
}

get_type_from_file <- function(file_path) {
  base <- basename(file_path)
  type <- gsub("_monthly|_annual|Ecospace_Average_|\\.csv", "", base)
  type_lowercase <- tolower(type)
}


#' Load environmental data from a CSV file
#'
#' @param file_path A string. Path to the CSV file containing the environmental data.
#'   The CSV file must contain the following columns:
#'     - `index`: Name of the environmental index (character).
#'     - `year`: Year corresponding to each value (integer).
#'     - `month`: Month corresponding to each value (integer).
#'     - `value`: Value of the index (numeric).
#'     - `unit`: Unit of the index (character).
#' @param lag_months An integer. The lag between environmental index and functional group, in months.
#'   For example, if lag_months = 12, the environmental effect is assumed to influence the functional
#'   group with a 12-month delay.
#' @param impacted_group A string indicating the functional group impacted by the environmental index.
#'
#' @return A tibble containing the environmental data with lag and functional group information.
#'
#' @examples
#' data <- load_csv_environmental_data(
#'   file_path = file.path(
#'     system.file("extdata", package = "ecosystemom"),
#'     "ewe_ecosim_with_environmental_data_nwatlantic", "amo_lag1.csv"
#'   ),
#'   lag_months = 12,
#'   impacted_group = "menhaden 0"
#' )
#'
#' @export
load_csv_environmental_data <- function(file_path, lag_months, impacted_group) {
  # Read the CSV file
  data <- utils::read.csv(file_path)

  # Validate required columns
  required_columns <- c("index", "year", "month", "value", "unit")
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    cli::cli_abort(c(
      "The CSV file must contain the following columns: {.val {required_columns}}.",
      "x" = "Missing column(s): {.val {missing_columns}}."
    ))
  }

  # Split the impacted group into species and group components
  split_group <- split_functional_groups(impacted_group)

  # Reshape the data
  out <- tibble::as_tibble(data) |>
    dplyr::mutate(
      year = as.integer(year),
      month = as.integer(month),
      value = as.numeric(value),
      lag_months = ifelse(is.na(value), NA, as.integer(lag_months)),
      impacted_group = as.character(split_group[["functional_group"]]),
      impacted_group_snake_case = as.character(split_group[["functional_group_snake_case"]]),
      species = as.character(split_group[["species"]]),
      group = as.character(split_group[["group"]])
    )
  return(out)
}

#' Load diet composition data
#'
#' @description
#' This function loads diet composition data from a CSV file and standardizes it
#' into a tidy format. It automatically detects the format of the input file
#' based on its column names and processes it accordingly.
#'
#' @details
#' The function currently supports three formats from Ecopath with Ecosim (EwE):
#' \itemize{
#'   \item \strong{Ecospace Yearly}: Identified by a "Year" column. Data represents
#'     biomass consumed, which is converted to proportions.
#'   \item \strong{Ecospace Monthly}: Identified by a "TimeStep" column. This
#'     format is recognized but not yet supported.
#'   \item \strong{Ecopath}: Identified by a "Prey \\ predator" column. This is a
#'     static diet matrix. The function also handles empty column
#'     names that can sometimes appear in EwE outputs by ignoring them.
#' }
#' @param file_path The path to the CSV file containing the diet composition data.
#' @param verbose Logical. If `TRUE` (default), prints informative messages.
#'
#' @return A tibble in a long format with the following columns:
#' \itemize{
#'   \item `year`: The year of the observation (NA for static Ecopath data).
#'   \item `prey`: The name of the prey functional group.
#'   \item `predator`: The name of the predator functional group.
#'   \item `proportion`: The proportion of the prey in the predator's diet.
#'   \item `prey_snake_case`: The prey name converted to snake_case.
#'   \item `predator_snake_case`: The predator name converted to snake_case.
#' }
#'
#' @export
load_diet_composition <- function(file_path, verbose = TRUE) {

  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  
  # Read the raw data
  raw_data <- utils::read.csv(file_path, check.names = FALSE)

  # Determine the input file format based on unique column names
  format_type <- if ("Year" %in% colnames(raw_data)) {
    "ewe_ecospace_yearly"
  } else if ("TimeStep" %in% colnames(raw_data)) {
    "ewe_ecospace_monthly"
  } else if ("Prey \\ predator" %in% colnames(raw_data)) {
    "ewe_ecopath"
  } else {
     # This ensures format_type is length 1 and not NULL
    "unknown" 
  }

  # Process the data based on the detected format
  diet_composition_clean <- switch(format_type,
    "ewe_ecospace_yearly" = {
      if (verbose) cli::cli_alert_info("Processing Ecospace yearly format...")
      output <- raw_data |>
        # Pivot from wide to long format
        tidyr::pivot_longer(cols = -Year, names_to = "interaction", values_to = "biomass_consumed") |>
        # Split the combined "Predator | Prey" column
        tidyr::separate(interaction, into = c("predator", "prey"), sep = "\\s*\\|\\s*") |>
        # Group by year and predator to calculate diet proportions
        dplyr::group_by(Year, predator) |>
        dplyr::mutate(proportion = biomass_consumed / sum(biomass_consumed)) |>
        dplyr::rename(year = Year) |>
        dplyr::mutate(
          prey_snake_case = to_snake_case(prey),
          predator_snake_case = to_snake_case(predator)
        ) |>
        dplyr::select(year, prey, predator, proportion, prey_snake_case, predator_snake_case) |>
        dplyr::ungroup()
        
      if (verbose) cli::cli_alert_success("Converted consumption biomass to proportions!")
      output
    },

    "ewe_ecopath" = {
      if (verbose) cli::cli_alert_info("Processing Ecopath format...")
      # Handle empty column names that can occur in EwE output
      empty_names <- colnames(raw_data) == "" | is.na(colnames(raw_data))
      colnames(raw_data)[empty_names] <- paste0("temp_", which(empty_names))
      diet_composition <- raw_data |>
        # Remove columns where all values are NA
        dplyr::select(-dplyr::starts_with("temp_")) |>
        dplyr::rename(prey = 1) |>
        # Import represents the portion consumed by a functional group from outside
        # the defined ecosystem boundaries. Therefore, the sum of the proportions of
        # the existing functional groups will be less than or equal to 1.
        dplyr::filter(!prey %in% c("Import", "Sum", "(1 - Sum)"))

      # The predator names are in the first column, which will become row names for mapping
      predator_names <- diet_composition |> 
        dplyr::pull(prey)

      if (anyDuplicated(predator_names)) {
        duplicated_names <- unique(predator_names[duplicated(predator_names)])
        cli::cli_abort(c(
          "Duplicate predator names found in the first column of the diet composition file.",
          "x" = "Duplicate name(s): {.val {duplicated_names}}."
        ))
      }

      mapping <- stats::setNames(paste0(seq_along(predator_names)), predator_names)

      output <- diet_composition |>
        # Rename numeric-like column headers to actual predator names
        dplyr::rename(tidyselect::any_of(mapping)) |>
        # Pivot the diet matrix into a long format
        tidyr::pivot_longer(cols = -prey, names_to = "predator", values_to = "proportion") |>
        dplyr::mutate(
          prey_snake_case = to_snake_case(prey),
          predator_snake_case = to_snake_case(predator)
        ) |>
        tibble::add_column(year = NA, .before = 1)
      if (verbose) cli::cli_alert_success("Loaded EwE Ecopath Diet Composition format.")
      output
    },

    "ewe_ecospace_monthly" = {
      cli::cli_abort("The 'ewe_ecospace_monthly' format is recognized but not yet supported.")
    },

    # Default case for unknown formats
    cli::cli_abort("Unknown diet file format.")
  )
}
