utils::globalVariables(c(
  "age_month", "age_month_column", "biomass", "catch", "exploitation", "file_name",
  "functional_group", "group", "group_header", "fleet", "month", "mortality",
  "natural_mortality", "number", "predator", "prey", "proportion", "region",
  "stanza", "step_in_year", "timestep", "type", "total_mortality", "value",
  "value_number", "value_weight", "weight", "Year", "year", "biomass_consumed",
  "fishing_mortality", "prey_snake_case", "predator_snake_case"
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
    "total_mortality" = "year^-1",
    "weight" = "mt"
  )
) {
  # Validate that the expected files exist before attempting to read them
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

  # Isolate catch (C), biomass (B), and total mortality, then convert C/B to the
  # instantaneous annual fishing mortality used in stock assessments.
  fishing_mortality <- data_monthly |>
    dplyr::filter(type %in% c("catch", "biomass", "mortality")) |>
    tidyr::pivot_wider(
      id_cols = c(year, month, functional_group, species, group, functional_group_snake_case),
      names_from = type,
      values_from = value
    ) |>
    dplyr::mutate(
      catch = dplyr::coalesce(catch, 0),
      biomass = dplyr::coalesce(biomass, 0),
      mortality = dplyr::coalesce(mortality, 0),
      value = dplyr::if_else(
        biomass > 0 & mortality > 0,
        (catch / biomass) * (mortality / (1 - exp(-mortality))),
        0
      ),
      file_name = NA_character_,
      type = "fishing_mortality"
    ) |>
    dplyr::select(-catch, -biomass, -mortality)

  # Extract total mortality from the original data.
  total_mortality <- data_monthly |>
    dplyr::filter(type == "mortality")

  # Get natural mortality; warn and set negative values to zero.
  natural_mortality <- dplyr::bind_rows(total_mortality, fishing_mortality) |>
    tidyr::pivot_wider(
      id_cols = c(year, month, functional_group, species, group, functional_group_snake_case),
      names_from = type,
      values_from = value
    ) |>
    dplyr::mutate(
      value = mortality - fishing_mortality,
      type = "natural_mortality",
      file_name = NA_character_
    ) |>
    dplyr::select(-mortality, -fishing_mortality)

  negative_natural_mortality <- natural_mortality |>
    dplyr::filter(value < 0) |>
    dplyr::distinct(year) |>
    dplyr::arrange(year)

  if (nrow(negative_natural_mortality) > 0 && verbose) {
    years_with_negative_m <- paste(negative_natural_mortality[["year"]], collapse = ", ")
    cli::cli_warn(
      c(
        "Detected negative natural mortality in year(s): {years_with_negative_m}.",
        "i" = "We used 0 for those negative natural mortality values for now.",
        "i" = "Please check the Ecosim fishing mortality input. Are the input values reasonable? Is catch greater than biomass?"
      )
    )
  }

  natural_mortality <- natural_mortality |>
    dplyr::mutate(value = pmax(value, 0))

  # TODO: build up this data set
  data_output <- data_monthly |>
    dplyr::bind_rows(fishing_mortality) |>
    dplyr::bind_rows(natural_mortality) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      # Update type "mortality" to "total mortality"
      type = dplyr::if_else(type == "mortality", "total_mortality", type),
      # Add "unit"
      unit = unit[type]
    ) |>
    dplyr::mutate(
      unit = dplyr::if_else(type == "fishing_mortality", "year^-1", unit)
    ) |>
    dplyr::mutate(
      unit = dplyr::if_else(type == "natural_mortality", "year^-1", unit)
    )
}

#' Load Ecospace model output
#'
#' Read monthly biomass and catch together with annual age-structure weights
#' and derive age-structured mortality. Ecospace age-structure files are
#' matched by their `All_Weight` and `All_Number` suffixes; the Number data
#' contributes to the number-weighted average weight, while cohort abundance
#' for mortality is derived from annual biomass divided by that weight.
#'
#' @param directory Path to an Ecospace output directory.
#' @param functional_groups A tibble mapping functional-group names to species
#'   and group labels, as returned by [split_functional_groups()].
#' @param verbose If `TRUE`, report warnings such as negative derived natural
#'   mortality.
#' @param unit Named character vector giving units for biomass, catch, and
#'   weight output.
#'
#' @return A tibble in the standard `load_model()` output structure.
#' @noRd
load_model_ewe_ecospace <- function(
  directory,
  functional_groups,
  verbose = TRUE,
  unit = c(
    "biomass" = "mt_per_square_kilometer_per_year",
    "catch" = "mt_per_square_kilometer_per_year",
    "weight" = "relative_to_Winf"
  )
) {
  # Read annual files for the model year sequence and annual biomass/catch
  # values; these are the biomass and catch sources used in the standard output.
  annual_biomass_file <- fs::path(directory, "Ecospace_Annual_Average_Biomass.csv")
  annual_catch_file <- fs::path(directory, "Ecospace_Annual_Average_Catch.csv")
  years <- read_n_skip(annual_biomass_file, keyword = "Year") |>
    dplyr::pull(1)

  # Load monthly biomass and catch. These files are group-level outputs; the
  # monthly age-structure files are processed separately below.
  terms <- c("Biomass", "Catch")
  monthly_files <- fs::dir_ls(
    path = directory,
    regexp = paste0(
      "Ecospace_Average_(", paste(terms, collapse = "|"), ")\\.csv$"
    ),
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

  # Each age-structure weight file must have a matching Number file. The
  # Number file is retained for the number-weighted weight average, not used as
  # cohort abundance in the mortality calculation.
  weight_files <- fs::dir_ls(
    path = directory,
    recurse = TRUE,
    regexp = "_All_Weight\\.csv$",
    type = "file"
  )
  number_files <- fs::dir_ls(
    path = directory,
    recurse = TRUE,
    regexp = "_All_Number\\.csv$",
    type = "file"
  )

  if (length(setdiff(
    sub("_All_Weight\\.csv$", "_All_Number.csv", weight_files),
    number_files
  )) > 0) {
    missing_number_files <- setdiff(
      sub("_All_Weight\\.csv$", "_All_Number.csv", weight_files),
      number_files
    )
    cli::cli_abort(
      "Missing matching Ecospace number file{?s}: {.val {missing_number_files}}"
    )
  }

  if (length(setdiff(
    sub("_All_Number\\.csv$", "_All_Weight.csv", number_files),
    weight_files
  )) > 0) {
    missing_weight_files <- setdiff(
      sub("_All_Number\\.csv$", "_All_Weight.csv", number_files),
      weight_files
    )
    cli::cli_abort(
      "Missing matching Ecospace weight file{?s}: {.val {missing_weight_files}}"
    )
  }

  # Pair files by replacing the terminal suffix while preserving their
  # directory and model-specific filename.
  number_files_for_weight <- sub(
    "_All_Weight\\.csv$",
    "_All_Number.csv",
    weight_files
  )
  age_structure_pairs <- purrr::map2(
    weight_files,
    number_files_for_weight,
    function(weight_file, number_file) {
      weight_structure <- read_ecospace_age_structure(weight_file)
      number_structure <- read_ecospace_age_structure(number_file)
      if (weight_structure[["steps_per_year"]] !=
          number_structure[["steps_per_year"]]) {
        cli::cli_abort(
          "Ecospace age-structure file pair has inconsistent time-step lengths: {.file {weight_file}} and {.file {number_file}}."
        )
      }

      # Timestep 1 is the first monthly snapshot. Keep the first snapshot of
      # each model year, and discard the special base-values row.
      weight_data <- weight_structure[["data"]] |>
        dplyr::mutate(
          year = ((timestep - 1) %/% weight_structure[["steps_per_year"]]) + 1,
          step_in_year = (timestep - 1) %% weight_structure[["steps_per_year"]]
        ) |>
        dplyr::filter(step_in_year == 0, region != "Ecosim Base Values") |>
        dplyr::mutate(
          functional_group = match_ecospace_group(
            stanza,
            functional_groups[["functional_group"]]
          )
        )
      number_data <- number_structure[["data"]] |>
        dplyr::mutate(
          year = ((timestep - 1) %/% number_structure[["steps_per_year"]]) + 1,
          step_in_year = (timestep - 1) %% number_structure[["steps_per_year"]]
        ) |>
        dplyr::filter(step_in_year == 0, region != "Ecosim Base Values") |>
        dplyr::mutate(
          functional_group = match_ecospace_group(
            stanza,
            functional_groups[["functional_group"]]
          )
        )

      # Require an exact row match before combining values, so weight and
      # Number observations cannot silently become misaligned.
      join_columns <- c(
        "timestep", "region", "stanza", "age_month", "year",
        "step_in_year", "functional_group"
      )
      if (nrow(dplyr::anti_join(weight_data, number_data, by = join_columns)) > 0 ||
          nrow(dplyr::anti_join(number_data, weight_data, by = join_columns)) > 0) {
        cli::cli_abort(
          "Ecospace age-structure file pair has non-matching rows: {.file {weight_file}} and {.file {number_file}}."
        )
      }

      # Keep raw Number only long enough to calculate an abundance-weighted
      # mean weight for each stanza and year.
      paired_data <- weight_data |>
        dplyr::inner_join(
          number_data,
          by = join_columns,
          suffix = c("_weight", "_number")
        ) |>
        dplyr::transmute(
          timestep,
          region,
          stanza,
          age_month,
          year,
          functional_group,
          file_name = weight_file,
          weight = value_weight,
          number = value_number
        )
      functional_groups_by_age_month <- weight_data |>
        dplyr::distinct(age_month, functional_group)

      list(
        data = paired_data,
        functional_groups_by_age_month = functional_groups_by_age_month,
        steps_per_year = weight_structure[["steps_per_year"]],
        file_name = weight_file
      )
    }
  )

  if (length(age_structure_pairs) > 0) {
    if (length(unique(purrr::map_dbl(age_structure_pairs, "steps_per_year"))) != 1) {
      cli::cli_abort("Ecospace age-structure files have inconsistent time-step lengths.")
    }

    monthly_age_data <- purrr::map(age_structure_pairs, "data") |>
      dplyr::bind_rows()
    # Collapse monthly age cells into one number-weighted average weight for
    # each functional group and year. These annual weights also support
    # biomass-to-number conversion in the mortality calculation below.
    weight_data <- monthly_age_data |>
      dplyr::group_by(file_name, year, functional_group) |>
      dplyr::summarise(
        value = dplyr::if_else(
          sum(number, na.rm = TRUE) > 0,
          sum(weight * number, na.rm = TRUE) / sum(number, na.rm = TRUE),
          NA_real_
        ),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        month = 1L,
        type = "weight"
      ) |>
      dplyr::left_join(
        functional_groups,
        by = "functional_group"
      )

    steps_per_year <- age_structure_pairs[[1]][["steps_per_year"]]
    annual_biomass <- read_ecospace_annual_data(
      annual_biomass_file,
      functional_groups,
      value_name = "biomass"
    )
    annual_catch <- read_ecospace_annual_data(
      annual_catch_file,
      functional_groups,
      value_name = "catch"
    )
    mortality <- purrr::map(
      age_structure_pairs,
      function(age_structure_pair) {
        # Preserve the age-column-to-functional-group mapping from the input.
        functional_groups_by_age_month <-
          age_structure_pair[["functional_groups_by_age_month"]] |>
          dplyr::group_by(functional_group) |>
          dplyr::mutate(monthly_age_cells_per_group = dplyr::n()) |>
          dplyr::ungroup()
        group_weights <- weight_data |>
          dplyr::filter(file_name == age_structure_pair[["file_name"]]) |>
          dplyr::select(year, functional_group, weight = value)
        # The annual biomass and weight outputs only identify total abundance
        # per functional group. Spread that total evenly across its monthly-age
        # columns so the cohort calculation can sum corresponding age ranges.
        numbers_by_age_month <- tidyr::crossing(
          year = sort(unique(annual_biomass[["year"]])),
          functional_groups_by_age_month
        ) |>
          dplyr::left_join(
            annual_biomass,
            by = c("year", "functional_group")
          ) |>
          dplyr::left_join(
            group_weights,
            by = c("year", "functional_group")
          ) |>
          dplyr::mutate(
            number = dplyr::if_else(
              weight > 0,
              biomass / weight,
              NA_real_
            )
          ) |>
          dplyr::transmute(
            year,
            age_month,
            functional_group,
            number = number / monthly_age_cells_per_group
          )

        calculate_ecospace_mortality(
          numbers_by_age_month = numbers_by_age_month,
          annual_biomass = annual_biomass,
          annual_catch = annual_catch,
          steps_per_year = steps_per_year
        ) |>
          dplyr::mutate(file_name = age_structure_pair[["file_name"]])
      }
    ) |>
      dplyr::bind_rows() |>
      dplyr::left_join(
        functional_groups,
        by = "functional_group"
      ) |>
      tidyr::pivot_longer(
        cols = c(total_mortality, fishing_mortality, natural_mortality),
        names_to = "type",
        values_to = "value"
      ) |>
      dplyr::mutate(
        month = 1L
      )

    negative_natural_mortality <- mortality |>
      dplyr::filter(type == "natural_mortality", value < 0) |>
      dplyr::distinct(year) |>
      dplyr::arrange(year)
    if (nrow(negative_natural_mortality) > 0 && verbose) {
      years_with_negative_m <- paste(negative_natural_mortality[["year"]], collapse = ", ")
      cli::cli_warn(
        "Detected negative Ecospace natural mortality in year(s): {years_with_negative_m}."
      )
    }
  } else {
    weight_data <- tibble::tibble()
    mortality <- tibble::tibble()
  }

  data_output <- data_monthly |>
    dplyr::bind_rows(weight_data, mortality) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      unit = unit[type]
    ) |>
    dplyr::mutate(
      unit = dplyr::if_else(
        type %in% c("fishing_mortality", "natural_mortality", "total_mortality"),
        "year^-1",
        unit
      )
    )
}

#' Read an Ecospace monthly-age CSV file
#'
#' Parse the metadata and table in an `All_Weight` or `All_Number` output file.
#' The returned table has one row per timestep, region, and monthly-age column.
#'
#' @param file_path Path to an Ecospace age-structure CSV file.
#'
#' @return A list with `data` (a tibble containing timestep, region, stanza,
#'   monthly age index, and value) and `steps_per_year` (the number of Ecospace
#'   timesteps in one model year).
#' @noRd
read_ecospace_age_structure <- function(file_path) {
  # The CSV begins with metadata; locate its table header instead of assuming
  # a fixed number of metadata lines.
  lines <- readLines(file_path, warn = FALSE)
  timestep_header <- grep("^Timestep", trimws(lines))[1]
  if (is.na(timestep_header)) {
    cli::cli_abort("No Timestep header found in {.file {file_path}}.")
  }

  timestep_length_line <- grep("^EcoSpaceTimeStepLength,", lines, value = TRUE)
  if (length(timestep_length_line) != 1) {
    cli::cli_abort(
      "Expected one EcoSpaceTimeStepLength value in {.file {file_path}}."
    )
  }
  # Ecospace expresses timestep length as a fraction of a year. Mortality
  # calculations require an integer number of timesteps per year.
  timestep_length <- as.numeric(
    sub("^EcoSpaceTimeStepLength,", "", timestep_length_line)
  )
  steps_per_year <- round(1 / timestep_length)
  if (!is.finite(timestep_length) || timestep_length <= 0 ||
      !is.finite(steps_per_year) || steps_per_year < 1 ||
      abs((1 / timestep_length) - steps_per_year) > 1e-8) {
    cli::cli_abort(
      "Invalid annual time-step length in {.file {file_path}}."
    )
  }

  # Read from the Timestep row onward, preserving the age-column names because
  # each column encodes both stanza and monthly age.
  data <- utils::read.csv(
    text = lines[timestep_header:length(lines)],
    check.names = FALSE,
    stringsAsFactors = FALSE,
    strip.white = TRUE
  )
  data <- data[, nzchar(names(data)), drop = FALSE]
  names(data)[1:2] <- c("timestep", "region")

  # Convert wide age columns to long form so each observation has an explicit
  # stanza and monthly-age index.
  data <- data |>
    dplyr::mutate(
      region = as.character(region)
    ) |>
    tidyr::pivot_longer(
      cols = -c(timestep, region),
      names_to = "age_month_column",
      values_to = "value"
    ) |>
    dplyr::mutate(
      stanza = sub("_[0-9]+$", "", age_month_column),
      age_month = as.integer(sub(".*_", "", age_month_column)),
      value = as.numeric(value)
    ) |>
    dplyr::select(timestep, region, stanza, age_month, value)

  list(data = data, steps_per_year = steps_per_year)
}

#' Match Ecospace stanza names to model functional groups
#'
#' Match file-provided stanza names to the functional-group labels supplied to
#' `load_model()`. Punctuation and whitespace are ignored for matching, but
#' original functional-group labels are returned.
#'
#' @param group_names Character vector of stanza or group names from Ecospace.
#' @param functional_group_names Character vector of package functional-group
#'   names.
#'
#' @return A character vector aligned with `group_names`.
#' @noRd
match_ecospace_group <- function(group_names, functional_group_names) {
  # Ignore punctuation and case differences in source labels, while retaining
  # the package's canonical label in the returned vector.
  normalize_group <- function(x) {
    gsub("[^[:alnum:]]", "", tolower(trimws(x)))
  }

  group_keys <- normalize_group(group_names)
  functional_group_keys <- normalize_group(functional_group_names)
  matched_groups <- functional_group_names[match(group_keys, functional_group_keys)]
  unmatched_groups <- unique(group_names[is.na(matched_groups)])
  if (length(unmatched_groups) > 0) {
    cli::cli_abort(
      "Could not match Ecospace age group{?s} {.val {unmatched_groups}} to {.arg functional_groups}."
    )
  }
  matched_groups
}

#' Read annual Ecospace biomass or catch
#'
#' Convert an annual Ecospace table from one column per group (or fleet-group
#' combination) to one row per year and functional group.
#'
#' @param file_path Path to an annual biomass or catch CSV file.
#' @param functional_groups A tibble mapping functional-group names to species
#'   and group labels.
#' @param value_name Name to use for the resulting value column; typically
#'   `"biomass"` or `"catch"`.
#'
#' @return A tibble with year, functional group, and the requested value column.
#' @noRd
read_ecospace_annual_data <- function(file_path, functional_groups, value_name) {
  data <- utils::read.csv(
    file_path,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  # Catch headers include a fleet prefix; remove it before mapping to the
  # functional-group table. Sum across fleets when a group has multiple rows.
  data |>
    tidyr::pivot_longer(
      cols = -Year,
      names_to = "group_header",
      values_to = value_name
    ) |>
    dplyr::mutate(
      group_header = if (value_name == "catch") {
        sub("^.*\\|", "", group_header)
      } else {
        group_header
      },
      functional_group = match_ecospace_group(
        group_header,
        functional_groups[["functional_group"]]
      )
    ) |>
    dplyr::group_by(year = Year, functional_group) |>
    dplyr::summarise(
      "{value_name}" := sum(.data[[value_name]], na.rm = TRUE),
      .groups = "drop"
    )
}

#' Calculate age-structured Ecospace mortality
#'
#' Estimate annual cohort total mortality from numbers at consecutive ages in
#' consecutive years. Convert annual catch-to-biomass exploitation to
#' instantaneous fishing mortality using the package's mortality adjustment,
#' then calculate natural mortality as total mortality minus fishing mortality.
#'
#' @param numbers_by_age_month Tibble with year, monthly age index, functional
#'   group, and biomass/weight-derived number values.
#' @param annual_biomass Tibble with annual biomass by year and functional group.
#' @param annual_catch Tibble with annual catch by year and functional group.
#' @param steps_per_year Number of monthly-age columns in one annual age group.
#'
#' @return A tibble with year, functional group, total mortality, fishing
#'   mortality, and natural mortality.
#' @noRd
calculate_ecospace_mortality <- function(
  numbers_by_age_month,
  annual_biomass,
  annual_catch,
  steps_per_year
) {
  # Catch divided by biomass is the annual exploitation rate. Missing catch
  # contributes zero exploitation; groups without positive biomass also get 0.
  annual_exploitation <- annual_biomass |>
    dplyr::left_join(
      annual_catch,
      by = c("year", "functional_group")
    ) |>
    dplyr::mutate(
      exploitation = dplyr::if_else(
        biomass > 0,
        dplyr::coalesce(catch, 0) / biomass,
        0
      )
    ) |>
    dplyr::select(year, functional_group, exploitation)

  years <- sort(unique(numbers_by_age_month[["year"]]))
  if (length(years) < 2) {
    return(tibble::tibble(
      year = integer(),
      functional_group = character(),
      total_mortality = double(),
      fishing_mortality = double(),
      natural_mortality = double()
    ))
  }

  # Establish the monthly-age sequence and its annual-age boundaries. A plus
  # group is inferred from the final monthly-age column when it spans at least
  # one full annual age interval.
  max_age_month_index <- max(numbers_by_age_month[["age_month"]], na.rm = TRUE)
  functional_groups_by_age_month <- numbers_by_age_month |>
    dplyr::distinct(age_month, functional_group) |>
    dplyr::arrange(age_month)
  if (anyDuplicated(functional_groups_by_age_month[["age_month"]])) {
    cli::cli_abort(
      "Each Ecospace monthly-age column must map to exactly one functional group."
    )
  }

  annual_age_groups <- functional_groups_by_age_month |>
    dplyr::filter(age_month %% steps_per_year == 0) |>
    dplyr::mutate(age_year = age_month %/% steps_per_year)
  if (nrow(annual_age_groups) == 0) {
    cli::cli_abort("No year-start ages found in Ecospace age-structure data.")
  }

  plus_age_group <- functional_groups_by_age_month |>
    dplyr::filter(age_month == max_age_month_index) |>
    dplyr::slice(1) |>
    dplyr::pull(functional_group)
  plus_age_year <- annual_age_groups |>
    dplyr::filter(functional_group == plus_age_group) |>
    dplyr::summarise(plus_age_year = min(age_year)) |>
    dplyr::pull(plus_age_year)
  if (length(plus_age_year) == 0 || is.na(plus_age_year) ||
      max_age_month_index < (plus_age_year + 1) * steps_per_year) {
    plus_age_year <- Inf
  }

  functional_group_by_age_month <- functional_groups_by_age_month[["functional_group"]]
  names(functional_group_by_age_month) <-
    as.character(functional_groups_by_age_month[["age_month"]])
  # Store the supplied abundance in a year-by-monthly-age matrix. This makes
  # each cohort's current and next-year age ranges explicit and efficient to
  # retrieve inside the mortality loop.
  numbers_by_year_and_age_month <- matrix(
    0,
    nrow = length(years),
    ncol = max_age_month_index + 1,
    dimnames = list(as.character(years), as.character(0:max_age_month_index))
  )
  numbers_by_year_and_age_month[cbind(
    match(numbers_by_age_month[["year"]], years),
    numbers_by_age_month[["age_month"]] + 1
  )] <- numbers_by_age_month[["number"]]

  mortality <- list()
  for (year in years[years < max(years)]) {
    if (!((year + 1) %in% years)) {
      next
    }

    for (age_year in sort(unique(annual_age_groups[["age_year"]]))) {
      if (age_year < plus_age_year) {
        current_age_months <- (age_year * steps_per_year) +
          seq.int(0, steps_per_year - 1)
        next_age_months <- current_age_months + steps_per_year
        if (max(next_age_months) > max_age_month_index) {
          next
        }
      } else if (age_year == plus_age_year) {
        current_age_months <- seq.int(
          age_year * steps_per_year,
          max_age_month_index
        )
        next_age_months <- seq.int(
          (age_year + 1) * steps_per_year,
          max_age_month_index
        )
      } else {
        next
      }

      # Sum abundance over the cohort's current age range, then its advanced
      # age range in the following year. This follows the cohort diagonally
      # through the year-by-age matrix.
      current_cohort_numbers <-
        numbers_by_year_and_age_month[as.character(year), current_age_months + 1]
      initial_cohort_number <- sum(current_cohort_numbers, na.rm = TRUE)
      next_cohort_number <- sum(
        numbers_by_year_and_age_month[
          as.character(year + 1),
          next_age_months + 1
        ],
        na.rm = TRUE
      )
      if (initial_cohort_number <= 0 ||
          next_cohort_number <= 0) {
        next
      }
      # Z is the log ratio of initial cohort abundance to abundance one year
      # later, as specified for the Ecospace age-structured output.
      total_mortality <- -log(next_cohort_number / initial_cohort_number)

      exploitation_for_year <- annual_exploitation[
        annual_exploitation[["year"]] == year,
        ,
        drop = FALSE
      ]
      exploitation_by_group <- stats::setNames(
        exploitation_for_year[["exploitation"]],
        exploitation_for_year[["functional_group"]]
      )
      # Apply the exploitation rate for the functional group occupied by each
      # age-month cell during the year. Cap the final cell at the last available
      # age because Ecospace may report a plus group.
      fishing_year_age_months <- pmin(
        outer(current_age_months, seq.int(0, steps_per_year - 1), "+"),
        max_age_month_index
      )
      fishing_year_functional_groups <-
        functional_group_by_age_month[as.character(fishing_year_age_months)]
      functional_group_exploitation <- matrix(
        exploitation_by_group[as.vector(fishing_year_functional_groups)],
        nrow = length(current_age_months),
        ncol = steps_per_year
      )
      functional_group_exploitation[is.na(functional_group_exploitation)] <- 0
      cohort_exploitation <- stats::weighted.mean(
        rowMeans(functional_group_exploitation),
        current_cohort_numbers
      )
      # Convert exploitation to instantaneous F using the same Z adjustment
      # used elsewhere in the package; retain the established positive-Z guard.
      fishing_mortality <- if (is.finite(total_mortality) && total_mortality > 0) {
        cohort_exploitation *
          (total_mortality / (1 - exp(-total_mortality)))
      } else {
        0
      }

      mortality[[length(mortality) + 1]] <- tibble::tibble(
        year = year,
        functional_group =
          functional_group_by_age_month[[as.character(min(current_age_months))]],
        initial_cohort_number = initial_cohort_number,
        total_mortality = total_mortality,
        fishing_mortality = fishing_mortality,
        natural_mortality = total_mortality - fishing_mortality
      )
    }
  }

  if (length(mortality) == 0) {
    return(tibble::tibble(
      year = integer(),
      functional_group = character(),
      total_mortality = double(),
      fishing_mortality = double(),
      natural_mortality = double()
    ))
  }

  # Multiple cohorts can contribute to one annual functional group. Combine
  # their rates by initial cohort abundance, then recompute M = Z - F.
  dplyr::bind_rows(mortality) |>
    dplyr::group_by(year, functional_group) |>
    dplyr::summarise(
      dplyr::across(
        c(total_mortality, fishing_mortality),
        ~ stats::weighted.mean(.x, initial_cohort_number, na.rm = TRUE)
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      natural_mortality = total_mortality - fishing_mortality
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
