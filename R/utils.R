#' Find the number of rows to skip when reading in an EWE file
#' @noRd
read_n_skip <- function(file_path, keyword = "timestep") {
  # Load the data file
  temp <- scan(file_path, what = "", sep = "\n", quiet = TRUE)
  # For example with the default
  # Find the line containing "timestep\\group" to skip
  skip_n_rows <- which(startsWith(tolower(temp), tolower(keyword)))
  column_names <- temp[skip_n_rows] |>
    strsplit(",")
  # Extract the data
  data <- temp[-c(1:skip_n_rows)]
  utils::read.table(
    text = as.character(data),
    sep = ",",
    col.names = column_names[[1]]
  )
}

#' Validate input ecosystem data and resolve age structure mappings
#'
#' @param data A tibble of ecosystem model output.
#' @param ages A named numeric vector or NULL.
#'
#' @return A list containing `functional_groups` (character vector) and
#'   `sorted_age_mapping` (named numeric vector).
#' @noRd
validate_data_and_ages <- function(data, ages) {
  # Base Structure Assertions
  if (!tibble::is_tibble(data)) {
    cli::cli_abort("{.arg data} must be a tibble.")
  }

  if (nrow(data) == 0) {
    cli::cli_abort("{.arg data} cannot be empty.")
  }

  required_columns <- c(
    "species_name", "truth_label", "truth_type", "truth_time_step",
    "truth_group", "truth_year", "truth_unit", "truth_value"
  )
  missing_columns <- setdiff(required_columns, colnames(data))
  if (length(missing_columns) > 0) {
    cli::cli_abort(
      "{.arg data} is missing required column{?s}: {.val {missing_columns}}"
    )
  }

  # Species Multiplicity Warning
  unique_species <- unique(data[["species_name"]])
  if (length(unique_species) > 1) {
    cli::cli_abort(
      c(
        "{.arg data} contains multiple species: {.val {unique_species}}.",
        "i" = "Please filter data to a single species before calling this function."
      )
    )
  }

  # Functional Groups Validation
  functional_groups <- unique(as.character(data[["truth_group"]]))
  if (length(functional_groups) < 2) {
    cli::cli_abort(
      "{.arg data} must contain at least 2 unique groups in {.var truth_group}."
    )
  }

  # Resolve / Map Ages Vector
  if (is.null(ages)) {
    numeric_conversion <- suppressWarnings(
      as.numeric(functional_groups)
    )

    if (!any(is.na(numeric_conversion))) {
      ages <- numeric_conversion
      names(ages) <- functional_groups
    } else {
      ages <- seq_along(functional_groups)
      names(ages) <- functional_groups
      cli::cli_inform(
        c(
          "!" = "{.arg ages} mapping was not provided and {.var truth_group} labels are non-numeric.",
          "i" = "Defaulting to sequential age indexing: {.val {ages}}.",
          "*" = "Good practice: Explicitly pass a named vector linking `group` to true structural ages."
        )
      )
    }
  } else {
    if (!is.numeric(ages) || is.null(names(ages))) {
      cli::cli_abort(
        "{.arg ages} must be a **named** numeric vector mapping {.var truth_group} values to numeric ages."
      )
    }

    missing_mappings <- setdiff(functional_groups, names(ages))
    if (length(missing_mappings) > 0) {
      cli::cli_abort(
        c(
          "The provided {.arg ages} mapping is incomplete.",
          "x" = "Missing explicit age assignments for the following groups found in your data: {.val {missing_mappings}}"
        )
      )
    }
  }

  return(ages)
}
