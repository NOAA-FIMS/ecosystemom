#' Calculate "truth" for a species using {ecosystemdata} outputs
#'
#' @description The truth from an ecosystem model is often in different units
#' than what is needed to sample from. This function takes an {ecosystemdata}
#' data set and calculates additional true information such as converting
#' monthly age-composition information into annual data. No sampling is
#' performed just mathematical operations.
#'
#' @details The following values are calculated within this function:
#'   * biomass
#'   * catch
#'   * numbers-at-age by dividing biomass-at-age by weight-at-age information
#'   * weight
#'
#' @param data A tibble containing ecosystem model (e.g., Ecopath with Ecosim)
#' outputs from [ecosystemdata::load_model()].
#' @param species_name A string specifying the desired species you want to
#' summarize information for. This string must match an entry in
#' `data[["species"]]` and it currently cannot be a vector, just a single
#' string. See the examples section for code on how to efficiently get output
#' for multiple species.
#'
#' @return
#' A nested tibble where each row holds an output type (e.g., biomass, catch,
#' numbers, or weight) for a given time step category (e.g., monthly or yearly).
#' Thus, data within a row in returned output can be expanded to reveal the
#' full data, e.g., all months for an output type. The tibble has the
#' following columns:
#' \describe{
#'   \item{species_name}{A character string with the name of the species.}
#'   \item{truth_label}{A character string indicating the type of data,
#'     e.g., "biomass", "catch", "numbers", or "weight".}
#'   \item{truth_type}{A character string specifying if the data is an
#'     index ("index") or age-structured ("agecomp").}
#'   \item{truth_time_step}{A character string indicating the temporal
#'     resolution of the data, either "monthly" or "yearly".}
#'   \item{truth_om}{A list-column containing the nested data frames with
#'     the actual "truth" values. Each data frame can be unnested and includes:
#'     \describe{
#'       \item{year}{The year of the observation.}
#'       \item{month}{The month of the observation.}
#'       \item{unit}{The unit of the observation.}
#'       \item{value}{The numeric value of the observation.}
#'       \item{group}{The name of a group. The groups can contain age ranges,
#'         plus groups, or sub categories, e.g., juvenile. This is NA if
#'         truth_type is "index" instead of "agecomp".}
#'     }
#'   }
#' }
#'
#'
#' @examples
#' data(ewe_nwatlantic_base, package = "ecosystemdata")
#' data <- ewe_nwatlantic_base
#' truth <- calc_truth(
#'   data,
#'   species_name = "menhaden"
#' )
#'
#' # To unnest tibble and view the data
#' truth_long <- tidyr::unnest(truth, cols = c(truth_om))
#'
#' # To calculate truth for a vector of species using {purrr}
#' species_names <- c("menhaden", "striped bass", "bluefish")
#' truth <- purrr::map_dfr(
#'   species_names,
#'   function(x) calc_truth(data, species_name = x)
#' )
#' @export
#' @keywords calc_truth
calc_truth <- function(data, species_name) {
  # Check if the species_name is in the data
  unique_species <- unique(data[["species"]])
  if (!species_name %in% unique_species) {
    cli::cli_abort(c(
      "x" = "Species {.val {species_name}} not found in the {.par unique(data[['species']])}.",
      "i" = "Available species are: {.val {unique_species}}",
      "i" = "Please check the species name and try again."
    ))
  }

  # Create a tibble with the labels (e.g., biomass) and types (e.g., index)
  # Define available labels from data
  labels <- c("biomass", "catch", "weight")
  unique_data_type <- unique(data[["type"]])
  missing_data <- setdiff(labels, unique_data_type)
  if (length(missing_data) > 0) {
    cli::cli_abort(c(
      "x" = "{.val {missing_data}}-at-age data is missing for {species_name}.",
      "i" = "Available types in the data are: {.val {unique_data_type}}",
      "i" = "Please ensure that the data contains {missing_data} information."
    ))
  }

  # Create a tibble that maps labels to their types (index or age composition)
  types <- tidyr::expand(
    # Create a tibble with zero rows but the desired columns
    dplyr::tibble("label" = character(), "type" = character()),
    # Create all combinations of label and type
    label = labels,
    type = c("agecomp", "index")
  ) |>
    # Remove the weight/index combo b/c it doesn't exist
    dplyr::filter(!(label == "weight" & type == "index"))

  # Extract monthly data by labels
  truth_monthly <- purrr::map(
    labels,
    function(x) calc_truth_monthly(data, species_name, x)
  ) |>
    # Assign names to the list elements
    purrr::set_names(labels)

  # Extract labels for index type from types tibble
  index_labels <- types |>
    dplyr::filter(type == "index") |>
    dplyr::pull(label)

  # Calculate monthly index (e.g., biomass and catch)
  truth_index_monthly <- purrr::map(
    truth_monthly[index_labels],
    function(x) calc_truth_index_monthly(x)
  )

  # Aggregate monthly index into yearly averages (e.g., biomass and catch)
  truth_index_yearly <- purrr::map(
    truth_index_monthly,
    function(x) calc_truth_index_yearly(x)
  )

  # Extract labels for index type from types tibble
  agecomp_labels <- types |>
    dplyr::filter(type == "agecomp") |>
    dplyr::pull(label)

  # Calculate monthly age composition (e.g., biomass, catch, and weight)
  truth_agecomp_monthly <- purrr::map(
    truth_monthly[agecomp_labels],
    function(x) calc_truth_agecomp_monthly(x)
  )

  # Aggregate monthly age composition data into yearly averages (e.g., biomass,
  # catch, and weight)
  truth_agecomp_yearly <- purrr::map(
    truth_agecomp_monthly,
    function(x) calc_truth_agecomp_yearly(x)
  )

  # Extract biomass-at-age and weight-at-age
  truth_baa_monthly <- truth_agecomp_monthly[["biomass"]]
  truth_waa_monthly <- truth_agecomp_monthly[["weight"]]
  # Calculate numbers-at-age by dividing biomass-at-age by weight-at-age,
  # rounded to integers
  truth_agecomp_monthly[["numbers"]] <- truth_baa_monthly |>
    dplyr::mutate(
      type = "numbers",
      value = unlist(purrr::map2(
        truth_baa_monthly[["value"]],
        truth_waa_monthly[["value"]],
        function(x, y) round(x / y)
      )),
      unit = "numbers"
    )

  # Aggregate numbers-at-age into yearly averages, rounded to integers
  truth_agecomp_yearly[["numbers"]] <- calc_truth_agecomp_yearly(
    truth_agecomp_monthly[["numbers"]]
  ) |>
    # Yearly numbers are rounded as they are mean values from truth_agecomp_monthly
    dplyr::mutate(value = round(value))

  # Calculate index for numbers from monthly numbers-at-age
  truth_index_monthly[["numbers"]] <- calc_truth_index_monthly(
    truth_agecomp_monthly[["numbers"]]
  )
  # Aggregate to yearly numbers index, rounded to integers
  truth_index_yearly[["numbers"]] <- calc_truth_index_yearly(
    truth_index_monthly[["numbers"]]
  ) |>
    dplyr::mutate(value = round(value))

  truth_values <- c(
    truth_index_monthly,
    truth_index_yearly,
    truth_agecomp_monthly,
    truth_agecomp_yearly
  ) |>
    # Remove species and type columns to standardize structure
    purrr::map(
      function(x) dplyr::select(x, -species, -type)
    ) |>
    # Prefix all column names in each tibble in truth_values with "truth_".
    purrr::map(
      function(x) dplyr::rename_with(x, function(col) paste0("truth_", col))
    )

  # Create time step vector for all "truth" components
  time_step <- c(
    rep(c("monthly", "yearly"), each = length(truth_index_monthly)),
    rep(c("monthly", "yearly"), each = length(truth_agecomp_monthly))
  )

  # Create type vector (index or agecomp) for all "truth" components
  full_type <- c(
    rep("index", length(truth_index_monthly) + length(truth_index_yearly)),
    rep("agecomp", length(truth_agecomp_monthly) + length(truth_agecomp_yearly))
  ) |>
    purrr::set_names(names(truth_values))

  # Combine all "truth" data into a single tibble
  dplyr::tibble(
    species_name = rep(species_name, each = length(full_type)),
    truth_label = names(full_type),
    truth_type = full_type,
    truth_time_step = time_step,
    # Nest rows into a list-column of data frames
    truth_om = truth_values
  ) |>
    # Reorder the tibble by truth_label
    dplyr::arrange(truth_label)
}

#' Calculate monthly "truth" for a species
#'
#' @description This function calculates the monthly "truth" for a species by
#' filtering the data for the specified species and type
#'
#' @param data A tibble containing ecosystem model outputs from {ecosystemdata}.
#' @param species_name A string specifying the species name to subset.
#' @param truth_type A string specifying the type of "truth" to calculate.
#'
#' @return A tibble containing the monthly "truth" data for the specified species.
#'
#' @examples
#' data(ewe_nwatlantic_base, package = "ecosystemdata")
#' data <- ewe_nwatlantic_base
#' truth_monthly <- ecosystemom:::calc_truth_monthly(
#'   data,
#'   species_name = "menhaden",
#'   truth_type = "biomass"
#' )
#' @keywords calc_truth
calc_truth_monthly <- function(data, species_name, truth_type) {
  # Calculate the yearly "truth" using data from {ecosystemdata}
  data |>
    # Filter the data to include only the specified types
    dplyr::filter(type %in% truth_type) |>
    # Extract all rows that have species_name in the species column
    dplyr::filter(grepl(species_name, species))
}

#' Calculate "true" monthly index for a species
#'
#' @description This function calculates the monthly index for a species by
#' summing the values across all ages by month.
#'
#' @param truth_monthly A tibble containing the monthly "truth" data for a species.
#' Returned from [calc_truth_monthly()].
#'
#' @return A tibble containing the monthly index (e.g., biomass and catch) for
#' the specified species
#'
#' @examples
#' data(ewe_nwatlantic_base, package = "ecosystemdata")
#' data <- ewe_nwatlantic_base
#' truth_index_monthly <- ecosystemom:::calc_truth_monthly(
#'   data,
#'   species_name = "menhaden",
#'   truth_type = "biomass"
#' ) |>
#'   ecosystemom:::calc_truth_index_monthly()
#' @keywords calc_truth
calc_truth_index_monthly <- function(truth_monthly) {
  # Sum the data type (e.g., biomass) across all ages by month
  truth_monthly |>
    dplyr::group_by(species, type, year, month, unit) |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Calculate "true" yearly index for a species
#'
#' @description This function calculates the yearly index for a species by
#' averaging monthly index values.
#'
#' @param truth_index_monthly A tibble containing the monthly index data for a species.
#' Returned from [calc_truth_index_monthly()].
#'
#' @return A tibble containing the yearly index (e.g., biomass and catch) for
#' the specified species.
#'
#' @examples
#' data(ewe_nwatlantic_base, package = "ecosystemdata")
#' data <- ewe_nwatlantic_base
#' truth_index_yearly <- ecosystemom:::calc_truth_monthly(
#'   data,
#'   species_name = "menhaden",
#'   truth_type = "biomass"
#' ) |>
#'   ecosystemom:::calc_truth_index_monthly() |>
#'   ecosystemom:::calc_truth_index_yearly()
#' @keywords calc_truth
calc_truth_index_yearly <- function(truth_index_monthly) {
  # Calculate the yearly truth
  truth_index_monthly |>
    # Sum the data type (e.g., biomass) across all months by year
    dplyr::group_by(species, type, year, unit) |>
    dplyr::summarise(
      value = mean(value, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Calculate "true" monthly age composition for a species
#'
#' @description This function calculates the monthly age composition for a species
#' by selecting the relevant columns and filtering the data for the specified species.
#'
#' @param truth_monthly A tibble containing the monthly "truth" data for a species.
#' Returned from [calc_truth_monthly()].
#'
#' @return A tibble containing the monthly age composition (e.g., biomass, catch,
#' and weight) for the specified species.
#'
#' @examples
#' data(ewe_nwatlantic_base, package = "ecosystemdata")
#' data <- ewe_nwatlantic_base
#' truth_agecomp_monthly <- ecosystemom:::calc_truth_monthly(
#'   data,
#'   species_name = "menhaden",
#'   truth_type = "biomass"
#' ) |>
#'   ecosystemom:::calc_truth_agecomp_monthly()
#' @keywords calc_truth
calc_truth_agecomp_monthly <- function(truth_monthly) {
  truth_monthly |>
    dplyr::select(species, group, type, year, month, value, unit)
}

#' Calculate "true" yearly age composition for a species
#'
#' @description This function calculates the yearly age composition for a species
#' by averaging the monthly age composition values.
#'
#' @param truth_agecomp_monthly A tibble containing the monthly age composition
#' data for a species. Returned from [calc_truth_agecomp_monthly()].
#'
#' @return A tibble containing the yearly age composition (e.g., biomass, catch,
#' and weight) for the specified species.
#'
#' @examples
#' data(ewe_nwatlantic_base, package = "ecosystemdata")
#' data <- ewe_nwatlantic_base
#' truth_agecomp_yearly <- ecosystemom:::calc_truth_monthly(
#'   data,
#'   species_name = "menhaden",
#'   truth_type = "biomass"
#' ) |>
#'   ecosystemom:::calc_truth_agecomp_monthly() |>
#'   ecosystemom:::calc_truth_agecomp_yearly()
#' @keywords calc_truth
calc_truth_agecomp_yearly <- function(truth_agecomp_monthly) {
  truth_agecomp_monthly |>
    dplyr::group_by(species, group, type, year, unit) |>
    dplyr::summarise(
      value = mean(value, na.rm = TRUE),
      .groups = "drop"
    )
}
