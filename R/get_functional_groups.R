utils::globalVariables(c("V1", "V2"))

#' Get the functional groups from an EwE file
#'
#' @description
#' Functional group names are useful for the column names of output data from
#' an EwE model, and thus, this function is a way to get them automatically
#' from the EwE output. The functional group names come from the basic
#' estimates file.
#'
#' @param file_path The path to the EwE file.
#' @return
#' A tibble with the following columns:
#' \itemize{
#'   \item function_group. The full functional group name from the model. This
#'         column is helpful for error checking and debugging.
#'   \item species. The species name from the functional group name. This
#'         column should only contain text strings and no numbers or special
#'         characters. There will potentially be multiple rows with the same
#'         species name because there can be multiple age groups for a given
#'         species.
#'   \item group. The group name as a string. This column can contain digits,
#'         special characters, and text strings. It is used to
#'         delineate the group within a species. Not all species will have
#'         multiple groups.
#'   \item functional_group_snake_case. A standardized version of the functional
#'         group name. The functional_group is converted to lowercase and swaps 
#'         spaces or hyphens for underscores.
#' }
#' @export
#' @examples
#' get_functional_groups(
#'   file_path = fs::path(
#'     system.file("extdata", package = "ecosystemom"),
#'     "ewe_ecosim_with_environmental_data_nwatlantic", "basic_estimates.csv"
#'   )
#' )
get_functional_groups <- function(file_path) {
  # Load the EwE data file and extract the data
  temp <- scan(file_path, what = "", sep = "\n", quiet = TRUE)
  # Extract the data
  out_vector <- utils::read.table(
    text = as.character(temp[-1]),
    sep = ","
  ) |> 
    # Extract non-NA rows from the first column
    dplyr::filter(!is.na(V1)) |>
    # Pull the names of the functional groups
    dplyr::pull(V2)
  
  # Return a tibble with the functional groups, species, group names.
  split_functional_groups(out_vector)
}
