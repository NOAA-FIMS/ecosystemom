utils::globalVariables(c("V1", "V2"))

#' Get the functional groups from an EwE file
#'
#' @description
#' Functional group names are useful for the column names of output data from
#' an EwE model, and thus, this function is a way to get them automatically
#' from the EwE output. The functional group names come from the basic
#' estimates file.
#'
#' @details
#' This function is for convenience only, it is not required by the package.
#' Using it to generate the functional group names from a `basic_estimates.csv`
#' file lessens the burden on you but we realize that the function will not
#' work for every ecosystem model. It does work for many common naming
#' conventions, including species names followed by numeric age groups, plus
#' groups, ranges, and a small number of text suffixes such as `"juv"` and
#' `"adult"`. However, functional group names in real EwE models are not always
#' consistent. If your model uses names that cannot be parsed correctly, you
#' can create the functional group tibble yourself and pass that tibble to
#' functions such as [load_csv_ewe()].
#'
#' A custom functional group tibble must contain one row for each functional
#' group in the model and the same four columns returned by this function. See
#' the return section for more details on the columns. To create the last
#' column, `functional_group_snake_case`, consider calling
#' [split_functional_groups()] on the original names and then manually edit
#' `species` and `group` where needed.
#'
#' The custom tibble should be checked before it is used in model-loading
#' functions. In particular, confirm that `functional_group` has no missing
#' values, that every functional group name is unique, that
#' `functional_group_snake_case` is unique, and that the number of rows equals
#' the number of living and non-living groups in the EwE output files you are
#' about to load. If those columns are misaligned, downstream data can be read
#' into the wrong functional group. When in doubt, start with the output of
#' `get_functional_groups()` or [split_functional_groups()], inspect the result,
#' and then replace only the rows that were parsed incorrectly.
#'
#' @param file_path The path to the EwE file.
#' @return
#' A tibble with the following columns:
#' \itemize{
#'   \item `functional_group`: The exact functional group name used by EwE.
#'         These values should match the names and order expected by the EwE
#'         output files that you will load later. Keep this column exact to
#'         the original EwE text as possible because it is the audit trail back
#'         to the model.
#'   \item `species`: The biological or conceptual species/group name after
#'         removing any age, stage, size, or other within-species suffix. For
#'         example, if EwE has `"King mackerel (0-1yr)"` and
#'         `"King mackerel (1+yr)"`, both rows should usually have
#'         `"King mackerel"` in `species`.
#'   \item `group`: the age, stage, size, or other suffix within `species`.
#'         Use a character value such as `"0-1yr"`, `"1+yr"`, `"juvenile"`, or
#'         `"adult"` when a functional group is one of several groups for the
#'         same species. Use `NA_character_` when the functional group does not
#'         have a within-species suffix. It is used to delineate the group
#'         within a species. Not all species will have multiple groups.
#'   \item `functional_group_snake_case`: A unique, syntactically convenient
#'         name that can be used as a column name in downstream output. These
#'         names should be lowercase, should not contain spaces or parentheses,
#'         and should be unique across rows.
#' }
#' @export
#' @examples
#' get_functional_groups(
#'   file_path = fs::path(
#'     system.file("extdata", package = "ecosystemom"),
#'     "ewe_ecosim_with_environmental_data_nwatlantic", "basic_estimates.csv"
#'   )
#' )
#'
#' # If the automatic parser does not work for your naming convention, create
#' # the functional group tibble yourself. This object can be supplied anywhere
#' # ecosystemom asks for `functional_groups`.
#' functional_groups <- tibble::tibble(
#'   functional_group = c(
#'     "King mackerel_(0-1yr)",
#'     "King mackerel_(1+yr)",
#'     "Detritus"
#'   ),
#'   species = c("King mackerel", "King mackerel", "Detritus"),
#'   group = c("0-1yr", "1+yr", NA_character_),
#'   functional_group_snake_case = c(
#'     "king_mackerel_0_1yr",
#'     "king_mackerel_1_plusyr",
#'     "detritus"
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
