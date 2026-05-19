utils::globalVariables(c("functional_group", "species", "group"))
#' Split the functional groups into species and group
#'
#' Split strings containing functional groups into species and group names.
#' Functional groups can contain age ranges, plus groups, or sub categories,
#' e.g., juvenile. This information is extracted from the functional group name
#' and returned as the group name. The \code{functional_group_snake_case}
#' column is created by converting the \code{functional_group} name to lowercase
#' and substituting spaces or hyphens with underscores.
#'
#' @param x A character vector of functional group names. This is most often
#'   created by running [unique()] on the functional group column of the data.
#' @return
#' A tibble with the following three columns: functional_group, species,
#' group, and functional_group_snake_case is returned.
#' @export
#' @author Kelli F. Johnson
#' @examples
#' # A hypothetical example of functional groups
#' split_functional_groups(
#'   c("spiny dogfish 0", "spiny dogfish 1-2", "spiny dogfish +")
#' )
#' split_functional_groups(c("spiny dogfish", "spiny dogfish juvenile"))
#' data("ewe_ecosim_base_nwatlantic", package = "ecosystemom")
#' split_functional_groups(unique(ewe_ecosim_base_nwatlantic[["functional_group"]]))
split_functional_groups <- function(x) {
  if (!inherits(x, what = "character")) {
    cli::cli_abort("{.var x} must be a character vector not a {class(x)}.")
  }
  if (length(x) != length(unique(x))) {
    cli::cli_abort("{.var x} must be unique functional groups.")
  }
  test_for_characters <- grepl("[^[:alnum:] _()+-]", x)
  if (any(test_for_characters)) {
    cli::cli_abort(c(
      x = "{.var x} cannot contain non alpha-numerics except +, -, _, (, or ).",
      i = "The following functional groups contain non alpha-numerics:",
      "{.var {x[test_for_characters]}}"
    ))
  }

  groups <- x |>
    # Split camelCase and PascalCase into words separated by a space
    gsub(pattern = "([a-z])([[:upper:]])", replacement = "\\1 \\2") |>
    # Split words followed by a number into words separated by a space
    gsub(pattern = "([a-z])([[:digit:]])", replacement = "\\1 \\2") |>
    # split digits separated by an underscore into digits separated by a dash
    # e.g., 1_2 becomes 1-2
    gsub(pattern = "([[:digit:]])_([[:digit:]])", replacement = "\\1-\\2") |>
    # Parentheses are sometimes used around age/group suffixes in EwE outputs
    # (e.g., "King mackerel (0-1yr)"); strip wrappers before parsing.
    gsub(pattern = "[()]", replacement = "")

  # Parse explicit age/plus suffixes first, e.g. "King mackerel 1+yr"
  suffix_pattern <- "^(.*) ([[:alnum:]_+\\-]*[0-9+][[:alnum:]_+\\-]*)$"
  has_explicit_suffix <- grepl(suffix_pattern, groups)
  species <- ifelse(
    has_explicit_suffix,
    sub(suffix_pattern, "\\1", groups),
    groups
  )
  group <- ifelse(
    has_explicit_suffix,
    sub(suffix_pattern, "\\2", groups),
    NA_character_
  )

  # Parse keyword suffixes like 'juv', 'juvenile', and 'adult' that do not
  # require a base functional group to exist.
  keyword_suffixes <- c("juv", "juvenile", "adult")
  keyword_suffix_pattern <- paste0("^(.*) (", paste(keyword_suffixes, collapse = "|"), ")$")
  has_keyword_suffix <- !has_explicit_suffix & grepl(keyword_suffix_pattern, groups, ignore.case = TRUE)

  species[has_keyword_suffix] <- sub(keyword_suffix_pattern, "\\1", groups[has_keyword_suffix], ignore.case = TRUE)
  group[has_keyword_suffix] <- tolower(sub(keyword_suffix_pattern, "\\2", groups[has_keyword_suffix], ignore.case = TRUE))

  # Parse alpha-only suffixes only when the species stem exists as a full
  # functional group, e.g. "spiny dogfish" and "spiny dogfish juvenile".
  alpha_suffix_pattern <- "^(.*) ([[:alpha:]]+)$"
  has_alpha_suffix <- !has_explicit_suffix & !has_keyword_suffix & grepl(alpha_suffix_pattern, groups)
  alpha_stem <- ifelse(
    has_alpha_suffix,
    sub(alpha_suffix_pattern, "\\1", groups),
    NA_character_
  )
  can_use_alpha_suffix <- has_alpha_suffix & alpha_stem %in% groups

  species[can_use_alpha_suffix] <- alpha_stem[can_use_alpha_suffix]
  group[can_use_alpha_suffix] <- sub(
    alpha_suffix_pattern,
    "\\2",
    groups[can_use_alpha_suffix]
  )

  tibble::tibble(
    functional_group = x,
    species = species,
    group = group,
    functional_group_snake_case = to_snake_case(functional_group)
  )
}

#' Convert a string to snake_case
#'
#' This helper function takes a character vector and converts it to snake_case.
#' It performs several transformations:
#' \itemize{
#'   \item Converts the string to lowercase.
#'   \item Removes parentheses.
#'   \item Inserts a space between letters and numbers.
#'   \item Replaces `+` with `_plus`.
#'   \item Replaces spaces, hyphens, and pipe characters with underscores.
#'   \item Collapses multiple underscores into a single one.
#' }
#'
#' @param x A character vector to convert.
#' @return A character vector in snake_case.
#' @noRd
to_snake_case <- function(x) {
  # Convert to lowercase
  tolower(x) |>
    # Ignore parentheses in source names
    (
      \(x) gsub("[()]", "", x)
    )() |>
    # Split words followed by a number with a space
    (\(x) gsub("([a-z])([[:digit:]])", "\\1 \\2", x))() |>
    # Replace '+' with '_plus'
    (\(x) gsub("\\+", "_plus", x))() |>
    # Replace spaces, hyphens, and slashes with underscores
    (\(x) gsub("[[:space:]|-]+", "_", x))() |>
    # Collapse multiple underscores into one (e.g., "spiny__dogfish" -> "spiny_dogfish")
    (\(x) gsub("_+", "_", x))()
}
