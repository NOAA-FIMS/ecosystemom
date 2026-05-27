#' Ecopath with Ecosim (EwE) models for the Northwest Atlantic
#'
#' These datasets contain tidy format tibbles of two EwE model outputs for the
#' Northwest Atlantic Ocean. `ewe_ecosim_base_nwatlantic` is from the base
#' model run, and `ewe_ecosim_with_environmental_data_nwatlantic` is from a
#' run that includes environmental data drivers. Both models are loaded from
#' raw EwE files and tidied using the `load_model` function.
#'
#' @format
#' Both datasets are tibbles with `r NROW(ewe_ecosim_base_nwatlantic)` rows and
#' `r NCOL(ewe_ecosim_base_nwatlantic)` columns:
#' \describe{
#'   \item{file_name}{The path to the file name, note that the path could be a
#'     relative path. This column is helpful for understanding which EWE model
#'     and which run the data came from.
#'   }
#' \item{type}{The data type represented by the row. Valid types include:
#'   \itemize{
#'     \item \code{"catch"}
#'     \item \code{"biomass"}
#'     \item \code{"weight"}
#'     \item \code{"total_mortality"}
#'     \item \code{"fishing_mortality"} (calculated as catch divided by biomass)
#'     \item \code{"natural_mortality"} (calculated as total mortality minus fishing mortality)
#'   }
#' }
#'   \item{year}{A four-digit integer specifying the year.}
#'   \item{month}{A one- or two-digit integer specifying the month.}
#'   \item{functional_group}{A string specifying the functional group that this
#'     row pertains to.}
#'   \item{value}{A real number containing the value of interest.}
#'   \item{species}{The species name.}
#'   \item{group}{The age or stanza for the species (e.g., `0`, `2-5`, `6+`).}
#'   \item{functional_group_snake_case}{The functional group name in snake_case.}
#'   \item{unit}{The unit of the value.}
#' }
#'
#' @source The data is prepared from raw model files using the scripts in the
#'   `data-raw` directory.
#'
#' @references
#' Chagaris, D., Drew, K., Schueller, A., Cieri, M., Brito, J., and
#' Buchheister, A. 2020. Ecological reference points for Atlantic menhaden
#' established using an ecosystem model of intermediate complexity. Frontiers
#' in Marine Science, 7:606417. 10.3389/fmars.2020.606417.
"ewe_ecosim_base_nwatlantic"

#' @rdname ewe_ecosim_base_nwatlantic
"ewe_ecosim_with_environmental_data_nwatlantic"