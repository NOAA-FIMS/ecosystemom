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
