# calculate_truth() using data from ecosystemdata
# TODO: we could eventually get rid of type argument and calculate the truth
# for all available types; could add an argument to select an area. 
calc_truth <- function(data, stock_name, timestep){
  label <-  c("biomass", "catch")
  # Calculate the monthly index using purrr::map()
  # and dplyr::bind_rows()
  truth_index_monthly <- purrr::map(
    label,
    ~ calc_truth_index_monthly(data, stock_name, .x)
  ) 

  # Calculate the yearly index using purrr::map()
  truth_index_yearly <- purrr::map(
    truth_index_monthly,
    ~ calc_truth_index_yearly(.x)
  ) 

  # Remove functional_group and type from individual lists of truth_index_monthly
  # and truth_index_yearly
  truth_values <- c(
    truth_index_monthly,
    truth_index_yearly
  ) |>
    purrr::map(
      ~ dplyr::select(.x, -functional_group, -type)
    )
  
  type <- c("index")
  timestep <- c("monthly", "yearly")
  total_length <- length(label) * length(type) * length(timestep)
  # Build nested data
  truth <- tidyr::tibble(
    stock_name = rep(stock_name, each = total_length),
    truth_label = rep(label, times = length(type) * length(timestep)),
    truth_type = rep(type, each = total_length),
    truth_timestep = rep(timestep, times = length(label) * length(type)),
    truth_om = truth_values
  )  
  
}

calc_truth_index_monthly <- function(data, stock_name, truth_type){
  # Calculate the yearly truth using the data from ecosystemdata
  truth_monthly <- data |>
    # Filter the data to include only the specified types
    dplyr::filter(type %in% truth_type) |>
    # Extract all functional groups that have stock_name in their name
    dplyr::filter(grepl(stock_name, functional_group)) |>
    # Separate the functional group name into two columns. For example, 
    # "stock_name 1" becomes "stock_name" and "1".
    tidyr::separate(
      col = functional_group,
      into = c("functional_group", "age"),
      sep = "(?<=\\D)(?=\\d)"
    ) |>
    # Update age column to be numeric
    dplyr::mutate(age = as.numeric(age)) |>
    # Sum the data type (e.g., biomass) across all ages by month
    dplyr::group_by(functional_group, type, year, month) |>
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    ) 
  
  return(truth_monthly)
}

calc_truth_index_yearly <- function(truth_monthly){
  # Calculate the yearly truth 
  truth_yearly <- truth_monthly |>
    # Sum the data type (e.g., biomass) across all months by year
    dplyr::group_by(functional_group, type, year) |>
    dplyr::summarise(
      value = mean(value, na.rm = TRUE),
      .groups = "drop"
    ) 
}

# calculate_numbers()

# calculate_numbers_at_age()

# calculate_weight_at_age()

# calculate_selectivity_at_age()

