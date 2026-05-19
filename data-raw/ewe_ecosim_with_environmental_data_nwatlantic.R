## code to prepare `ewe_ecosim_with_environmental_data_nwatlantic` dataset goes here
# This script prepares the `ewe_ecosim_with_environmental_data_nwatlantic` dataset.
# This dataset is an Ecopath with Ecosim (EwE) model for the Northwest Atlantic.
# The raw data for this model is stored in the
# `inst/extdata/ewe_ecosim_with_environmental_data_nwatlantic` directory.
# The script processes these raw files and creates an R object that is then
# included in the package.

# Define the path to the directory containing the raw EwE model files.
# Using fs::path ensures cross-platform compatibility for file paths.
raw_file_path <- fs::path(
  "inst", "extdata", "ewe_ecosim_with_environmental_data_nwatlantic"
)

# Extract the functional groups from the model's basic estimates file.
# The `get_functional_groups` function reads
# the specified CSV file to get a list of all functional groups in the model.
functional_groups <- get_functional_groups(
  file_path = fs::path(
    raw_file_path, "basic_estimates.csv"
  )
)

# Load the complete EwE model.
# The `load_model` function reads all the necessary
# files from the specified directory to construct the full model object.
# It requires the list of functional groups that was extracted in the previous step.
ewe_ecosim_with_environmental_data_nwatlantic <- load_model(
  directory = raw_file_path,
  type = "ewe_ecosim",
  functional_groups = functional_groups
)

# Save the loaded model object as a dataset in the package.
# `usethis::use_data` saves the `ewe_ecosim_with_environmental_data_nwatlantic` object into the
# `data/` directory of the package. This makes the dataset available to users
# of the 'ecosystemom' package once it's loaded.
# `overwrite = TRUE` allows updating the dataset if this script is run again.
usethis::use_data(ewe_ecosim_with_environmental_data_nwatlantic, overwrite = TRUE)
