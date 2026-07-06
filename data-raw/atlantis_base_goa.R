# Download Atlantis output files from Google Drive and save them to the 
#`inst/extdata/atlantis_goa` directory.
googledrive::drive_download(
  file = googledrive::as_id("1inDVvkm3cPOdtKVpTM9zvJZSm9uD1Ug1"),
  path = fs::path("inst", "extdata", "atlantis_goa.zip"),
  overwrite = TRUE
)

# unzip the downloaded file to the `inst/extdata/atlantis_goa` directory
unzip(
  zipfile = fs::path("inst", "extdata", "atlantis_goa.zip"),
  exdir = fs::path("inst", "extdata", "atlantis_goa")
)

# remove the zip file after unzipping
fs::file_delete(fs::path("inst", "extdata", "atlantis_goa.zip"))

