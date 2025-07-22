# creating a new folder in data folder called 2024-divvy-ripdata
data_dir <- "data/2024-divvy-tripdata"
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

# creating an object referring to the zip files
zip_files <- list.files(
  path = "data/divvy-tripdata.s3.amazonaws.com",
  pattern = "\\.zip$",
  full.names = TRUE
)

# unzipping the files into the new folder
for (zip_file in zip_files) {
  unzip(zip_file, exdir = data_dir)
  # Remove __MACOSX folder (if it exists)
  unlink(file.path(data_dir, "__MACOSX"), recursive = TRUE, force = TRUE)
}
