# Source
source("R/Drifblim.R")

# Example
input_file <- file.path("data", "HLY_station_info.csv")
station_data <- read.csv(input_file) # This will be the same for every user (comes with package)

out_dir <- tempdir() # temp directory
# all_stations(station_data[1:100,], out_dir, first_year = 1980, last_year = 2020)
station_by_name_id(station_name = "MALAHAT")
