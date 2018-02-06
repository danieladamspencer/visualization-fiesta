library(rjson)
json_file <- "https://developer.nrel.gov/api/alt-fuel-stations/v1/nearest.json?api_key=rgPvVtLCOCU3zjqP2v1xgCCniC6Swb7Mi22WpS5f&location=Santa_Cruz+CA"
alt_fuel <- fromJSON(paste(readLines(json_file),collapse = ""))
stations <- alt_fuel$fuel_stations
