# This is an analysis of climate data using tensor models for my interview with Verisk
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting

# The tropopause is the boundary that demarcates the troposphere from the stratosphere, and is the part of the atmosphere where there occurs an abrupt change in the environmental lapse rate (ELR), from a positive rate in the troposphere to a negative rate in the stratosphere.
data_dir <- "~/github/visualization-fiesta/NCAR_data/troposphere_air_temp"
result_files <- list.files(data_dir, full.names = TRUE)
data_1948 <- nc_open(result_files[1])
print(data_1948)
data_1948_array <- ncvar_get(data_1948,"air")

tropopause_temp <- sapply(1980:2021, function(yy) {
  data_year <- result_files |> grep(pattern = paste0(yy,".nc"),value = T) |> nc_open()
  air_array <- ncvar_get(data_year,"air")
  return(apply(air_array,1:2,mean))
}, simplify = "array") |> 
  abind::abind(along = 3)

# CO2 data
# library(tidyverse)
# co2_data <- read_csv("~/github/visualization-fiesta/NCAR_data/co2_trend_gl.csv",skip = 41) |> 
#   filter(year < 2022)
# plot(co2_data$smoothed)
# lines(co2_data$trend)
# plot(diff(co2_data$smoothed))

# First stab at an analysis
# result_dir <- "~/github/visualization-fiesta/NCAR_data"
# library(bayestensorreg)
# daily_diff_co2 <- as.matrix(diff(co2_data$smoothed))
# daily_temp <- tropopause_temp[,,-1]
# daily_temp <- array(daily_temp, dim = c(dim(daily_temp),1))
# 
# result <- BTRR_single_subject(list(Y = daily_temp, x = daily_diff_co2))
# saveRDS(result, file = file.path(result_dir,"naive_co2_TRR_analysis.rds"))
# This will never stop running. Next idea!

# Cost data
cost <-
  read_csv("~/github/visualization-fiesta/NCAR_data/damage-costs-from-natural-disasters.csv") |> 
  rename(Cost = `Total economic damage from natural disasters`) |> 
  select(-Code) |> 
  mutate(Cost = ifelse(is.na(Cost),0,Cost)) |> 
  pivot_wider(id_cols = Year,names_from = Entity, values_from = Cost) |> 
  mutate(Atmospheric = Drought + `Extreme temperature` + `Extreme weather` + Flood + Landslide + Wildfire) |> 
  select(Year, Atmospheric) |> 
  filter(Year < 2022)

tropopause_temp <- sapply(1980:2021, function(yy) {
  data_year <- result_files |> grep(pattern = paste0(yy,".nc"),value = T) |> nc_open()
  air_array <- ncvar_get(data_year,"air")
  return(apply(air_array,1:2,mean))
}, simplify = "array") |> 
  abind::abind(along = 3)

mdl_input <- as.TR_data(cost$Atmospheric, tropopause_temp)

test <- BTRTucker(input = mdl_input,ranks = c(1,1),n_iter = 100,n_burn = 0)
test2 <- BTRTucker(input = mdl_input,ranks = c(2,2),n_iter = 100,n_burn = 0)
test3 <- BTRTucker(input = mdl_input,ranks = c(3,3),n_iter = 100,n_burn = 0)
test4 <- BTRTucker(input = mdl_input,ranks = c(4,4),n_iter = 100,n_burn = 0)
test5 <- BTRTucker(input = mdl_input,ranks = c(5,5),n_iter = 100,n_burn = 0)
sapply(list(test,test2,test3,test4,test5), function(x) DIC(x$llik))

BTRT_finalB <- BTRT_final_B(test)
image(BTRT_finalB)
summary(c(BTRT_finalB))
