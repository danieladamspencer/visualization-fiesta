# Test of NetCDF data read-in
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting

nc_data <- nc_open("~/Downloads/High_Resolution_Climate_Simulation_Dataset_540_Myr.nc")
{
  sink('~/Downloads/High_Resolution_Climate_metadata.txt')
  print(nc_data)
  sink()
}
