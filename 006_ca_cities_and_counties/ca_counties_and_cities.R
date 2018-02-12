# Libraries ----
library(openxlsx)
library(tidyverse)
source("useful.R")

# Function for percent change ----
pct_change <- function(x) 100*((x - dplyr::lag(x)) / dplyr::lag(x))

# Read the Excel spreadsheet ----
ca_pop <- read.xlsx("006_ca_cities_and_counties/population_by_county_and_city_-_1970_to_2016_-_public_-_oct_27_2016.xlsx")
names(ca_pop)[1:2] <- c("county","city")

# Clean the dataset ----
ca_all <- ca_pop %>% 
  tidyr::gather("year","pop",-c(county,city)) %>% 
  dplyr::mutate(year = as.numeric(year))

# Subset the data to county-level ----
ca_county <- ca_all %>% 
  dplyr::group_by(county,year) %>% 
  dplyr::summarize(pop = sum(pop))

# Find the percent change in population for each county ----
county_pct_change <- ca_county %>% 
  dplyr::group_by(county) %>% 
  dplyr::mutate(pct = pct_change(pop)) %>% 
  dplyr::ungroup()

# Take a look at all of the percent changes ----
county_pct_change %>% 
  ggplot() + geom_line(aes(x = year,y = pct,group = county)) + theme_bw()

# What the hell is going on in Tehama in 1975? ----
county_pct_change %>% 
  dplyr::filter(county == "Tehama") %>% View()

# Correct the strange population data in 1975 in Tehama county with linear interpolation ----
ca_county[ca_county$county == "Tehama" & ca_county$year == 1975,]$pop <- 
  round((ca_county[ca_county$county == "Tehama" & ca_county$year == 1974,]$pop +
  ca_county[ca_county$county == "Tehama" & ca_county$year == 1976,]$pop) / 2)

# Re-find the percent change in population for each county ----
county_pct_change <- ca_county %>% 
  dplyr::group_by(county) %>% 
  dplyr::mutate(pct = pct_change(pop)) %>% 
  dplyr::ungroup()

# Take a look at all of the percent changes and highlight Santa Cruz ----
line_plot <- county_pct_change %>% 
  ggplot() + geom_line(aes(x = year,y = pct,group = county)) +
  geom_line(data = dplyr::filter(county_pct_change,county == "Santa Cruz"),
            aes(x = year,y = pct),color = "red",lwd = 1.3) +
  labs(x = "Year",y = "Yearly Percent Change in Population") + 
  ggplot2::annotate("text",x = 2014, y = 2.5, label = "Santa Cruz", col = "red") + 
  ggtitle("California Counties") + theme_bw()
png(filename = "006_ca_cities_and_counties/county_pop_line_plot.png")
left_right_footnote(line_plot,
                    "Dan Spencer [2018]",
                    "Data Source: http://www.counties.org/post/datapile",
                    size = 0.5)
dev.off()

# Make a map of the percent change ----

# > Make a quick minimal theme to emulate theme_nothing(), but with legends ----
ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

# > Rename "county" to "subregion" in the county_pct_change data frame for good plotting ----
names(county_pct_change)[1] <- "subregion"
county_pct_change$subregion <- tolower(county_pct_change$subregion)

# > Plot it once ----
# These are just baseline maps in order to keep the maps easy
ca_state_map <- map_data("state",region = "california")
ca_county_map <- map_data("county",region = "california")

# Merge the county map df to the pct change df for mapping
map_pct_change <- dplyr::inner_join(ca_county_map,county_pct_change,by = "subregion")

a <- ggplot(ca_state_map,aes(x = long,y = lat, group = group)) + coord_fixed(1.3) + geom_polygon(data = map_pct_change[map_pct_change$year == 2014,],aes(fill = pct),color = "white") + geom_polygon(color = "black", fill = NA) + ggtitle(paste("California Population -",2014)) + scale_fill_gradient2(name = "Percent\nChange",limits = c(-10,10),low = "red",high = "green") + theme_bw() + ditch_the_axes
left_right_footnote(a,"Dan Spencer [2018]","Data Source: http://www.counties.org/post/datapile")


# Now plot it for each year ----
sapply(unique(county_pct_change$year),function(each_year){
  out <- ggplot(ca_state_map,aes(x = long,y = lat, group = group)) + 
    coord_fixed(1.3) + 
    geom_polygon(data = map_pct_change[map_pct_change$year == each_year,],
                 aes(fill = pct),color = "white") + 
    geom_polygon(color = "black", fill = NA) + ggtitle(paste("California Population -",each_year)) + 
    scale_fill_gradient2(name = "Percent\nChange",limits = c(-10,10),low = "red",high = "green") + 
    theme_bw() + ditch_the_axes
  png(filename = paste0("006_ca_cities_and_counties/reddit/pct_change_year_",each_year,".png"),
      width = 7, height = 7,res = 150,units = "in")
  left_right_footnote(out,"u/ninjasinthewind [2018]","Data Source: http://www.counties.org/post/datapile")
  dev.off()
})

# Merge the plots together in a gif ----
library(magick)
list.files("006_ca_cities_and_counties/reddit/",pattern = "*.png",full.names = TRUE) %>% 
  purrr::map(image_read) %>% 
  magick::image_join() %>% 
  magick::image_animate(fps = 2) %>% 
  magick::image_write("006_ca_cities_and_counties/reddit/pct_change.gif")
