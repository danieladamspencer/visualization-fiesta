library(tidyverse)

coffee_spending <- read.csv("coffee_tea_cocoa_world/coffee_tea_cocoa_summarized.csv",header=TRUE)

pop_2015 <- read.csv("coffee_tea_cocoa_world/population_world.csv",header = TRUE) %>% 
  dplyr::filter(Series.Name == "Population, total") %>% 
  dplyr::select(Country.Name, X2015..YR2015.) %>% 
  dplyr::mutate(population = as.numeric(as.character(X2015..YR2015.))) %>% 
  dplyr::rename(country = Country.Name) %>% 
  dplyr::select(-X2015..YR2015.) %>% na.omit()

pop_2015$country <- gsub("-"," ",pop_2015$country)


coffee_per_cap <- coffee_spending %>% 
  dplyr::left_join(pop_2015,by="country") %>% 
  dplyr::mutate(coffee_tea_cocoa = coffee_tea_cocoa*1e6) %>% 
  dplyr::mutate(spend_per_cap = coffee_tea_cocoa / population)

library(rworldmap)
library(RColorBrewer)
my_pal <- brewer.pal(7,"YlOrBr")
map_coffee <- joinCountryData2Map(coffee_per_cap,joinCode = "NAME", nameJoinColumn = "country")
par(mar=c(2,0,2,0))
mapCountryData(map_coffee,nameColumnToPlot = "spend_per_cap", 
               mapTitle = "Annual Spending on Coffee, Tea,\n and Cocoa per Capita (US $)",
               oceanCol = "lightblue",
               colourPalette = my_pal,
               missingCountryCol = "grey50",
               ylim = c(-80,40),
               addLegend = FALSE)
addMapLegend(my_pal,
             cutVector = quantile(coffee_per_cap$spend_per_cap,probs = seq(0,1,length.out = 8)),
             legendLabels = "all")
mtext(side = 1,line=1,at=150,text = "Data Source: World Bank",cex = 0.6)
mtext(side = 1,line=1,at = -150, text = "Dan Spencer [2018]",cex=0.6)



