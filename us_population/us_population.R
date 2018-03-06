library(tidyverse)
library(rvest)
library(magrittr)

url <- "https://en.wikipedia.org/wiki/Demography_of_the_United_States"

all_tables <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table(fill = TRUE)

from_census <- all_tables[[2]] %>% 
  set_names(c("Year","Population","","Change")) %>% 
  extract(-1,) %>% 
  extract(,-(3:4)) %>% 
  dplyr::mutate(Year = as.numeric(Year),
                Population = as.numeric(gsub(",","",Population))) %>% 
  na.omit()

# ggplot(from_census) + geom_line(aes(x = Year, y = Population/1e6)) + 
#   labs(y = "Population (in millions)") +
#   ggtitle("Population from US Census") +
#   theme_bw()

estimates <- all_tables[[3]]
names(estimates) <- tolower(gsub(" ","_",names(estimates)))
names(estimates)[1] <- "year"
names(estimates)[c(2,6,7,8)] <- c("avg_pop","crude_birth_rate","crude_death_rate","natural_change")
est <- sapply(estimates,function(x) as.numeric(gsub(",","",x))) %>% 
  as.data.frame()


save(est,from_census,file = "us_population/population_data.RData")
