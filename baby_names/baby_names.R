library(tidyverse)
source("useful.R")
baby_names <- read.csv("baby_names/NationalNames.csv")
ira <- dplyr::filter(baby_names,Name == "Ira")
lua <- dplyr::filter(baby_names,Name == "Lua")
ivy <- dplyr::filter(baby_names,Name == "Ivy")
compare_new <- dplyr::filter(baby_names,Name == "Lua" | Name == "Ira" | Name == "Zia") %>% 
  group_by(Year,Name) %>% 
  dplyr::summarize(Count = sum(Count))
my_plot <- ggplot(compare_new) + geom_line(aes(x = Year,y = Count, color = Name)) + 
  ggtitle("Babies Born in the US") +
  theme_bw() 

left_right_footnote(my_plot,"Dan Spencer [2018]","https://www.kaggle.com/kaggle/us-baby-names/data")
