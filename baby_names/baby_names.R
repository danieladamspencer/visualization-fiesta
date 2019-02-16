# Time series of counts of baby names ----
library(tidyverse)
library(magrittr)
source("useful.R")
baby_names <- read.csv("baby_names/NationalNames.csv")
ira <- dplyr::filter(baby_names,Name == "Ira")
lua <- dplyr::filter(baby_names,Name == "Lua")
otto <- dplyr::filter(baby_names,Name == "Otto")
compare_new <- dplyr::filter(baby_names,Name == "Lua" | Name == "Ira" | Name == "Otto") %>% 
  group_by(Year,Name) %>% 
  dplyr::summarize(Count = sum(Count))
my_plot <- ggplot(compare_new) + geom_line(aes(x = Year,y = Count, color = Name)) + 
  ggtitle("Babies Born in the US") +
  theme_bw() 

left_right_footnote(my_plot,"Dan Spencer [2018]","https://www.kaggle.com/kaggle/us-baby-names/data")

# Filtering and searching baby names ----
male_length_three <- baby_names %>% 
  dplyr::select(Name,Gender,Year, Count) %>%
  dplyr::distinct() %>% 
  dplyr::filter(str_length(Name) == 3,
                Gender == "M") 

male_length_three %>% 
  dplyr::group_by(Name) %>% 
  dplyr::summarise(Count = sum(Count)) %>% 
  # dplyr::filter(Count == 5) %>% 
  dplyr::arrange(Count) %>%
  tail(20) %>%
  use_series(Name)

male_length_three %>% 
  dplyr::mutate(last = substring(Name,3,3)) %>% 
  dplyr::filter(last == "y") %>% 
  dplyr::group_by(Name) %>% 
  dplyr::summarise(Count = sum(Count)) %>% View()

# By syllable count ----
baby_names %>% 
  dplyr::mutate(Name = as.character(Name)) %>% 
  dplyr::filter(Gender == "M",
                str_length(Name) == 4,
                quanteda::nsyllable(Name) == 2) %>% str
