library(tidyverse)
library(magrittr)

cacao <- read.csv("cacao_flavors/flavors_of_cacao.csv")
cacao %<>% 
  magrittr::set_colnames(c("maker",
                           "bean_origin",
                           "ref",
                           "review_date",
                           "percent_cocoa",
                           "company_location",
                           "rating",
                           "bean_type",
                           "general_origin")) %>% 
  dplyr::mutate(percent_cocoa = as.numeric(gsub("%","",percent_cocoa)))

cacao %>% 
  ggplot() + geom_point(aes(x = percent_cocoa, 
                            y = rating,
                            color = bean_type)) +
  theme_bw() + theme(legend.position = "none")
