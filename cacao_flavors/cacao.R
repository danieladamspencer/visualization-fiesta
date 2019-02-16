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

mean_lm <- lm(rating ~ 1,data = cacao)
step_scope <- list(lower = as.formula("rating ~ 1"),
                   upper = as.formula("rating ~ review_date + percent_cocoa + company_location + general_origin"))
step(mean_lm,direction = "forward",scope = step_scope)
