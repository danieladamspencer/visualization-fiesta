dnut <- read.csv("~/Downloads/multiTimeline.csv",header=T)

library(tidyverse)
dnut %>% 
  dplyr::mutate(Date = as.Date(paste0(Month,"-01"))) %>% 
  dplyr::select(-Month) %>% 
  tidyr::gather(key = "Spelling",value = "Index",-Date) %>% 
  ggplot() + geom_line(aes(x = Date, y = Index, col = Spelling)) + theme_bw() +
  ggtitle("Google Trends")
