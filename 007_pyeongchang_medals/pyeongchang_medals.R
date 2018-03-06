# Load the library
library(tidyverse)
library(rvest)
library(magrittr)
library(stringr)
source("useful.R")

# Here's the URL for the page with the table on it
url <- "https://www.pyeongchang2018.com/en/game-time/results/OWG2018/en/general/detailed-medal-standings-.htm"
  
# Pull the table from the html
scrape_table <- url %>% 
  read_html() %>% 
  html_nodes("table") %>% 
  html_table(fill=TRUE,header = TRUE) %>% 
  magrittr::extract2(1)

# Change the names so they make sense
scraped_header <- names(scrape_table)
which_change <- 3:18
names(scrape_table)[which_change] <-  paste(scraped_header[which_change],rep(c("M","W","X","T"),3),sep = " - ")
scrape_table <- scrape_table[-1,]

scrape_table %<>% 
  dplyr::select(-c(Rank,RankbyTotal,starts_with("Total")))

medals <- scrape_table %>% 
  # dplyr::select(contains("Medal")) %>% 
  tidyr::gather("medal_cat","count",-NOC) %>% 
  dplyr::mutate(medal = stringr::word(medal_cat,1),
                abbr = stringr::word(medal_cat,-1),
                category = gsub("M","Men",abbr),
                category = gsub("W","Women",category),
                category = gsub("X","Co-ed",category),
                country = NOC,
                count = as.numeric(count),
                medal = factor(medal,levels = c("Gold","Silver","Bronze"))) %>% 
  dplyr::filter(category != "T",
                country != "Total") %>% 
  dplyr::select(country,medal,category,count)

write.csv(medals,file = "007_pyeongchang_medals/medals.csv")

# png(filename = "007_pyeongchang_medals/pyeongchang_medals.png", width = 1080, height = 1080)
output <- ggplot(medals) + geom_bar(aes(x = category, y = count, fill = medal),stat = "identity") + facet_wrap(~country) + scale_fill_manual(name = "Medal",values = c("Gold" = "gold","Silver" = "grey80","Bronze" = "wheat3")) + labs(x = "", y = "Medal Count") + ggtitle("Medal summary for the Pyeongchang Winter Olympics") + theme_bw()
# dev.off()
left_right_footnote(output,"/u/ninjasinthewind [2018]","Data Source: https://www.pyeongchang2018.com/en/game-time/results/OWG2018/en/general/detailed-medal-standings-.htm")
