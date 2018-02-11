# Processing from the raw post ----

library(rjson)
library(tidyverse)
library(magrittr)

univ <- fromJSON(file = "university_rankings/schoolInfo.json")
col_names <- names(univ[[1]]) %>% gsub(pattern = "-", replacement = "_",x=.)

unlisted_univ_level <- sapply(univ,function(each_university){
  is_null <- sapply(each_university,is.null)
  each_university[is_null] <- "NA"
  unlist(each_university)
},simplify = FALSE)
full_univ <- Reduce(rbind,unlisted_univ_level) %>% as.data.frame 
names(full_univ) <- col_names

full_univ %<>% dplyr::mutate(act_avg = as.numeric(as.character(act_avg)),
                sat_avg = as.numeric(as.character(sat_avg)),
                enrollment = as.numeric(as.character(enrollment)),
                city = as.character(city),
                acceptance_rate = as.numeric(as.character(acceptance_rate)),
                rankingDisplayScore = as.numeric(as.character(rankingDisplayScore)),
                tuition = as.numeric(as.character(tuition)),
                percent_receiving_aid = as.numeric(as.character(percent_receiving_aid)),
                cost_after_aid = as.numeric(as.character(cost_after_aid)),
                hs_gpa_avg = as.numeric(as.character(hs_gpa_avg)),
                engineeringRepScore = as.numeric(as.character(engineeringRepScore)),
                univ_rank = as.numeric(substring(rankingDisplayRank,2)))

UCs <- grep("University of California",full_univ$displayName,value=F)

full_univ[UCs,]

write.csv(full_univ,file = "university_rankings/universities.csv")

# Read from the .csv that was created ----
full_univ <- read.csv(file="university_rankings/universities.csv")

# Get the average university rank for each state ----
univ_rank <- full_univ %>% 
  dplyr::filter(rankingRankStatus == "ranked")

state_abb_names <- data.frame(abb = state.abb,
                              name = state.name)

state_avg <- univ_rank %>% 
  dplyr::group_by(state) %>% 
  dplyr::summarize(avg_rank = mean(univ_rank)) %>% 
  dplyr::left_join(state_abb_names,by = c("state" = "abb")) %>% 
  dplyr::filter(state != "DC") %>% 
  dplyr::mutate(state_name = tolower(as.character(name)))

# Plot the results on a map of the USA ----

# >> Experiment with footnote function ----

# write a simple function to add footnote
makeFootnote <- function(footnoteText =
                           format(Sys.time(), "%d %b %Y"),
                         size = .7, color = grey(.5))
{
  require(grid)
  pushViewport(viewport())
  grid.text(label = footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y = unit(2, "mm"),
            just = c("right", "bottom"),
            gp = gpar(cex = size, col = color))
  popViewport()
}

# > Testing with ggmap ----
library(ggmap)
usa_center = as.numeric(geocode("United States"))
USAMap = ggmap(get_googlemap(center=usa_center, scale=2, zoom=4), extent="normal")

# > Using maps package ----
library(maps)
# install.packages("fiftystater")
library(fiftystater)

footnote <- "Data Source: US News and World Report\nDan Spencer [2018]"

avg_rank_map <- ggplot(data = state_avg,aes(map_id = state_name)) + 
  geom_map(aes(fill = avg_rank),map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) + 
  coord_map() + 
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) + 
  scale_fill_distiller(palette = "Greys",name = "Average Rank") + 
  labs(x="",y="") +
  fifty_states_inset_boxes() + 
  theme_bw() + 
  ggtitle("US News & World Report Average University Rank") +
  theme(legend.position = "bottom")

library(grid)
library(gridExtra)

plot_w_footnote <- arrangeGrob(avg_rank_map,
                               bottom = textGrob(footnote,x = -67, hjust = 0, vjust = 0,
                                                 gp = gpar(fontface = "italic",fontsize = 12)))

print(avg_rank_map)
makeFootnote("Data Source: US News & World Report", color = "black")
pushViewport(viewport())
grid.text(label = "Dan Spencer [2018]" ,
          x = unit(0,"npc") + unit(2, "mm"),
          y = unit(2, "mm"),
          just = c("left", "bottom"),
          gp = gpar(cex = 0.7, col = "black"))
popViewport()
grid.draw(plot_w_footnote)
