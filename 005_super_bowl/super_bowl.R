library(openxlsx)
library(tidyverse)
library(magrittr)
library(RColorBrewer)

super_bowl <- read.xlsx("super_bowl/Super-Bowl-List.xlsx",detectDates = TRUE) %>% 
  set_colnames(c("bowl",
                 "date",
                 "location",
                 "city_state",
                 "latitude",
                 "longitude",
                 "afc_team",
                 "nfc_team",
                 "afc_score",
                 "nfc_score",
                 "favorite",
                 "attendance",
                 "mvp",
                 "mvp_position",
                 "est_viewers",
                 "network",
                 "cost_of_30_second_commercial",
                 "wiki_link"))

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols <- gg_color_hue(2)
football_plot <- super_bowl %>% 
  dplyr::mutate(cost_per_viewer = cost_of_30_second_commercial / (est_viewers * (10^6)),
                total_score = afc_score + nfc_score) %>% 
  ggplot() + geom_line(aes(x = date,y = cost_per_viewer*100),col = cols[1]) +
  geom_line(aes(x = date, y = log(total_score - 12)),col = cols[2]) +
  labs(x = "Year",y = "Cost per Viewer (cents)") + theme_bw() +
  ggtitle("Cost of Commercials and Total Score for the Super Bowl") +
  scale_y_continuous(sec.axis = sec_axis(trans = ~exp(.) + 12,name = "Total Score")) +
  theme(axis.title.y = element_text(color = cols[1]),
        axis.title.y.right = element_text(color = cols[2]))

print(football_plot)

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

library(grid)
library(gridExtra)

plot_w_footnote <- arrangeGrob(football_plot,
                               bottom = textGrob(footnote,x = -67, hjust = 0, vjust = 0,
                                                 gp = gpar(fontface = "italic",fontsize = 12)))

print(football_plot)
makeFootnote("Dan Spencer [2018]", 
             color = "black",
             size = 0.5)
pushViewport(viewport())
grid.text(label = "Data Source: http://overflow.solutions/datasets/super-bowl-lii-datasets/" ,
          x = unit(0,"npc") + unit(2, "mm"),
          y = unit(2, "mm"),
          just = c("left", "bottom"),
          gp = gpar(cex = 0.5, col = "black"))
popViewport()
grid.draw(plot_w_footnote)

