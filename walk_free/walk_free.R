library(tidyverse)

# free <- readxl::read_xlsx("FINAL-GSI-2016-GLOBAL-DATA-TABLE_FOR-NON-COMMERCIAL-USE-1522122445.xlsx",sheet = 2,skip = 1) %>% 
#   dplyr::na_if("no data") %>% 
#   dplyr::rename(country = Country,
#                 region = REGION,
#                 rights = `Dimension 1: Political Rights and Safety`,
#                 protections = `Dimension 2: Financial and Health Protections`,
#                 vulnerable_protections = `Dimension 3: Protection for the Most Vulnerable`,
#                 conflict = `Dimension 4: Conflict`,
#                 mean_vulnerability = `Mean Vulnerability`,
#                 population = POPULATION,
#                 percent_enslaved = `ESTIMATED PROPORTION OF POPULATION IN MODERN SLAVERY`,
#                 num_enslaved = `ESTIMATED NUMBER IN MODERN SLAVERY`,
#                 gov_response_rank = `GOVERNMENT RESPONSE RANK`,
#                 exit_support = `Milestone 1: Victims are supported to exit slavery (/32)`,
#                 exit_support_pct = `Milestone 1: Victims are supported to exit slavery (%)`,
#                 justice_responses = `Milestone 2: Criminal justice responses (/21)`,
#                 justice_responses_pct = `Milestone 2: Criminal justice responses (%)`,
#                 accountability = `Milestone 3: Coordination and accountability (/14)`,
#                 accountability_pct = `Milestone 3: Coordination and accountability (%)`,
#                 address_risk = `Milestone 4: Addressing risk (/25)`,
#                 address_risk_pct = `Milestone 4: Addressing risk (%)`,
#                 supply_chain = `Milestone 5: Investigating supply chains (/7)`,
#                 supply_chain_pct = `Milestone 5: Investigating supply chains (%)`,
#                 milestone_score = `Total score (/100)`,
#                 corruption = `Corruption or state sanctioned forced labour score (/-2)`,
#                 total_score = `Total score (U-V)`,
#                 credit = `CREDIT rating`) %>% 
#   dplyr::filter(!is.na(country)) %>% 
#   dplyr::mutate(exit_support = as.numeric(exit_support),
#                 exit_support_pct = as.numeric(exit_support_pct),
#                 justice_responses = as.numeric(justice_responses),
#                 justice_responses_pct = as.numeric(justice_responses_pct),
#                 accountability = as.numeric(accountability),
#                 accountability_pct = as.numeric(accountability_pct),
#                 address_risk = as.numeric(address_risk),
#                 address_risk_pct = as.numeric(address_risk_pct),
#                 supply_chain = as.numeric(supply_chain),
#                 supply_chain_pct = as.numeric(supply_chain_pct),
#                 milestone_score = as.numeric(milestone_score),
#                 corruption = as.numeric(corruption),
#                 total_score = as.numeric(total_score),
#                 credit = ifelse(credit == "BBB*","BBB",credit),
#                 credit = factor(credit,
#                                 levels = rev(c("A","B","BB","BBB","C","CC","CCC","D")),
#                                 ordered = TRUE))
  
load("walk_free/walk_free.RData")

ggplot(free) + geom_point(aes(x = total_score,
                              y = percent_enslaved,
                              color = region)) +
  theme_bw()

dplyr::filter(free,percent_enslaved > 2) %>% dplyr::select(country,percent_enslaved)

step_lm <- step(glm(cbind(num_enslaved,population) ~ -1 + . - country - region,
                    family = binomial(link = "logit"),
                    data = free,
                    na.action = "na.exclude"))

ggplot(free) + geom_boxplot(aes(x = region,y = percent_enslaved,color = region)) + theme_bw()

mw <- map_data(map = "world")
free[free$country == "United States",]$country <- "USA"
free[free$country == "Republic of the Congo",]$country <- "Republic of Congo"
free[free$country == "United Kingdom",]$country <- "UK"
plot_object <- dplyr::inner_join(mw,free,by = c("region" = "country")) %>% 
  ggplot() + geom_map(map = mw,aes(map_id = region,x = long, y = lat,fill = percent_enslaved)) + 
  ggtitle("The State of Modern Slavery") +
  scale_fill_distiller(name = "% Enslaved",palette = "Greys",na.value = "darkblue") +
  theme(panel.background = element_rect(fill = "deepskyblue",
                                        colour = "deepskyblue",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "deepskyblue"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "deepskyblue"),
        legend.position = "bottom",
        plot.margin = margin(1,1,0,-15),
        legend.margin = margin(-10,0,0,0),
        legend.box.margin = margin(-10,-10,5,-10),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5,size = 24)) +
  labs(x = "",y = "")

source("useful.R")
left_right_footnote(plot_object,"Dan Spencer [2018]","Data Source: \n https://www.walkfreefoundation.org/")
  # scale_fill_gradient2(name = "% Enslaved",
  #                      low = "green",
  #                      mid = "brown1",
  #                      high = "red",
  #                      midpoint = min(free$percent_enslaved) + diff(range(free$percent_enslaved)) / 2) +
  # theme_bw()
