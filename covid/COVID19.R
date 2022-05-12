# This is a COVID19 analysis
require(tidyverse)
require(scales)
require(readxl)
library(ggrepel)
#these libraries need to be loaded
library(utils)

# read the Dataset sheet into “R”. The dataset will be called "covid". ----
covid <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
levels(covid$countriesAndTerritories)[which(levels(covid$countriesAndTerritories) == "United_States_of_America")] <- "USA"
covid$dateRep <- as.Date(covid$dateRep, format = "%d/%m/%Y")

# Rank of cumulative COVID cases ----
covid_case_rank <- covid %>% 
  arrange(dateRep) %>% 
  group_by(`countriesAndTerritories`) %>% 
  mutate(cum_cases = cumsum(cases)) %>% 
  summarize(max_cases = max(cum_cases)) %>% 
  ungroup() %>% 
  mutate(case_rank = rank(-max_cases, ties.method = "min"),
         country_labels = ifelse(case_rank <= 5, as.character(`countriesAndTerritories`), ""))

# Rank of cumulative COVID deaths ----
covid_death_rank <- covid %>% 
  arrange(dateRep) %>% 
  group_by(`countriesAndTerritories`) %>% 
  mutate(cum_deaths = cumsum(deaths)) %>% 
  summarize(max_deaths = max(cum_deaths)) %>% 
  ungroup() %>% 
  mutate(death_rank = rank(-max_deaths, ties.method = "min"),
         country_labels = ifelse(death_rank <= 5, as.character(`countriesAndTerritories`), ""))

# A look at cumulative cases over time ----
covid %>% 
  left_join(covid_case_rank) %>% 
  arrange(dateRep) %>% 
  group_by(`countriesAndTerritories`) %>% 
  mutate(`Total cases` = cumsum(cases),
         dateRep = as.Date(dateRep)) %>% 
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = dateRep, y = `Total cases`, color = geoId)) +
  geom_text(aes(x = max(dateRep) + 3, y = max_cases, label = country_labels)) +
  scale_x_date(date_breaks = "1 month",
               labels=date_format("%b-%Y"),
               limits = as.Date(c('2019-12-25','2020-07-01'),"%Y-%m-%d")) +
  labs(x = "Date", title = paste("COVID-19","-",format(Sys.time(),"%b %d, %Y"))) +
  theme_bw() +
  theme(legend.position = "none")

# A look at cumulative deaths over time ----
covid %>% 
  left_join(covid_death_rank) %>% 
  arrange(dateRep) %>% 
  group_by(`countriesAndTerritories`) %>% 
  mutate(`Total deaths` = cumsum(deaths),
         dateRep = as.Date(dateRep)) %>% 
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = dateRep, y = `Total deaths`, color = geoId)) +
  geom_text_repel(aes(x = max(dateRep) + 3, y = max_deaths, label = country_labels)) +
  scale_x_date(date_breaks = "1 month",
               labels=date_format("%b-%Y"),
               limits = as.Date(c('2019-12-25','2020-07-01'),"%Y-%m-%d")) +
  labs(x = "Date", title = paste("COVID-19","-",format(Sys.time(),"%b %d, %Y"))) +
  # scale_y_log10() +
  theme_bw() +
  theme(legend.position = "none")

# EPI curve for the USA ----
covid %>% 
  filter(`countriesAndTerritories` == "USA", cases > 0) %>% 
  ggplot() +
  geom_bar(aes(x = dateRep, y = cases), stat = "identity",width = 0.8) +
  geom_text(aes(x = as.Date("2020-01-20"), y = 50000, 
                label = "Data from \n https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
                hjust = "left")) +
  scale_y_log10() +
  labs(x = "Date", y = "New Cases", title = "USA COVID-19") +
  theme_bw()

# A look at the spreadsheet for the most recent day ----
covid %>% 
  left_join(covid_case_rank) %>% 
  arrange(dateRep) %>% 
  group_by(`countriesAndTerritories`) %>% 
  mutate(`Total cases` = cumsum(cases),
         `Total deaths` = cumsum(deaths),
         Mortality = `Total deaths` / `Total cases`) %>% 
  filter(dateRep == max(dateRep)) %>% View

# Look at the mortality over time ----
covid %>% 
  left_join(covid_case_rank) %>% 
  arrange(dateRep) %>% 
  group_by(`countriesAndTerritories`) %>% 
  mutate(`Total cases` = cumsum(cases),
         `Total deaths` = cumsum(deaths),
         dateRep = as.Date(dateRep),
         Mortality = 100000 * `Total deaths` / `Total cases`,
         Mortality = ifelse(is.na(Mortality), 0, Mortality),
         is_USA = ifelse(`countriesAndTerritories` == "USA", "USA", ""),
         USA = ifelse(`countriesAndTerritories` == "USA","USA","")) %>% 
  filter(`Total cases` > 20, # This part is to eliminate countries with few reported cases and/or no deaths
         Mortality > 0) %>% 
  ungroup() %>% 
  ggplot() +
  geom_line(aes(x = dateRep, y = Mortality, group = `countriesAndTerritories`, color = is_USA)) +
  geom_text(aes(x = max(dateRep) + 3, y = last(Mortality, order_by = is_USA), label = USA)) +
  scale_color_manual(values = c("grey50","red")) + 
  scale_y_log10() +
  labs(y = "Mortality (# deaths per 100,000 cases)") +
  scale_x_date("Date",date_breaks = "1 month", 
               labels=date_format("%b-%Y"),
               limits = as.Date(c('2019-12-25','2020-06-01'),"%Y-%m-%d")) +
  theme_bw() +
  theme(legend.position = "none")
  