# Read in the data from the county level estimates of educational attainment
edu <- read.csv("county_education/Education.csv", header = T)
summary(edu)

library(maps)
data("county.fips")

library(tidyverse)
edu_df <- left_join(county.fips, edu, by = c('fips' = "FIPS.Code")) %>% 
  filter(!is.na(Percent.of.adults.with.less.than.a.high.school.diploma..2015.19)) %>% 
  mutate(lths = Percent.of.adults.with.less.than.a.high.school.diploma..2015.19,
         hs = Percent.of.adults.with.a.high.school.diploma.only..2015.19,
         ass = Percent.of.adults.completing.some.college.or.associate.s.degree..2015.19,
         bs = Percent.of.adults.with.a.bachelor.s.degree.or.higher..2015.19,
         nocol = lths + hs,
         nocol_col = )

library(ggmap)
