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
