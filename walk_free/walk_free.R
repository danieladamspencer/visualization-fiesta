library(tidyverse)

free <- readxl::read_xlsx("FINAL-GSI-2016-GLOBAL-DATA-TABLE_FOR-NON-COMMERCIAL-USE-1522122445.xlsx",sheet = 2,skip = 1) %>% 
  dplyr::na_if("no data") %>% 
  dplyr::rename(country = Country,
                region = REGION,
                rights = `Dimension 1: Political Rights and Safety`,
                protections = `Dimension 2: Financial and Health Protections`,
                vulnerable_protections = `Dimension 3: Protection for the Most Vulnerable`,
                conflict = `Dimension 4: Conflict`,
                mean_vulnerability = `Mean Vulnerability`,
                population = POPULATION,
                percent_enslaved = `ESTIMATED PROPORTION OF POPULATION IN MODERN SLAVERY`,
                num_enslaved = `ESTIMATED NUMBER IN MODERN SLAVERY`,
                gov_response_rank = `GOVERNMENT RESPONSE RANK`,
                exit_support = `Milestone 1: Victims are supported to exit slavery (/32)`,
                exit_support_pct = `Milestone 1: Victims are supported to exit slavery (%)`,
                justice_responses = `Milestone 2: Criminal justice responses (/21)`,
                justice_responses_pct = `Milestone 2: Criminal justice responses (%)`,
                accountability = `Milestone 3: Coordination and accountability (/14)`,
                accountability_pct = `Milestone 3: Coordination and accountability (%)`,
                address_risk = `Milestone 4: Addressing risk (/25)`,
                address_risk_pct = `Milestone 4: Addressing risk (%)`,
                supply_chain = `Milestone 5: Investigating supply chains (/7)`,
                supply_chain_pct = `Milestone 5: Investigating supply chains (%)`,
                milestone_score = `Total score (/100)`,
                corruption = `Corruption or state sanctioned forced labour score (/-2)`,
                total_score = `Total score (U-V)`,
                credit = `CREDIT rating`) %>% 
  dplyr::filter(!is.na(country)) %>% 
  dplyr::mutate(exit_support = as.numeric(exit_support),
                exit_support_pct = as.numeric(exit_support_pct),
                justice_responses = as.numeric(justice_responses),
                justice_responses_pct = as.numeric(justice_responses_pct),
                accountability = as.numeric(accountability),
                accountability_pct = as.numeric(accountability_pct),
                address_risk = as.numeric(address_risk),
                address_risk_pct = as.numeric(address_risk_pct),
                supply_chain = as.numeric(supply_chain),
                supply_chain_pct = as.numeric(supply_chain_pct),
                milestone_score = as.numeric(milestone_score),
                corruption = as.numeric(corruption),
                total_score = as.numeric(total_score),
                credit = ifelse(credit == "BBB*","BBB",credit),
                credit = factor(credit,
                                levels = rev(c("A","B","BB","BBB","C","CC","CCC","D")),
                                ordered = TRUE))
  

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

