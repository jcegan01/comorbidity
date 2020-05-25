

######### setup ######### 
library(tidyverse)
setwd("/Users/Cegan/Documents/GitHub/comorbidity")
load("rda/comorbidity_subset.rda")


######### plots ######### 

counts <- table(cmr$age_group,cmr$comor_count)
barplot(counts)
count(cmr, age_group)
summary(cmr)




######### analysis ######### 

#total population in each group
cmr %>% group_by(age_group, comor_count) %>% tally() 

#overall table
res_overall <- cmr %>%
  group_by(age_group, comor_count) %>%
  mutate(count = n()) %>% 
  summarize(population = mean(count),
            hosp_rate = mean(hosp_yn, na.rm = TRUE),           
            icu_rate = mean(icu_yn, na.rm = TRUE),
            mechvent_rate = mean(mechvent_yn, na.rm = TRUE),
            death_rate = mean(death_yn, na.rm = TRUE)) 

#conditional if in HOSP
res_hosp <- cmr %>%
  group_by(age_group, comor_count) %>%
  filter(hosp_yn == 1) %>%
  mutate(count = n()) %>% 
  summarize(population = mean(count),
            hosp_rate = mean(hosp_yn, na.rm = TRUE),           
            icu_rate = mean(icu_yn, na.rm = TRUE),
            mechvent_rate = mean(mechvent_yn, na.rm = TRUE),
            death_rate = mean(death_yn, na.rm = TRUE)) 

#conditional if in ICU
res_icu <- cmr %>%
  group_by(age_group, comor_count) %>%
  filter(icu_yn == 1) %>%
  mutate(count = n()) %>% 
  summarize(population = mean(count),
            hosp_rate = mean(hosp_yn, na.rm = TRUE),           
            icu_rate = mean(icu_yn, na.rm = TRUE),
            mechvent_rate = mean(mechvent_yn, na.rm = TRUE),
            death_rate = mean(death_yn, na.rm = TRUE)) 

#conditional if Ventilated
res_vent <- cmr %>%
  group_by(age_group, comor_count) %>%
  filter(mechvent_yn == 1) %>%
  mutate(count = n()) %>% 
  summarize(population = mean(count),
            hosp_rate = mean(hosp_yn, na.rm = TRUE),           
            icu_rate = mean(icu_yn, na.rm = TRUE),
            mechvent_rate = mean(mechvent_yn, na.rm = TRUE),
            death_rate = mean(death_yn, na.rm = TRUE)) 


######### output ######### 

write.csv(res_overall, paste0("outputs/comorbidity_overall_",format(Sys.time(), "%Y-%m-%d"),".csv"),row.names = FALSE)
write.csv(res_hosp, paste0("outputs/comorbidity_hosp_",format(Sys.time(), "%Y-%m-%d"),".csv"),row.names = FALSE)
write.csv(res_icu, paste0("outputs/comorbidity_icu_",format(Sys.time(), "%Y-%m-%d"),".csv"),row.names = FALSE)
write.csv(res_vent, paste0("outputs/comorbidity_vent_",format(Sys.time(), "%Y-%m-%d"),".csv"),row.names = FALSE)

  
