

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

#overall table
res<- cmr %>%
  group_by(age_group, comor_count) %>%
  mutate(count = n()) %>% 
  summarize(population = mean(count),
            death_rate = mean(death_yn, na.rm = TRUE),
            hosp_rate = mean(hosp_yn, na.rm = TRUE),           
            icu_rate = mean(icu_yn, na.rm = TRUE),
            mechvent_rate = mean(mechvent_yn, na.rm = TRUE)) 

cmr %>% group_by(age_group, comor_count) %>% tally() #total population in each group
  
#conditional if in HOSP
cmr %>%
  group_by(age_group, comor_count) %>%
  filter(hosp_yn == 1) %>%
  mutate(count = n()) %>% 
  summarize(population = mean(count),
            death_rate = mean(death_yn, na.rm = TRUE),
            hosp_rate = mean(hosp_yn, na.rm = TRUE),           
            icu_rate = mean(icu_yn, na.rm = TRUE),
            mechvent_rate = mean(mechvent_yn, na.rm = TRUE)) 

#conditional if in ICU
cmr %>%
  group_by(age_group, comor_count) %>%
  filter(icu_yn == 1) %>%
  mutate(count = n()) %>% 
  summarize(population = mean(count),
            death_rate = mean(death_yn, na.rm = TRUE),
            hosp_rate = mean(hosp_yn, na.rm = TRUE),           
            icu_rate = mean(icu_yn, na.rm = TRUE),
            mechvent_rate = mean(mechvent_yn, na.rm = TRUE)) 

#conditional if Ventilated
cmr %>%
  group_by(age_group, comor_count) %>%
  filter(mechvent_yn == 1) %>%
  mutate(count = n()) %>% 
  summarize(population = mean(count),
            death_rate = mean(death_yn, na.rm = TRUE),
            hosp_rate = mean(hosp_yn, na.rm = TRUE),           
            icu_rate = mean(icu_yn, na.rm = TRUE),
            mechvent_rate = mean(mechvent_yn, na.rm = TRUE)) 

######### output ######### 
write.csv(res, "outputs/comorbidity_results.csv",row.names = FALSE)
  
