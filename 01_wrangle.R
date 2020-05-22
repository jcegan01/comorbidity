

######### setup ######### 
library(tidyverse)
setwd("/Users/Cegan/Documents/code/comorbidity")
load("rda/comorbidity_dataset.rda")

#lists
l_dem <- c("age_group","age_years","race","state","sex","age")
l_hosp <- c("hosp_yn","adm1_dt","dis1_dt","icu_yn","icu_adm1_dt","icu_dis1_dt","mechvent_yn","mechvent_dur","death_yn")
l_comor <- c("diabetes_yn","renaldis_yn","liverdis_yn","immsupp_yn","neuro_yn","hypertension_yn","obesity_yn","othercond_yn","cvd_yn","autoimm_yn")
l_co.comor <- paste("co.",l_comor,se)
l_all <- c(l_dem,l_hosp,l_comor)

#subset of comorbidity data
cmr <- comor[,which(colnames(comor) %in% c(l_dem,l_hosp,l_comor))]

#show count by state
count(cmr, state)

#show count by cvd_yn
# count(cmr, cvd_yn)  ##note: only 57K covid confirmed patients and 88K non-covid confirmed patients
# 
# #filter for only COVID positive patients
# a <- cmr %>%  filter(cvd_yn == "Yes")
# 1-(nrow(a)/nrow(cmr)) #fraction to be removed that's not covid positive - 95% of 1.2M
# cmr <- cmr  %>% filter(cvd_yn %in% c("Yes"))
# nrow(cmr) #57K


######### comor count #########

#changes yes/no to 1,0 while preserving unknowns
cmr <- cmr %>% 
  mutate(co.diabetes_yn = ifelse(diabetes_yn == "Yes",1,ifelse(diabetes_yn == "No",0,NA)))  %>%
  mutate(co.renaldis_yn = ifelse(renaldis_yn == "Yes",1,ifelse(renaldis_yn == "No",0,NA)))  %>% 
  mutate(co.liverdis_yn = ifelse(liverdis_yn == "Yes",1,ifelse(liverdis_yn == "No",0,NA)))  %>%
  mutate(co.immsupp_yn = ifelse(immsupp_yn == "Yes",1,ifelse(immsupp_yn == "No",0,NA)))  %>%
  mutate(co.neuro_yn = ifelse(neuro_yn == "Yes",1,ifelse(neuro_yn == "No",0,NA)))  %>%
  mutate(co.hypertension_yn = ifelse(hypertension_yn == "Yes",1,ifelse(hypertension_yn == "No",0,NA)))  %>%   
  mutate(co.obesity_yn = ifelse(obesity_yn == "Yes",1,ifelse(obesity_yn == "No",0,NA)))  %>% 
  mutate(co.othercond_yn = ifelse(othercond_yn == "Yes",1,ifelse(othercond_yn == "No",0,NA)))  %>%
  mutate(co.cvd_yn = ifelse(cvd_yn == "Yes",1,ifelse(cvd_yn == "No",0,NA)))  %>%  
  mutate(co.autoimm_yn = ifelse(autoimm_yn == "Yes",1,ifelse(autoimm_yn == "No",0,NA)))    

#filter out cases with no comorbidity information 
a <- cmr %>%  filter_at(vars(starts_with("co.")), all_vars(is.na(.)))
b <- cmr %>%  filter_at(vars(starts_with("co.")), any_vars(!is.na(.)))
nrow(a)/(nrow(b)+nrow(a)) #fraction to be removed due to no comorbidity data of 1.2M
cmr <- cmr  %>% filter_at(vars(starts_with("co.")), any_vars(!is.na(.)))
nrow(cmr) #188K

#reorder rows (no longer necessary with following column sum script, but nice to have)
cmr <- cmr %>%
  select(all_of(l_all), everything())

#define numerical columns to sum
l_comor_num <- which(colnames(cmr) %in% l_co.comor)

#sum columns, while ignoring NAs
cmr <- cmr %>% 
  mutate(comor_count = rowSums(.[names(.)[l_comor_num]], na.rm = TRUE))
count(cmr, comor_count) 

# # A tibble: 7 x 2
# comor_count      n
# <dbl>  <int>
# 1           0 110885
# 2           1  63118
# 3           2  12493
# 4           3   1816
# 5           4    226
# 6           5     29
# 7           6      1

#save rda subset
save(cmr,file = "rda/comorbidity_subset.rda")


