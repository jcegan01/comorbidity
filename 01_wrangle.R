

######### setup ######### 
library(tidyverse)
setwd("/Users/Cegan/Documents/GitHub/comorbidity")
load("rda/comorbidity_dataset.rda")

#lists
l_dem <- c("age_group","age_years","race","state","sex","age","pregnant_yn")
l_hosp <- c("hosp_yn","adm1_dt","dis1_dt","icu_yn","icu_adm1_dt","icu_dis1_dt","mechvent_yn","mechvent_dur","death_yn")
l_comor <- c("autoimm_yn","cld_yn","cvd_yn","diabetes_yn","hypertension_yn","immsupp_yn","liverdis_yn","neuro_yn","obesity_yn","othercond_yn", "otherdis_yn","renaldis_yn","smoke_curr_yn")
l_co.comor <- paste("co.",l_comor,sep="")
l_all <- c(l_dem,l_hosp,l_comor)

#subset of comorbidity data
cmr <- comor[,which(colnames(comor) %in% c(l_dem,l_hosp,l_comor))]

#show count by state
count(cmr, state)
count(cmr, pregnant_yn)

#filter for only non-pregnant patients
a <- cmr %>%  filter(pregnant_yn == "Yes")
nrow(a)/nrow(cmr) #fraction to be removed that's pregnant - 0.5% of 1.2M
cmr <- cmr  %>% filter(pregnant_yn != "Yes")
nrow(cmr) #1.19M

#filter out missing age groups
a <- cmr %>%  filter(age_group == "")
nrow(a)/nrow(cmr) #fraction to be removed that's pregnant - 0.2% of 1.2M
cmr <- cmr  %>% filter(age_group != "")
nrow(cmr) #1.19M

######### comor count #########

#changes yes/no to 1,0 while preserving unknowns - comorbidities
cmr <- cmr %>% 
  mutate(co.autoimm_yn = ifelse(autoimm_yn == "Yes",1,ifelse(autoimm_yn == "No",0,NA))) %>%
  mutate(co.cld_yn = ifelse(cld_yn == "Yes",1,ifelse(cld_yn == "No",0,NA)))  %>% 
  mutate(co.diabetes_yn = ifelse(diabetes_yn == "Yes",1,ifelse(diabetes_yn == "No",0,NA)))  %>%
  mutate(co.renaldis_yn = ifelse(renaldis_yn == "Yes",1,ifelse(renaldis_yn == "No",0,NA)))  %>% 
  mutate(co.liverdis_yn = ifelse(liverdis_yn == "Yes",1,ifelse(liverdis_yn == "No",0,NA)))  %>%
  mutate(co.immsupp_yn = ifelse(immsupp_yn == "Yes",1,ifelse(immsupp_yn == "No",0,NA)))  %>%
  mutate(co.neuro_yn = ifelse(neuro_yn == "Yes",1,ifelse(neuro_yn == "No",0,NA)))  %>%
  mutate(co.hypertension_yn = ifelse(hypertension_yn == "Yes",1,ifelse(hypertension_yn == "No",0,NA)))  %>%   
  mutate(co.obesity_yn = ifelse(obesity_yn == "Yes",1,ifelse(obesity_yn == "No",0,NA)))  %>% 
  mutate(co.othercond_yn = ifelse(othercond_yn == "Yes",1,ifelse(othercond_yn == "No",0,NA)))  %>%
  mutate(co.cvd_yn = ifelse(cvd_yn == "Yes",1,ifelse(cvd_yn == "No",0,NA)))  %>%  
  mutate(co.otherdis_yn = ifelse(otherdis_yn == "Yes",1,ifelse(otherdis_yn == "No",0,NA)))  %>%    
  mutate(co.smoke_curr_yn = ifelse(smoke_curr_yn == "Yes",1,ifelse(smoke_curr_yn == "No",0,NA)))    

#changes yes/no to 1,0 while preserving unknowns - outcomes
cmr <- cmr %>% 
  mutate(death_yn = ifelse(death_yn == "Yes",1,ifelse(death_yn == "No",0,NA)))  %>%    
  mutate(icu_yn = ifelse(icu_yn == "Yes",1,ifelse(icu_yn == "No",0,NA)))  %>%  
  mutate(mechvent_yn = ifelse(mechvent_yn == "Yes",1,ifelse(mechvent_yn == "No",0,NA)))  %>%     
  mutate(hosp_yn = ifelse(hosp_yn == "Yes",1,ifelse(hosp_yn == "No",0,NA)))
  
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

#convert to 2 or more
cmr$comor_count[which(cmr$comor_count>2)] <- 2 
count(cmr, comor_count) 

#save rda subset
save(cmr,file = "rda/comorbidity_subset.rda")


