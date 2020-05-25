

######### setup ######### 
library(tidyverse)
setwd("/Users/Cegan/Documents/GitHub/comorbidity")
load("rda/comorbidity_dataset.rda")

#individual lists
l_dem <- c("age_group","age_years","race","state","sex","age","pregnant_yn")
l_outcomes <- c("death_yn","icu_yn","mechvent_yn","hosp_yn")
l_hosptimes <- c("adm1_dt","dis1_dt","icu_adm1_dt","icu_dis1_dt","mechvent_dur")
l_comor <- c("autoimm_yn","cld_yn","cvd_yn","diabetes_yn","hypertension_yn","immsupp_yn","liverdis_yn","neuro_yn","obesity_yn","othercond_yn", "otherdis_yn","renaldis_yn","smoke_curr_yn")

#combined lists
l_binary <- c(l_outcomes,l_comor) #to be used in loop if possible to shorten dplyr equations
l_all <- c(l_dem,l_outcomes,l_hosptimes,l_comor)

#names for new comorbidity columns
l_co.comor <- paste("co.",l_comor,sep="")

#SUBSET OF COMORBIDITY DATA
cmr <- comor[,which(colnames(comor) %in% c(l_all))]

#reorder rows (nice to have)
cmr <- cmr %>%
  select(all_of(l_all), everything())

#force ICU as "Yes" if MECHVENT is "Yes"
cmr <- cmr %>% mutate(icu_yn = ifelse(mechvent_yn == "Yes","Yes",icu_yn))

#force HOSP as "Yes" if ICU is "Yes"
cmr <- cmr %>% mutate(hosp_yn = ifelse(icu_yn == "Yes","Yes",hosp_yn))

#filter for only non-pregnant patients
a <- cmr %>%  filter(pregnant_yn == "Yes")
print(paste("Pregnant fraction removed is",sprintf("%1.2f%%", 100*nrow(a)/nrow(cmr)),"of",formatC(nrow(cmr),big.mark = ","),"entries"))
cmr <- cmr  %>% filter(pregnant_yn != "Yes")

#filter out missing age groups
a <- cmr %>%  filter(age_group == "")
print(paste("Missing age group fraction removed is",sprintf("%1.2f%%", 100*nrow(a)/nrow(cmr)),"of",formatC(nrow(cmr),big.mark = ","),"entries"))
cmr <- cmr  %>% filter(age_group != "")

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
print(paste("Missing comorbidity fraction removed is",sprintf("%1.2f%%", 100*nrow(a)/nrow(cmr)),"of",formatC(nrow(cmr),big.mark = ","),"entries"))
cmr <- cmr  %>% filter_at(vars(starts_with("co.")), any_vars(!is.na(.)))

#define numerical columns to sum
l_comor_num <- which(colnames(cmr) %in% l_co.comor)

#sum columns, while ignoring NAs
cmr <- cmr %>% 
  mutate(comor_count = rowSums(.[names(.)[l_comor_num]], na.rm = TRUE))
count(cmr, comor_count) 

#convert >2 to 2, for later labeling of "2+"
cmr$comor_count[which(cmr$comor_count>2)] <- 2 
count(cmr, comor_count) 

# comor_count     n
# <dbl> <int>
# 1           0 87829
# 2           1 76452
# 3           2 56389

#save rda subset
save(cmr,file = "rda/comorbidity_subset.rda")
remove(comor)
gc()

