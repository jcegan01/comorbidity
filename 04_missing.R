

#load global packages and variables
setwd("/Users/Cegan/Documents/GitHub/comorbidity")
source("00_global.R")
library(naniar)

#load RDA subset for analysis
load("rda/comorbidity_subset.rda")

#messing around
cmr_h <- cmr %>% filter(hosp_yn == 1) 

count(cmr, death_yn)
count(cmr, hosp_yn)
count(cmr, icu_yn)
count(cmr, mechvent_yn)


cmr  %>% summarize(avg = mean(death_yn, na.rm = TRUE))

82/(82+145)


#missing outcomes
cmr_missing <- cmr[,which(colnames(cmr)%in% l_outcomes)]
cmr_missing <- cmr_missing[order(cmr_missing$death_yn, cmr_missing$icu_yn, cmr_missing$mechvent_yn,cmr_missing$hosp_yn),]
vis_miss(cmr_missing,warn_large_data=FALSE,)

#missing ages
age <- c("age_group","age_years","age")
cmr_missing <- cmr[,which(colnames(cmr)%in% age)]
cmr_missing <- cmr_missing[order(cmr_missing$age_group, cmr_missing$age_years, cmr_missing$age),]
vis_miss(cmr_missing,warn_large_data=FALSE,)
