

#load global packages and variables
setwd("/Users/Cegan/Documents/GitHub/comorbidity")
source("00_global.R")
library(naniar)

#load RDA subset for analysis
load("rda/comorbidity_subset.rda")

#missing ages
age <- c("age_group","age_years","age")
cmr_missing <- comor[,which(colnames(comor)%in% age)]
cmr_missing <- cmr_missing[order(cmr_missing$age_group, cmr_missing$age_years, cmr_missing$age),]
tiff("figures/missing_age.tiff", units="in", width=8, height=5, res=300)
  vis_miss(cmr_missing,warn_large_data=FALSE,)
dev.off()

#missing outcomes
cmr_missing <- cmr[,which(colnames(cmr)%in% l_outcomes)]
cmr_missing <- cmr_missing[order(cmr_missing$death_yn, cmr_missing$icu_yn, cmr_missing$mechvent_yn,cmr_missing$hosp_yn),]
tiff("figures/missing_outcomes.tiff", units="in", width=8, height=5, res=300)
  vis_miss(cmr_missing,warn_large_data=FALSE,)
dev.off()

#missing outcomes - death YES
cmr_missing <- cmr[,which(colnames(cmr)%in% l_outcomes)]
cmr_missing <- cmr_missing %>% filter(death_yn == 1) 
cmr_missing <- cmr_missing[order(cmr_missing$death_yn, cmr_missing$icu_yn, cmr_missing$mechvent_yn,cmr_missing$hosp_yn),]
tiff("figures/missing_outcomes_death-YES.tiff", units="in", width=8, height=5, res=300)
  vis_miss(cmr_missing,warn_large_data=FALSE,)
dev.off()

#missing outcomes - death NO
cmr_missing <- cmr[,which(colnames(cmr)%in% l_outcomes)]
cmr_missing <- cmr_missing %>% filter(death_yn == 0) 
cmr_missing <- cmr_missing[order(cmr_missing$death_yn, cmr_missing$icu_yn, cmr_missing$mechvent_yn,cmr_missing$hosp_yn),]
tiff("figures/missing_outcomes_death-NO.tiff", units="in", width=8, height=5, res=300)
  vis_miss(cmr_missing,warn_large_data=FALSE,)
dev.off()

#missing outcomes - hosp YES
cmr_missing <- cmr[,which(colnames(cmr)%in% l_outcomes)]
cmr_missing <- cmr_missing %>% filter(hosp_yn == 1) 
cmr_missing <- cmr_missing[order(cmr_missing$death_yn, cmr_missing$icu_yn, cmr_missing$mechvent_yn,cmr_missing$hosp_yn),]
tiff("figures/missing_outcomes_hosp-YES.tiff", units="in", width=8, height=5, res=300)
  vis_miss(cmr_missing,warn_large_data=FALSE,)
dev.off()

#missing outcomes - hosp NO
cmr_missing <- cmr[,which(colnames(cmr)%in% l_outcomes)]
cmr_missing <- cmr_missing %>% filter(hosp_yn == 0) 
cmr_missing <- cmr_missing[order(cmr_missing$death_yn, cmr_missing$icu_yn, cmr_missing$mechvent_yn,cmr_missing$hosp_yn),]
tiff("figures/missing_outcomes_hosp-NO.tiff", units="in", width=8, height=5, res=300)
  vis_miss(cmr_missing,warn_large_data=FALSE,)
dev.off()


#messing around
cmr_h <- cmr %>% filter(hosp_yn == 1) 

count(cmr, death_yn)
count(cmr, hosp_yn)
count(cmr, icu_yn)
count(cmr, mechvent_yn)


