

#load global packages and variables
setwd("/Users/Cegan/Documents/GitHub/comorbidity")
source("code/00_global.R")
library(naniar)

#load RDA subset for analysis
load("rda/comorbidity_subset.rda")


#requires rerun with all non-Yes's as NA
list <- c("co.diabetes_yn","co.cvd_yn","co.otherdis_yn","co.cld_yn","co.renaldis_yn","co.immsupp_yn","co.neuro_yn","co.smoke_curr_yn","co.liverdis_yn","co.hypertension_yn","co.autoimm_yn","co.obesity_yn","co.othercond_yn","co.renaldis_yn")
cmr_missing <- cmr %>% select(all_of(list))
#cmr_missing <- cmr[,which(colnames(cmr)%in% list)]
cmr_missing <- cmr_missing[order(cmr_missing$co.diabetes_yn, cmr_missing$co.cvd_yn, cmr_missing$co.otherdis_yn, cmr_missing$co.cld_yn, cmr_missing$co.renaldis_yn, cmr_missing$co.immsupp_yn, cmr_missing$co.neuro_yn, cmr_missing$co.smoke_curr_yn, cmr_missing$co.liverdis_yn,cmr_missing$co.hypertension_yn),]
tiff("figures/ComorY.tiff", units="in", width=12, height=5, res=300)
vis_miss(cmr_missing,warn_large_data=FALSE,)
dev.off()

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





#messing around
cmr_h <- cmr %>% filter(hosp_yn == 1) 

count(cmr, death_yn)
count(cmr, hosp_yn)
count(cmr, icu_yn)
count(cmr, mechvent_yn)


