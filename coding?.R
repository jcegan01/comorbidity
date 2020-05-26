#! GET FUNCTION AND [[]]
#print responses
l_response <- paste0("response.0",1:5)
for(n in 1:length(l_response)){
  print(get(l_response[[n]]))
}

cmr[,l_co.comor[[1]]]

#####
# how to convert to dplyr?
#####
cmr$comor_count[which(cmr$comor_count>2)] <- 2 

cmr <- cmr %>% mutate(hosp_yn = ifelse(icu_yn == "Yes","Yes",hosp_yn))
cmr$hosp_yn[which(cmr$icu_yn=="Yes")] <- "Yes"

#####
# how could i condense into single line or loop using dplyr?
# requires referencing object/column names from a list
# right now i have to duplicate the formula for each column i want to peform this on
# i know how to do this in base R, e.g., which((colnames(cmr) %in% list), but can't figure out for dplyr
#####

#list of comorbidity column names i want to alter
l_comor <- c("autoimm_yn","cld_yn","cvd_yn","diabetes_yn","hypertension_yn","immsupp_yn","liverdis_yn","neuro_yn","obesity_yn","othercond_yn", "otherdis_yn","renaldis_yn","smoke_curr_yn")

#current code using dplyr
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


#base R version of above code
for(n in 1:length(l_comor)){
  cmr[cmr[,which(colnames(cmr) == l_comor[n])] != "Yes" | cmr[,which(colnames(cmr) == l_comor[n])] != "No" ,which(colnames(cmr) == l_comor[n])] <- NaN
  cmr[cmr[,which(colnames(cmr) == l_comor[n])]=="Yes",which(colnames(cmr) == l_comor[n])] <- 1
  cmr[cmr[,which(colnames(cmr) == l_comor[n])]=="No",which(colnames(cmr) == l_comor[n])] <- 0
  names(cmr)[names(cmr)==l_comor[n]] <- l_co.comor[n] #rename comorbidity columns to have consistent "co" in front for later dplyr formulas
}
