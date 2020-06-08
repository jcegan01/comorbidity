

#load global packages and variables
setwd("/Users/Cegan/Documents/GitHub/comorbidity")
source("code/00_global.R")

#load comorbidity dataset from RDA file
load(paste0("rda/comorbidity_dataset_",download_date,".rda"))

#individual lists
l_dem <- c("age_group","age_years","race","state","sex","age","pregnant_yn")
l_outcomes <- c("death_yn","hosp_yn","icu_yn","mechvent_yn")
l_hosptimes <- c("adm1_dt","dis1_dt","icu_adm1_dt","icu_dis1_dt","mechvent_dur")
l_comor <- c("autoimm_yn","cld_yn","cvd_yn","diabetes_yn","hypertension_yn","immsupp_yn","liverdis_yn","neuro_yn","obesity_yn","othercond_yn", "otherdis_yn","renaldis_yn","smoke_curr_yn")

#combined lists
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
response.01 <- paste("Pregnant fraction removed is",sprintf("%1.2f%%", 100*nrow(a)/nrow(cmr)),"of",formatC(nrow(cmr),big.mark = ","),"entries")
cmr <- cmr  %>% filter(pregnant_yn != "Yes")

#replace age groups
cmr <- cmr %>% mutate(age_group = ifelse(age <= 17,"0 - 17 Years",ifelse(age <= 29,"18 - 29 Years",age_group)))

#filter out missing age groups
a <- cmr %>%  filter(age_group == "")
response.02 <- paste("Missing age group fraction removed is",sprintf("%1.2f%%", 100*nrow(a)/nrow(cmr)),"of",formatC(nrow(cmr),big.mark = ","),"entries")
cmr <- cmr  %>% filter(age_group != "")


######### comor count #########

#changes yes/no to 1,0 while preserving unknowns - comorbidities
for(n in 1:length(l_comor)){
  var1 <- sym(l_co.comor[n])
  var2 <- sym(l_comor[n])
  cmr <- mutate(cmr,!!var1 := ifelse(!!var2 == "Yes",1,ifelse(!!var2 == "No",0,0))) #Unknowns to 0 for test
}

#changes yes/no to 1,0 while preserving unknowns - outcomes
for(n in 1:length(l_outcomes)){
  var <- sym(l_outcomes[n])
  cmr <- mutate(cmr,!!var := ifelse(!!var == "Yes",1,ifelse(!!var == "No",0,0))) #Unknowns to 0 for test 
}

#filter out cases with no comorbidity information 
a <- cmr %>%  filter_at(vars(starts_with("co.")), all_vars(is.na(.)))
response.03 <- paste("Missing comorbidity fraction removed is",sprintf("%1.2f%%", 100*nrow(a)/nrow(cmr)),"of",formatC(nrow(cmr),big.mark = ","),"entries")
cmr <- cmr  %>% filter_at(vars(starts_with("co.")), any_vars(!is.na(.)))

#define numerical columns to sum
l_comor_num <- which(colnames(cmr) %in% l_co.comor)

#sum columns, while ignoring NAs
cmr <- cmr %>% 
  mutate(comor_count = rowSums(.[names(.)[l_comor_num]], na.rm = TRUE))

#convert to "2+" category
cmr <- cmr %>% mutate(comor_count = ifelse(comor_count > 2,2,comor_count))

#change to character
cmr$comor_count <- as.character(cmr$comor_count)
cmr <- cmr %>% mutate(comor_count = ifelse(comor_count == "2","2+",comor_count))

#fraction of admittance and discharge dates missing
a <- cmr %>%  filter(adm1_dt == "")
response.04 <- paste("Missing admittance date fraction is",sprintf("%1.2f%%", 100*nrow(a)/nrow(cmr)),"of",formatC(nrow(cmr),big.mark = ","),"entries")
a <- cmr %>%  filter(dis1_dt == "")
response.05 <- paste("Missing discharge date fraction is",sprintf("%1.2f%%", 100*nrow(a)/nrow(cmr)),"of",formatC(nrow(cmr),big.mark = ","),"entries")

#comorbidity count
response.06 <- "Comorbidity count:"
response.07 <- count(cmr, comor_count) 

#print responses in record keeping log
l_response <- paste0("response.0",1:7)

time <- as.POSIXct(Sys.time(), tz="Europe/London")
time <- suppressWarnings(format(time,"%Y-%m-%d %H%M",tz="America/New_York",usetz=TRUE))
write.table(time, file = paste0("outputs/records/",time,".txt"),append=TRUE,row.names = FALSE,col.names = FALSE)

for(n in 1:length(l_response)){
  record <- print(get(l_response[[n]]))
  write.table(record, file = paste0("outputs/records/",time,".txt"),append=TRUE,row.names = FALSE,col.names = FALSE)
}

#save rda subset
save(cmr,file = paste0("rda/comorbidity_subset_",download_date,".rda"))
#remove(comor)
gc()

