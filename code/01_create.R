

#load global packages and variables
setwd("/Users/Cegan/Documents/GitHub/comorbidity")
source("code/00_global.R")

#load raw data
library(data.table)
setwd("/Volumes/MY PASSPORT/comorbidity dataset/")
comor <- fread(paste0("Case+Only+Epi+TF+Dataset+%28no+PII%29_",download_date,".csv"), sep = ",", header= TRUE);

#save RDA
setwd("/Users/Cegan/Documents/GitHub/comorbidity")
save(comor,file = paste0("rda/comorbidity_dataset_",download_date,".rda"))

#barplot of state frequencies
top_states <- count(comor,state) %>% arrange(-n)
barplot(prop.table(table(comor$state[which(comor$state %in% c("LA",top_states$state[1:10]))])))