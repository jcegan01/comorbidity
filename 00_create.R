#WRANGLE DATA

library(tidyverse)
library(data.table)
setwd("/Users/Cegan/Documents/GitHub/comorbidity")
comor <- fread("data/Case+Only+Epi+TF+Dataset+%28no+PII%29.csv", sep = ",", header= TRUE);
save(comor,file = "rda/comorbidity_dataset.rda")

#barplot of state frequencies
barplot(prop.table(table(comor$state)))

#Texas subset 
comor.tx <- comor[which(comor$state=="TX"),]
write.csv(comor.tx, "data/comorbidity_TX.csv",row.names = FALSE)