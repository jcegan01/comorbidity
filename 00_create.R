#WRANGLE DATA

library(tidyverse)
library(data.table)
setwd("/Users/Cegan/Documents/GitHub/comorbidity")
comor <- fread("data/Case+Only+Epi+TF+Dataset+%28no+PII%29.csv", sep = ",", header= TRUE);
save(comor,file = "rda/comorbidity_dataset.rda")

#barplot of state frequencies
top_states <- count(comor,state) %>% arrange(-n)
barplot(prop.table(table(comor$state[which(comor$state %in% c("LA",top_states$state[1:10]))])))

#Texas subset 
comor.tx <- comor[which(comor$state=="TX"),]
#write.csv(comor.tx, "data/comorbidity_TX.csv",row.names = FALSE)