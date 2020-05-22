

######### setup ######### 
library(tidyverse)
setwd("/Users/Cegan/Documents/code/comorbidity")
load("rda/comorbidity_subset.rda")


######### analysis ######### 

counts <- table(cmr$age_group,cmr$comor_count)
barplot(counts)






#other organizing

#https://datacarpentry.org/R-genomics/04-dplyr.html#mutate
metadata %>%
  group_by(cit, clade) %>%
  summarize(mean_size = mean(genome_size, na.rm = TRUE),
            min_generation = min(generation))
