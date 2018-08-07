##SETUP
library(dplyr)
library(ggplot2)

#cleanup environment
rm(list = ls())
gc()

#import folder structure
source("GlobalVariables.R")

##read in prepared file
for_sampling <- readRDS(file = paste(path_Data, "for_sampling.RDS", sep = "/"))

##remove when kick-off columns are fixed:
for_sampling <- for_sampling %>%
                  select(-starts_with("Kick-off_"))

##########################
## set aside Year 2017 for performance testing
for_perf_test <- for_sampling %>% 
                  filter(Year == 2017)

for_sampling <- for_sampling %>% 
                  filter(Year < 2017)

saveRDS(object = for_perf_test, file = paste(path_Data, "for_perf_test.RDS",   sep = "/"))
saveRDS(object = for_sampling,  file = paste(path_Data, "original_sample.RDS", sep = "/"))
##TODO: remove extra (e.g. Year) columns

##source: https://www.analyticsvidhya.com/blog/2017/03/imbalanced-classification-problem/
## Question: weighting?
## Question: de-noising/autoencoders?


##########################
##########################
## SAMPLING
injured_ind    <- which(for_sampling$injured == 1)
noninjured_ind <- which(for_sampling$injured == 0)

##########################
## 1. Random Under-Sampling
## source: https://stackoverflow.com/questions/48981550/what-is-the-best-way-of-under-sampling-in-r

sample_size <- length(injured_ind) * 4  #80% will be non-injured
pick_noninjured <- sample(x = noninjured_ind, size = sample_size, replace = FALSE)

under_sample <- for_sampling[c(injured_ind, pick_noninjured), ]

saveRDS(object = under_sample, file = paste(path_Data, "under_sample.RDS", sep = "/"))

##########################
## 2. Random Over-Sampling

sample_size <- length(noninjured_ind) * 0.25  #20% will be injured
pick_injured <- sample(x = injured_ind, size = sample_size, replace = TRUE)

over_sample <- for_sampling[c(pick_injured, noninjured_ind), ]

saveRDS(object = over_sample, file = paste(path_Data, "over_sample.RDS", sep = "/"))


##########################
## 3. Group-Based Over Sampling


##########################
## 4. Synthetic Minority Over-sampling Technique (SMOTE)


##########################
## 5. Modified synthetic minority oversampling technique (MSMOTE)