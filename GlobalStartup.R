##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Library Import
require(data.table)
require(tidyverse)
require(lubridate)
require(gridExtra)
require(caret)
require(DMwR) ## used for SMOTE
## library(ROSE) --> rose not working right now..
require(doParallel)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Cleanup environment
rm(list = ls())
gc()

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Import folder structure
source("GlobalVariables.R")

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Set saving of figures
save_plots <- F

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Import functions & themes
file.sources <- list.files(path        = path_Functions,
                           pattern     = "*.R$",
                           full.names  = TRUE, 
                           ignore.case = TRUE)

sapply(x = file.sources, FUN = source)