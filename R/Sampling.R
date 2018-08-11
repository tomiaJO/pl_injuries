## Library imports
require(tidyverse)
require(caret)


## cleanup environment
rm(list = ls())
gc()


## import folder structure
source("GlobalVariables.R")


## Set saving of figures
save_plots <- FALSE


## Import correlation matrix functions
source(paste(path_Functions, "correl_functions.R",        sep = "/"))


## Import ggplot themes
source(paste(path_Functions, "ggplot_themes.R",           sep = "/"))


## Import plotting functions
source(paste(path_Functions, "f_conditional_ggsave.R",    sep = "/"))
source(paste(path_Functions, "f_plot_cormat.R",           sep = "/"))


## Read in prepared file
original_sample <- readRDS(file = paste(path_Data, "original_sample.RDS", sep = "/"))
original_sample %>% View()

## Remove base encoding columns for categoricals:
original_sample <- original_sample %>%
                     select(-Position_Midfielder, 
                            -Foot_Right, 
                            -Month_Aug, 
                            -`Kick-off_15-16`, 
                            -Venue_London, 
                            -Weekday,
                            -Nationality_UK,
                            -Birth_UK)


## Correlation based removals
###################################
####  Check for correlations
## source: http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

## Among games played in season
p_correl_season_games <- original_sample %>% 
                          select(all_games_season, pl_games_season, non_pl_games_season) %>%
                          cor() %>%
                          round(2) %>%
                          f_reorder_cormat %>%
                          f_get_upper_tri %>% 
                          reshape2::melt(na.rm = TRUE) %>% 
                          f_plot_cormat()

f_conditional_ggsave(save = save_plots, 
                     p = p_correl_season_games, 
                     filepath = paste(path_Figures, "99. Correlation Matrix - Season games.jpeg", sep = "/"), 
                     w = 6, 
                     h = 7)

## dropping "all_" variables
original_sample <- original_sample %>%
                    select(-starts_with("all_"))

## Among PL games played variables
p_correl_pl_nonpl <- original_sample %>% 
                      select(pl_games_season, non_pl_games_season,
                             pl_games_28,     non_pl_games_28,
                             pl_games_14,     non_pl_min_14,
                             pl_games_4,      non_pl_games_4) %>%
                      cor() %>%
                      round(2) %>%
                      f_reorder_cormat %>%
                      f_get_upper_tri %>% 
                      reshape2::melt(na.rm = TRUE) %>% 
                      f_plot_cormat()

f_conditional_ggsave(save = save_plots, 
                     p = p_correl_pl_nonpl, 
                     filepath = paste(path_Figures, "99. Correlation Matrix - PL and Non-PL games.jpeg", sep = "/"), 
                     w = 6, 
                     h = 7)
## note: no too high correlations! many decorrelated items, great thing

## add polinomials
original_sample <- original_sample %>%
                    mutate(`Age (Years) - SQ`     = `Age (Years)` ^ 2,
                           `Height (cm) - SQ`     = `Height (cm)` ^ 2,
                           `Weight (kg) - SQ`     = `Weight (kg)` ^ 2,
                           `BMI - SQ`             = BMI ^ 2,
                           pl_games_season_sq     = pl_games_season ^ 2, 
                           non_pl_games_season_sq = non_pl_games_season ^ 2,
                           pl_games_28_sq         = pl_games_28 ^ 2,     
                           non_pl_games_28_sq     = non_pl_games_28 ^ 2,
                           pl_games_14_sq         = pl_games_14 ^ 2,     
                           non_pl_min_14_sq       = non_pl_min_14 ^ 2,
                           pl_games_4_sq          = pl_games_4 ^ 2,      
                           non_pl_games_4_sq      = non_pl_games_4 ^ 2)


##########################
## Target needs to be factor for classification with caret
original_sample$injured <- factor(original_sample$injured, levels = c("1", "0"), labels = c("Yes", "No"))

## set aside Year 2017 for performance testing
training_set    <- original_sample %>% 
                     filter(Year < 2017)

performance_set <- original_sample %>% 
                     filter(Year == 2017)


## remove variables should not be in the predictive model
training_set <- training_set %>%
                  select(-mid, -pid, -Year)

training_set <- training_set %>%
                  select(-injury_length)

#######################################################
## Create training & test sets
training_ratio <- 0.70

set.seed(93)
train_indices <- createDataPartition(y = training_set[["injured"]],
                                     times = 1,
                                     p = training_ratio,
                                     list = FALSE)

data_train <- training_set[train_indices,  ] %>% as.data.frame()
data_test  <- training_set[-train_indices, ] %>% as.data.frame()


#######################################################
## Save dfs
saveRDS(object = data_train,      file = paste(path_Data, "data_train.RDS",      sep = "/"))
saveRDS(object = data_test,       file = paste(path_Data, "data_test.RDS",       sep = "/"))
saveRDS(object = performance_set, file = paste(path_Data, "data_perfomance.RDS", sep = "/"))

data_train %>% View()
# ### NOT USED ###
# ## last check before take-off
# any(is.na(training_set))
# any(is.nan(training_set %>% as.matrix()))
# any(is.finite(training_set %>% as.matrix()))

# ##source: https://www.analyticsvidhya.com/blog/2017/03/imbalanced-classification-problem/
# ## Question: weighting?
# ## Question: de-noising/autoencoders?
# 
# 
# ##########################
# ##########################
# ## SAMPLING
# injured_ind    <- which(for_sampling$injured == 1)
# noninjured_ind <- which(for_sampling$injured == 0)
# 
# ##########################
# ## 1. Random Under-Sampling
# ## source: https://stackoverflow.com/questions/48981550/what-is-the-best-way-of-under-sampling-in-r
# 
# sample_size <- length(injured_ind) * 4  #80% will be non-injured
# pick_noninjured <- sample(x = noninjured_ind, size = sample_size, replace = FALSE)
# 
# under_sample <- for_sampling[c(injured_ind, pick_noninjured), ]
# 
# saveRDS(object = under_sample, file = paste(path_Data, "under_sample.RDS", sep = "/"))
# 
# ##########################
# ## 2. Random Over-Sampling
# 
# sample_size <- length(noninjured_ind) * 0.25  #20% will be injured
# pick_injured <- sample(x = injured_ind, size = sample_size, replace = TRUE)
# 
# over_sample <- for_sampling[c(pick_injured, noninjured_ind), ]
# 
# saveRDS(object = over_sample, file = paste(path_Data, "over_sample.RDS", sep = "/"))
# 
# 
# ##########################
# ## 3. Group-Based Over Sampling
# 
# 
# ##########################
# ## 4. Synthetic Minority Over-sampling Technique (SMOTE)
# 
# 
# ##########################
# ## 5. Modified synthetic minority oversampling technique (MSMOTE)