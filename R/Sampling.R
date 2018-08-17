##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Initialize R setup
source("GlobalStartup.R")

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load Data
original_sample <- readRDS(file = paste(path_Data, "original_sample.RDS", sep = "/"))
#original_sample %>% View()

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## DROP base encoding columns for categoricals:
original_sample <- original_sample %>%
                     select(-Position_Midfielder, 
                            -Foot_Right, 
                            -Month_Aug, 
                            -`Kick-off_15-16`, 
                            -Venue_London, 
                            -Weekday,
                            -Nationality_UK,
                            -Birth_UK,
                            -Team_Chelsea,
                            -Opponent_Chelsea)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## DROP "all_" variables
original_sample <- original_sample %>%
                    select(-starts_with("all_"))

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## DROP "mins" variables
original_sample <- original_sample %>%
                    select(-starts_with("pl_min"), -starts_with("non_pl_min"))


##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## VISUALIZE correlations
## source: http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

## Among games played in season
# p_correl_season_games <- original_sample %>% 
#                           select(all_games_season, pl_games_season, non_pl_games_season) %>%
#                           cor() %>%
#                           round(2) %>%
#                           f_reorder_cormat %>%
#                           f_get_upper_tri %>% 
#                           reshape2::melt(na.rm = TRUE) %>% 
#                           f_plot_cormat()
# 
# f_conditional_ggsave(save = save_plots, 
#                      p = p_correl_season_games, 
#                      filepath = paste(path_Figures, "99. Correlation Matrix - Season games.jpeg", sep = "/"), 
#                      w = 6, 
#                      h = 7)



## Among PL games played variables
p_correl_pl_nonpl <- original_sample %>% 
                      select(pl_games_season,  non_pl_games_season,
                             pl_games_28,      non_pl_games_28,
                             pl_games_14,      non_pl_games_14,
                             pl_games_4,       non_pl_games_4,
                             pl_avgmin_season, non_pl_avgmin_season,
                             pl_avgmin_28,     non_pl_avgmin_28,
                             pl_avgmin_14,     non_pl_avgmin_14,
                             pl_avgmin_4,      non_pl_avgmin_4) %>%
                      cor() %>%
                      round(2) %>%
                      #f_reorder_cormat %>%
                      f_get_upper_tri %>% 
                      reshape2::melt(na.rm = TRUE) %>% 
                      f_plot_cormat()
## TODO: format cormat

f_conditional_ggsave(save = save_plots, 
                     p = p_correl_pl_nonpl, 
                     filepath = paste(path_Figures, "99. Correlation Matrix - PL and Non-PL games.jpeg", sep = "/"), 
                     w = 6, 
                     h = 7)
## note: no too high correlations! many decorrelated items, great thing

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ADD polinomials
# original_sample <- original_sample %>%
#                     mutate(`Age (Years) - SQ`     = `Age (Years)` ^ 2,
#                            `Height (cm) - SQ`     = `Height (cm)` ^ 2,
#                            `Weight (kg) - SQ`     = `Weight (kg)` ^ 2,
#                            `BMI - SQ`             = BMI ^ 2,
#                            pl_games_season_sq     = pl_games_season ^ 2, 
#                            non_pl_games_season_sq = non_pl_games_season ^ 2,
#                            pl_games_28_sq         = pl_games_28 ^ 2,     
#                            non_pl_games_28_sq     = non_pl_games_28 ^ 2,
#                            pl_games_14_sq         = pl_games_14 ^ 2,     
#                            non_pl_min_14_sq       = non_pl_min_14 ^ 2,
#                            pl_games_4_sq          = pl_games_4 ^ 2,      
#                            non_pl_games_4_sq      = non_pl_games_4 ^ 2)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Target needs to be factor for classification with caret
original_sample$injured        <- factor(original_sample$injured,        levels = c("1", "0"), labels = c("Yes", "No"))
original_sample$injured_7days  <- factor(original_sample$injured_7days,  levels = c("1", "0"), labels = c("Yes", "No"))
original_sample$injured_14days <- factor(original_sample$injured_14days, levels = c("1", "0"), labels = c("Yes", "No"))
original_sample$injured_21days <- factor(original_sample$injured_21days, levels = c("1", "0"), labels = c("Yes", "No"))
original_sample$injured_28days <- factor(original_sample$injured_28days, levels = c("1", "0"), labels = c("Yes", "No"))

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## set aside Year 2017 for performance testing
training_set    <- original_sample %>% 
                     filter(Year < 2017)

performance_set <- original_sample %>% 
                     filter(Year == 2017)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create training & test sets
training_ratio <- 0.70

## injured
set.seed(93)
train_indices <- createDataPartition(y = training_set[["injured"]],
                                     times = 1,
                                     p = training_ratio,
                                     list = FALSE)

data_train <- training_set[train_indices,  ] %>% 
                select(-injured_7days, -injured_14days, -injured_21days, -injured_28days) %>%
                select(-mid, -pid, -Year) %>%
                select(-injury_length) %>% 
                as.data.frame()

data_test  <- training_set[-train_indices, ] %>% as.data.frame()

## 7 days
set.seed(93)
train_indices <- createDataPartition(y = training_set[["injured_7days"]],
                                     times = 1,
                                     p = training_ratio,
                                     list = FALSE)

data_train_7 <- training_set[train_indices,  ] %>% 
                  select(-injured, -injured_14days, -injured_21days, -injured_28days) %>%
                  select(-mid, -pid, -Year) %>%
                  select(-injury_length) %>% 
                  as.data.frame()

data_test_7  <- training_set[-train_indices, ] %>% as.data.frame()

## 14 days
set.seed(93)
train_indices <- createDataPartition(y = training_set[["injured_14days"]],
                                     times = 1,
                                     p = training_ratio,
                                     list = FALSE)

data_train_14 <- training_set[train_indices,  ] %>% 
                  select(-mid, -pid, -Year) %>%
                  select(-injury_length) %>% 
                  as.data.frame()

data_test_14  <- training_set[-train_indices, ] %>% as.data.frame()

## 21 days
set.seed(93)
train_indices <- createDataPartition(y = training_set[["injured_21days"]],
                                     times = 1,
                                     p = training_ratio,
                                     list = FALSE)

data_train_21 <- training_set[train_indices,  ] %>% 
                  select(-mid, -pid, -Year) %>%
                  select(-injury_length) %>% 
                  as.data.frame()

data_test_21  <- training_set[-train_indices, ] %>% as.data.frame()

## 28 days
set.seed(93)
train_indices <- createDataPartition(y = training_set[["injured_28days"]],
                                     times = 1,
                                     p = training_ratio,
                                     list = FALSE)

data_train_28 <- training_set[train_indices,  ] %>% 
                  select(-mid, -pid, -Year) %>%
                  select(-injury_length) %>% 
                  as.data.frame()

data_test_28  <- training_set[-train_indices, ] %>% as.data.frame()


##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Save dfs
saveRDS(object = data_train,      file = paste(path_Data, "data_train.RDS",      sep = "/"))
saveRDS(object = data_test,       file = paste(path_Data, "data_test.RDS",       sep = "/"))
saveRDS(object = performance_set, file = paste(path_Data, "data_perfomance.RDS", sep = "/"))

saveRDS(object = data_train_7,    file = paste(path_Data, "data_train_7.RDS",    sep = "/"))
saveRDS(object = data_test_7,     file = paste(path_Data, "data_test_7.RDS",     sep = "/"))

saveRDS(object = data_train_14,   file = paste(path_Data, "data_train_14.RDS",   sep = "/"))
saveRDS(object = data_test_14,    file = paste(path_Data, "data_test_14.RDS",    sep = "/"))

saveRDS(object = data_train_21,   file = paste(path_Data, "data_train_21.RDS",   sep = "/"))
saveRDS(object = data_test_21,    file = paste(path_Data, "data_test_21.RDS",    sep = "/"))

saveRDS(object = data_train_28,   file = paste(path_Data, "data_train_28.RDS",   sep = "/"))
saveRDS(object = data_test_28,    file = paste(path_Data, "data_test_28.RDS",    sep = "/"))