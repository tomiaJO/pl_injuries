##SETUP
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lubridate)

#cleanup environment
rm(list = ls())
gc()

#import folder structure
source("GlobalVariables.R")

#set saving of figures
save_plots <- TRUE

injuries_raw <- fread(paste(path_RawData, "injury_data_pg.csv", sep = "/")) 

#import EDA functions
source(paste(path_Functions, "f_sec_scale.R",             sep = "/"))
source(paste(path_Functions, "f_breakdown_by_bins.R",     sep = "/"))
source(paste(path_Functions, "f_breakdown_by_variable.R", sep = "/"))
source(paste(path_Functions, "f_calculate_ci.R",          sep = "/"))
source(paste(path_Functions, "f_conditional_ggsave.R",    sep = "/"))

#import ggplot themes
source(paste(path_Functions, "ggplot_themes.R",           sep = "/"))

#import correlation matrix functions
source(paste(path_Functions, "correl_functions.R",        sep = "/"))

##Formatting clean-up for date variables
injuries <- injuries_raw %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d"),
                     `Date of birth` = as.Date(`Date of birth`, "%Y-%m-%d")) %>%
              mutate(`Age (Years)` = time_length(difftime(Date, `Date of birth`), "years")) 


##Add Month
Month_mapping <- data.frame(Month_Num = c(1:12), Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                            Month_in_Season = c(6, 7, 8, 9, 10, 11, 12, 1, 2, 3, 4, 5))

injuries <- injuries %>%
              mutate(Month_Num = month(Date)) %>%
              left_join(Month_mapping, by = "Month_Num") %>%
              mutate(Month = reorder(Month, Month_in_Season)) %>%
              select(-Month_Num, -Month_in_Season)

##Add Year
injuries <- injuries %>%
              mutate(Year = year(Date))


##Fix formatting: for height & weight
injuries <- injuries %>%
              filter(!(is.null(Height) | Height == "")) %>%
              mutate(`Height (cm)` = gsub(pattern = " cm", replacement = "", x = Height)) %>%
              mutate(`Height (cm)` = as.numeric(`Height (cm)`)) %>%
              select(everything(), -Height)

injuries <- injuries %>%
              filter(!(is.null(Weight) | Weight == "")) %>%
              mutate(`Weight (kg)` = gsub(pattern = " kg", replacement = "", x = Weight)) %>%
              mutate(`Weight (kg)` = as.numeric(`Weight (kg)`)) %>%
              select(everything(), -Weight)


##Add BMI column:
injuries <- injuries %>%
  mutate(BMI = `Weight (kg)` / (`Height (cm)` / 100)^2)


##Visualize: Injury trend over years
p_injury_trend <- injuries %>%
                    group_by(Year) %>%
                    summarize(game_count = n(),
                              injury_count = (sum(injury_type != 0))) %>%
                    ungroup() %>%
                    mutate(injury_rate_pct = injury_count / game_count * 100) %>%
                    f_breakdown_by_bins(s_x = "Year", 
                                        s_title = "Injury data is missing pre 2010",
                                        s_subtitle = "Number of games, injury rate by year") +
                    story_theme()

f_conditional_ggsave(save = save_plots, 
                     p = p_injury_trend, 
                     filepath = paste(path_Figures, "1. Injury Trend Over Years.jpeg", sep = "/"), 
                     w = 6, 
                     h = 4.5)


##Drop years with no injury data
injuries <- injuries %>%
              filter(Year %in% c(2010:2017))


##Visualize: Injury type counts
p_injury_types <- injuries %>%
                    filter(injury_type != '0') %>%
                    count(injury_type, sort = T) %>%
                    mutate(injury_type = ifelse(n < 25, 'Other', injury_type)) %>%
                    mutate(injury_type = reorder(injury_type, n)) %>%
                    group_by(injury_type) %>%
                    summarize(n = sum(n)) %>%
                    ungroup() %>%
                    ggplot(aes(x = injury_type, y = n)) +
                    geom_bar(stat = "identity", fill = "steelblue2") +
                    geom_text(aes(label = n), hjust = 1.05, vjust = 0.325, fontface = "bold") +
                    coord_flip() + 
                    labs(title    = "The most common injury is to the hamstring",
                         subtitle = "Number of injuries by type",
                         x        = "Injury Type",
                         y        = "Count of injuries",
                         caption  = "Note: Injuries that happened less than 25x are group as 'Other'") +
                    story_theme()

f_conditional_ggsave(save = save_plots, 
                     p = p_injury_types, 
                     filepath = paste(path_Figures, "2. Injury Types.jpeg", sep = "/"), 
                     w = 6, 
                     h = 4.5)


##Visualize: injury lengths
p_injury_lenghts <- injuries %>%
                      filter(injury_type != '0') %>%
                      group_by(injury_type) %>%
                      mutate(n = n()) %>%
                      ungroup() %>%
                      mutate(injury_type = ifelse(n < 25, 'Other', injury_type)) %>%
                      mutate(injury_type = reorder(injury_type, n)) %>%
                      mutate(injury_length_capped_180 = ifelse(injury_length > 180, 180, injury_length)) %>%
                      ggplot(aes(x = injury_type, y = injury_length_capped_180)) + 
                      geom_boxplot() +
                      coord_flip() +
                      labs(title    = "There is significant variation among seriousness of injuries",
                           subtitle = "Length of injuries by type",
                           x        = "Injury Type",
                           y        = "Length of injuries",
                           caption  = "Note: Injuries that happened less than 25x are group as 'Other'") +
                      technical_theme()

f_conditional_ggsave(save = save_plots, 
                     p = p_injury_lenghts, 
                     filepath = paste(path_Figures, "3. Injury Lengths.jpeg", sep = "/"), 
                     w = 7, 
                     h = 4.5)

## Hamstring zoom:
injuries %>%
  filter(injury_type == "Hamstring") %>%
  filter(injury_length > 1) %>%
  summarize(median_injury_length = median(injury_length)) 

injuries %>%
  filter(injury_type == "Hamstring") %>%
  filter(injury_length > 1) %>%
  summarize(perc99_injury_length = quantile(injury_length, .95)) 

p_hamstring_lenghts <- injuries %>%
                        filter(injury_type == "Hamstring") %>%
                        filter(injury_length > 1) %>% #should miss at least 1 day
                        mutate(injury_length_capped_180 = ifelse(injury_length >= 180, 180, injury_length)) %>% 
                        ggplot(aes(x = injury_length_capped_180)) +
                        geom_density(fill = "steelblue2") +
                        geom_vline(xintercept = 18, color = "firebrick") +
                        geom_text(aes(x = 18, y = 0.03), label = "Median: 18 days", hjust = -0.1, vjust = 0, color = "firebrick") +
                        geom_vline(xintercept = 78, color = "firebrick") +
                        geom_text(aes(x = 78, y = 0.02), label = ".95 percentile: 78 days", hjust = -0.1, vjust = 0, color = "firebrick") +
                        labs(title    = "Typical injuries last 1 to 11 weeks",
                             subtitle = "Density plot, Length of injuries",
                             x        = "Injury Length (days)",
                             y        = "Density",
                             caption  = "Note: Injury length was capped at 180 days") + 
                        theme_minimal() +
                        theme(legend.position = "bottom") +
                        story_theme()

f_conditional_ggsave(save = save_plots, 
                     p = p_hamstring_lenghts, 
                     filepath = paste(path_Figures, "4. Hamstring Injury Lengths.jpeg", sep = "/"), 
                     w = 6, 
                     h = 4.5)


##Keep only hamstring
hamstring_only <- injuries %>% 
                    filter(injury_type == "0" | injury_type == "Hamstring")


##Keep only injuries with "significant" missed time
hamstring_only <- hamstring_only %>%
                    filter(!(injury_type == "Hamstring" & injury_length < 7))


## REMOVE: minimal playing time
hamstring_only <- hamstring_only %>%
                    filter(!is.na(pl_mins_season)) %>%
                    filter(!is.na(all_mins_season))

season_min_cutoff <- 180

tmp <- hamstring_only %>%
        group_by(pl_mins_season < season_min_cutoff) %>%
        summarize(game_count = n(),
                  injury_count = sum(injured),
                  injury_rate_pct = sum(injured) / n(),
                  ci95_lower_injury_rate_pct = f_calculate_ci(injury_rate_pct, game_count, 0.95)$lower,
                  ci95_upper_injury_rate_pct = f_calculate_ci(injury_rate_pct, game_count, 0.95)$upper) %>%
        ungroup()

keep_injury_rate_pct <- formatC(tmp[tmp$`pl_mins_season < season_min_cutoff` == FALSE, ]$injury_rate_pct * 100, digits = 2, format = "f")
cut_injury_rate_pct  <- formatC(tmp[tmp$`pl_mins_season < season_min_cutoff` == TRUE,  ]$injury_rate_pct * 100, digits = 2, format = "f")

p_by_minutes <- hamstring_only %>%
                  ggplot(aes(x = pl_mins_season)) +
                    geom_density(fill = "steelblue2", color = "steelblue2") 

d <- ggplot_build(p_by_minutes)$data[[1]]
ypos_line <- round(d$ymax %>% max() * 1.05, digits = 4)
xpos_line <- d$x %>% max() 

p_by_minutes <- p_by_minutes +
                  geom_segment(aes(x = 0, y = ypos_line, xend = season_min_cutoff, yend = ypos_line), color = "firebrick") +
                  geom_segment(aes(x = season_min_cutoff, y = ypos_line, xend = xpos_line, yend = ypos_line), color = "steelblue2") 

p_by_minutes <- p_by_minutes +
                  geom_point(aes(x = season_min_cutoff / 2, y = ypos_line), 
                             color = "firebrick", size = 5) +
                  geom_text(aes(x = season_min_cutoff / 2, y = ypos_line), 
                            label = cut_injury_rate_pct, color = "white", size = 2.5) +
                  geom_point(aes(x = (xpos_line - season_min_cutoff) / 2 + season_min_cutoff, y = ypos_line), 
                             color = "steelblue2", size = 5) +
                  geom_text(aes(x = (xpos_line - season_min_cutoff) / 2 + season_min_cutoff, y = ypos_line), 
                            label = keep_injury_rate_pct, color = "white", size = 2.5) 

p_by_minutes <- p_by_minutes +
                  geom_area(data = subset(d, x < season_min_cutoff), aes(x = x, y = y), fill = "firebrick", color = "firebrick") +
                  scale_x_continuous(labels = scales::comma) +
                  scale_y_continuous(labels = scales::comma) +
                  labs(y = "Density",
                       x = "Minutes played in the PL season",
                       title = "Minutes",
                       subtitle = "s_subtitle",
                       caption = "s_caption") +
                  story_theme()

f_conditional_ggsave(save = save_plots, 
                     p = p_by_minutes, 
                     filepath = paste(path_Figures, "5. Hamstring Injury vs PL season minutes.jpeg", sep = "/"), 
                     w = 6, 
                     h = 4.5)

## REMOVE: goalkeepers & missing position
p_by_position1 <- hamstring_only %>%
                  mutate(Position = ifelse(Position == "", "Missing", Position)) %>%
                  group_by(Position) %>%
                  summarize(game_count = n()) %>%
                  ungroup() %>%
                  mutate(Position = reorder(Position, game_count)) %>%
                  ggplot(aes(x = Position, y = game_count)) +
                    geom_bar(stat = "identity", fill = "steelblue2") +
                    scale_y_continuous(labels = scales::comma) +
                    coord_flip() + 
                    labs(y = "# of games",
                         x = "Position",
                         title = "Positional breakdown of games played and injury rates",
                         subtitle = "s_subtitle",
                         caption = "s_caption") +
                    story_theme()

f_conditional_ggsave(save = save_plots, 
                     p = p_by_position1, 
                     filepath = paste(path_Figures, "6a. Hamstring Injury vs Position.jpeg", sep = "/"), 
                     w = 4, 
                     h = 4.5)


p_by_position2 <- hamstring_only %>%
                    mutate(Position = ifelse(Position == "", "Missing", Position)) %>%
                    group_by(Position) %>%
                    summarize(game_count = n(),
                              injury_count = sum(injured),
                              injury_rate_pct = injury_count / game_count,
                              ci95_lower_injury_rate_pct = f_calculate_ci(injury_rate_pct, game_count, 0.95)$lower,
                              ci95_upper_injury_rate_pct = f_calculate_ci(injury_rate_pct, game_count, 0.95)$upper) %>%
                    ungroup() %>%
                    mutate(injury_rate_pct            = formatC(injury_rate_pct * 100, digits = 2, format = "f"),
                           ci95_lower_injury_rate_pct = formatC(ci95_lower_injury_rate_pct * 100, digits = 2, format = "f"),
                           ci95_upper_injury_rate_pct = formatC(ci95_upper_injury_rate_pct * 100, digits = 2, format = "f")) %>%
                    mutate(Position = reorder(Position, game_count)) %>%
                    ggplot(aes(x = Position, y = injury_rate_pct, label = injury_rate_pct)) + 
                      geom_point(stat='identity', color = "steelblue2", size = 6) +
                      geom_segment(aes(y = ci95_lower_injury_rate_pct, 
                                       yend = ci95_upper_injury_rate_pct, 
                                       x = Position, 
                                       xend = Position), 
                                   color = "steelblue2") +
                      geom_text(color = "white", size = 2) +
                      coord_flip() + 
                      labs(y = "# of games",
                           x = "Position",
                           title = "Positional breakdown of games played and injury rates",
                           subtitle = "s_subtitle",
                           caption = "s_caption") +
                      story_theme()


f_conditional_ggsave(save = save_plots, 
                     p = p_by_position2, 
                     filepath = paste(path_Figures, "6b. Hamstring Injury vs Position.jpeg", sep = "/"), 
                     w = 6, 
                     h = 4.5)

##TODO: finalize formatting, grid

hamstring_only <- hamstring_only %>%
                    #Note1 - Goalkeepers: very different dynamics, as shown by the Injury rate %
                    filter(Position != "Goalkeeper") %>%
                    #Note2 - Missing: don't trust that data very much
                    filter(Position != "")


##Visualize: Injury vs Minutes played in a given game - less time, less chance to get injured?
p_minutes_played <- hamstring_only %>%
                      mutate(`Minutes played` = cut(minutes, 
                                                    breaks = c(0, 15, 30, 45, 60, 75,  Inf), 
                                                    include.lowest = T,
                                                    labels = c("-15", "15-30", "30-45", "45-60", "60-75", "75+"))) %>%
                      group_by(`Minutes played`) %>%
                      summarize(game_count = n(),
                                injury_count = (sum(injury_type != 0))) %>%
                      ungroup() %>%
                      mutate(injury_rate_pct = injury_count / game_count * 100) %>%
                      f_breakdown_by_bins(s_title = "When hamstring injuries happen during the game",
                                          s_x = "`Minutes played`")

f_conditional_ggsave(save = save_plots, 
                     p = p_minutes_played, 
                     filepath = paste(path_Figures, "7. Hamstring Injury vs Minutes played.jpeg", sep = "/"), 
                     w = 6, 
                     h = 4.5)


##Visualize: Injury vs Age
p_hamstring_vs_age <- hamstring_only %>%
                        mutate(`Age (Years)` = cut(`Age (Years)``, 
                                                   breaks = c(-Inf, 20, 22.5, 25, 27.5, 30, 32.5, 35, Inf), 
                                                   include.lowest = T)) %>%
                        group_by(`Age (Years)`) %>%
                        summarize(game_count = n(),
                                  injury_count = (sum(injury_type != 0))) %>%
                        ungroup() %>%
                        mutate(injury_rate_pct = injury_count / game_count * 100) %>%
                        f_breakdown_by_bins(s_title = "Age vs Injury rates",
                                            s_x = "`Age (Years)`") +
                        story_theme()

f_conditional_ggsave(save = save_plots, 
                     p = p_hamstring_vs_age, 
                     filepath = paste(path_Figures, "8. Hamstring Injury vs Age.jpeg", sep = "/"), 
                     w = 6, 
                     h = 4.5)


##Weight
p_hamstring_vs_weight <- hamstring_only %>%
                          mutate(`Weight (kg)` = cut(`Weight (kg)`, 
                                                     breaks = c(-Inf, 60, 65, 70, 75, 80, 85, 90, Inf), 
                                                     include.lowest = T)) %>%
                          group_by(`Weight (kg)`) %>%
                          summarize(game_count = n(),
                                    injury_count = (sum(injury_type != 0))) %>%
                          ungroup() %>%
                          mutate(injury_rate_pct = injury_count / game_count * 100) %>%
                          f_breakdown_by_bins(s_title = "Weight vs Injury rates",
                                              s_x = "`Weight (kg)`") +
                          story_theme()

f_conditional_ggsave(save = save_plots, 
                     p = p_hamstring_vs_weight, 
                     filepath = paste(path_Figures, "9. Hamstring Injury vs Weight.jpeg", sep = "/"), 
                     w = 6, 
                     h = 4.5)


##Height:
p_hamstring_vs_height <- hamstring_only %>%
                          mutate(`Height (cm)` = cut(`Height (cm)`, 
                                                     breaks = seq(0, 220, by = 5), 
                                                     include.lowest = T)) %>%
                          group_by(`Height (cm)`) %>%
                          summarize(game_count = n(),
                                    injury_count = (sum(injury_type != 0))) %>%
                          ungroup() %>%
                          mutate(injury_rate_pct = injury_count / game_count * 100) %>%
                          f_breakdown_by_bins(s_title = "Height vs Injury rates",
                                              s_x = "`Height (cm)`") +
                          story_theme()

f_conditional_ggsave(save = save_plots, 
                     p = p_hamstring_vs_height, 
                     filepath = paste(path_Figures, "10. Hamstring Injury vs Height.jpeg", sep = "/"), 
                     w = 6, 
                     h = 4.5)


## BMI:
p_hamstring_vs_bmi <- hamstring_only %>%
                        mutate(BMI = cut(BMI, 
                                         breaks = c(-Inf, 20, 21, 22, 23, 24, 25, 26, 27, Inf), 
                                         include.lowest = T)) %>%
                        group_by(BMI) %>%
                        summarize(game_count = n(),
                                  injury_count = (sum(injury_type != 0))) %>%
                        ungroup() %>%
                        mutate(injury_rate_pct = injury_count / game_count * 100) %>%
                        f_breakdown_by_bins(s_title = "BMI vs Injury rates",
                                            s_x = "BMI") +
                        story_theme()

f_conditional_ggsave(save = save_plots, 
                     p = p_hamstring_vs_bmi, 
                     filepath = paste(path_Figures, "11. Hamstring Injury vs BMI.jpeg", sep = "/"), 
                     w = 6, 
                     h = 4.5)


##By Kick-off time  
p_hamstring_vs_kickoff <- hamstring_only %>%
                            mutate(kick_off_hour = as.numeric(substr(`Kick-off`, 1, 2)) + as.numeric(as.numeric(substr(`Kick-off`, 4, 5)) > 30)) %>%
                            mutate(`Kick-off Hour` = cut(kick_off_hour, breaks = c(0, 15, 17, 19, 24), 
                                                         include.lowest = T, labels = c("- 15.00", "15-17", "17-19", "19:00 -"))) %>%
                            group_by(`Kick-off Hour`) %>%
                            summarize(game_count = n(),
                                      injury_count = (sum(injury_type != 0))) %>%
                            ungroup() %>%
                            mutate(injury_rate_pct = injury_count / game_count * 100) %>%
                            f_breakdown_by_bins(s_title = "Kick-off time vs Injury rates",
                                                s_x = "`Kick-off Hour`") +
                            story_theme()

f_conditional_ggsave(save = save_plots, 
                     p = p_hamstring_vs_kickoff, 
                     filepath = paste(path_Figures, "12. Hamstring Injury vs Kick-off time.jpeg", sep = "/"), 
                     w = 6, 
                     h = 4.5)


##By Year
p_hamstring_vs_year1 <- hamstring_only %>%
                          group_by(Year) %>%
                          summarize(game_count = n(),
                                    injury_count = (sum(injury_type != 0))) %>%
                          ungroup() %>%
                          mutate(injury_rate_pct = injury_count / game_count * 100) %>%
                          f_breakdown_by_bins(s_title = "Hamstring Injury vs Year",
                                              s_subtitle = "Number of games, injury rate by year",
                                              s_x = "Year") +
                          story_theme()

f_conditional_ggsave(save = save_plots, 
                     p = p_hamstring_vs_year1, 
                     filepath = paste(path_Figures, "13. Hamstring Injury vs Year.jpeg", sep = "/"), 
                     w = 6, 
                     h = 4.5)

##with confidence intervals
p_hamstring_vs_year2 <- hamstring_only %>%
                          group_by(Year) %>%
                          summarize(game_count = n(),
                                    injury_count = (sum(injury_type != 0))) %>%
                          ungroup() %>%
                          mutate(injury_rate_pct = injury_count / game_count) %>%
                          mutate(ci95_lower_injury_rate_pct = f_calculate_ci(injury_rate_pct, game_count, 0.95)$lower,
                                 ci95_upper_injury_rate_pct = f_calculate_ci(injury_rate_pct, game_count, 0.95)$upper) %>%
                          mutate(injury_rate_pct            = injury_rate_pct            * 100,
                                 ci95_lower_injury_rate_pct = ci95_lower_injury_rate_pct * 100,
                                 ci95_upper_injury_rate_pct = ci95_upper_injury_rate_pct * 100) %>%
                          ggplot(aes(x = Year, y = injury_rate_pct, group = 1)) +
                          geom_line() +
                          geom_errorbar(aes(ymin = ci95_lower_injury_rate_pct, ymax = ci95_upper_injury_rate_pct), 
                                        colour = "black", 
                                        width = .4) +
                          geom_point(aes(size = game_count), shape = 21, fill = "white") +
                          scale_y_continuous(limits = c(0, 1.2)) +
                          labs(y = "Injury rate (in %)", 
                               x = "Year", 
                               title = "Hamstring Injury vs Year", 
                               subtitle = "Injury rate by year, with 95% confidence intervals",
                               caption = "Note: Bubble sizes represent # of games in each category") +
                          technical_theme()

f_conditional_ggsave(save = save_plots, 
                     p = p_hamstring_vs_year2, 
                     filepath = paste(path_Figures, "13b. Hamstring Injury vs Year.jpeg", sep = "/"), 
                     w = 6, 
                     h = 4.5)

##By Month --> visualize by year as well
p_hamstring_vs_month <- hamstring_only %>%
                          group_by(Month) %>%
                          summarize(game_count = n(),
                                    injury_count = (sum(injury_type != 0))) %>%
                          ungroup() %>%
                          mutate(injury_rate_pct = injury_count / game_count * 100) %>%
                          f_breakdown_by_bins(s_title = "Hamstring Injury vs Month",
                                              s_subtitle = "Number of games, injury rate by month",
                                              s_x = "Month") +
                          story_theme()

f_conditional_ggsave(save = save_plots, 
                     p = p_hamstring_vs_month, 
                     filepath = paste(path_Figures, "14. Hamstring Injury vs Month.jpeg", sep = "/"), 
                     w = 6, 
                     h = 4.5)


## Now with confidence intervals
p_hamstring_vs_month_w_conf <- hamstring_only %>%
                                group_by(Month) %>%
                                summarize(game_count = n(),
                                          injury_count = (sum(injury_type != 0))) %>%
                                ungroup() %>%
                                mutate(injury_rate_pct = injury_count / game_count) %>%
                                mutate(ci95_lower_injury_rate_pct = f_calculate_ci(injury_rate_pct, game_count, 0.95)$lower,
                                       ci95_upper_injury_rate_pct = f_calculate_ci(injury_rate_pct, game_count, 0.95)$upper) %>%
                                mutate(injury_rate_pct            = injury_rate_pct            * 100,
                                       ci95_lower_injury_rate_pct = ci95_lower_injury_rate_pct * 100,
                                       ci95_upper_injury_rate_pct = ci95_upper_injury_rate_pct * 100) %>%
                                ggplot(aes(x = Month, y = injury_rate_pct, group = 1)) +
                                  geom_line() +
                                  geom_errorbar(aes(ymin = ci95_lower_injury_rate_pct, ymax = ci95_upper_injury_rate_pct), 
                                                colour = "black", 
                                                width = .4) +
                                  geom_point(aes(size = game_count), shape = 21, fill = "white") +
                                labs(y = "Injury rate (in %)", 
                                     x = "Month", 
                                     title = "Hamstring Injury vs Month", 
                                     subtitle = "Injury rate by month, with 95% confidence intervals",
                                     caption = "Note: Bubble sizes represent # of games in each category") +
                                technical_theme()

f_conditional_ggsave(save = save_plots, 
                     p = p_hamstring_vs_month_w_conf, 
                     filepath = paste(path_Figures, "14b. Hamstring Injury vs Month.jpeg", sep = "/"), 
                     w = 6, 
                     h = 4.5)


## Breaken out by year -- NOT FINISHED
hamstring_only %>%
  mutate(Month_Num = month(Date)) %>%
  left_join(Month_mapping, by = "Month_Num") %>%
  mutate(Month = reorder(Month, Month_in_Season)) %>%
  
  group_by(Month) %>%
  summarize(game_count = n(),
            injury_count = (sum(injury_type != 0))) %>%
  ungroup() %>%
  mutate(injury_rate_pct = injury_count / game_count * 100) %>%
  f_breakdown_by_bins(s_title = "Hamstring Injury vs Month",
                      s_subtitle = "Number of games, injury rate by month",
                      s_x = "Month")


## By Venue
p_hamstring_vs_venue <- hamstring_only %>%
                          mutate(home = factor(home, levels = c(1, 0), labels = c("Home", "Away"))) %>%
                          group_by(home, Venue) %>%
                          summarize(game_count = n(),
                                    injury_count = sum(injured)) %>%
                          ungroup() %>%
                          mutate(Venue = ifelse(game_count < 1250, "Other", Venue)) %>%
                          mutate(Venue = reorder(Venue, -game_count)) %>%
                          group_by(home, Venue) %>%
                          summarize(game_count = sum(game_count),
                                    injury_count = sum(injury_count),
                                    injury_rate_pct = injury_count / game_count * 100) %>%
                          ungroup() %>%
                          f_breakdown_by_bins(s_title = "Hamstring Injury vs Venue",
                                              s_subtitle = "Number of games, injury rate by Venue, split by Home/Away",
                                              s_x = "Venue") +
                          facet_wrap(.~home, ncol = 1) +
                          #coord_flip() +
                          story_theme() +
                          theme(axis.text.x = element_text(angle = 45, hjust = 1))


f_conditional_ggsave(save = save_plots, 
                     p = p_hamstring_vs_venue, 
                     filepath = paste(path_Figures, "15. Hamstring Injury vs Venue.jpeg", sep = "/"), 
                     w = 5, 
                     h = 8)


##Group "small" venues into "Other":
hamstring_only <- hamstring_only %>%
                    group_by(home, Venue) %>%
                    mutate(game_count = n()) %>%
                    ungroup() %>%
                    mutate(Venue = ifelse(game_count < 1250, "Other", Venue)) %>%
                    select(-game_count)


## TODO: by nationality (grouping!)
## TODO: by team (grouping!)
## TODO: Kick-off Grouping


#######################################
## Comparison with categorical variables

f_breakdown_by_variable(hamstring_only, "Position")


f_breakdown_by_variable(hamstring_only, "Foot")
##Note: injury count is significantly lower when Foot is missing

##TODO: create some groups based on country of birth / nationality ?
f_breakdown_by_variable(hamstring_only, "Nationality")


###################################
#### Fixing NAs & encode categicals

#NAs:
hamstring_only <- hamstring_only %>% 
                    filter(Foot != "")


###################################
####  Check for correlations
## source: http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

cormat <- hamstring_only %>% 
            select(starts_with("all_mins_")) %>% ## starts_with("pl_"), 
            cor() %>%
            round(2)

# Reorder the correlation matrix
cormat <- f_reorder_cormat(cormat)
upper_tri <- f_get_upper_tri(cormat)

# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Create a ggheatmap
p_correl <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
              geom_tile(color = "white")+
              scale_fill_gradient2(low = "white", high = "firebrick", limit = c(0,1), space = "Lab", 
                                   name="Pearson\nCorrelation") +
              coord_fixed() +
              geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
              technical_theme()+ # minimal theme
              theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))+
              theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.grid.major = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.ticks = element_blank(),
                legend.justification = c(1, 0),
                legend.position = c(0.6, 0.7),
                legend.direction = "horizontal")+
                guides(fill = guide_colorbar(barwidth = 7, barheight = 1, title.position = "top", title.hjust = 0.5))

f_conditional_ggsave(save = save_plots, 
                     p = p_correl, 
                     filepath = paste(path_Figures, "16. Correlation Matrix.jpeg", sep = "/"), 
                     w = 6, 
                     h = 7)


## For now, keep only these variables:
save_for_sampling <- hamstring_only %>% 
                      select(mid, 
                             pid,
                             Year,
                             injured, 
                             `Kick-off`,              
                             Venue,
                             home,
                             all_mins_season,                  
                             all_mins_14,        
                             all_mins_4,         
                             Foot,          
                             Position,
                             `Age (Years)`,          
                             Month,        
                             `Height (cm)`,           
                             `Weight (kg)`,
                             BMI)

## Encode categoricals:
position_encoded <- save_for_sampling %>%
                      select(mid, pid, Position) %>%
                      mutate(Position = paste("Position", Position, sep = "_")) %>%
                      mutate(i = 1) %>% tidyr::spread(key = Position, value = i, fill = 0)

foot_encoded     <- save_for_sampling %>%
                      select(mid, pid, Foot) %>%
                      mutate(Foot = paste("Foot", Foot, sep = "_")) %>%
                      mutate(i = 1) %>% tidyr::spread(key = Foot, value = i, fill = 0)

kickoff_encoded  <- save_for_sampling %>%
                      select(mid, pid, `Kick-off`) %>%
                      mutate(`Kick-off` = paste("Kick-off", `Kick-off`, sep = "_")) %>%
                      mutate(i = 1) %>% tidyr::spread(key = `Kick-off`, value = i, fill = 0)

save_for_sampling <- save_for_sampling %>%
                      select(-Position, -Foot) %>%
                      left_join(position_encoded, by = c("mid", "pid")) %>%
                      left_join(foot_encoded,     by = c("mid", "pid")) %>%
                      left_join(kickoff_encoded,  by = c("mid", "pid"))

rm(position_encoded)
rm(foot_encoded)
rm(kickoff_encoded)


## save data for modeling
saveRDS(object = save_for_sampling, file = paste(path_Data, "for_sampling.RDS", sep = "/"))
