##SETUP
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lubridate)

#cleanup environment
rm(list = ls())

#import folder structure
source("GlobalVariables.R")

injuries_raw <- fread(paste(path_RawData, "injury_data_pg.csv", sep = "/")) 

#import EDA functions
source(paste(path_Functions, "f_sec_scale.R",             sep = "/"))
source(paste(path_Functions, "f_breakdown_by_bins.R",     sep = "/"))
source(paste(path_Functions, "f_breakdown_by_variable.R", sep = "/"))
source(paste(path_Functions, "f_calculate_ci.R",          sep = "/"))

#import ggplot themes
source(paste(path_Functions, "ggplot_themes.R",           sep = "/"))

##Formatting clean-up for date variables
injuries <- injuries_raw %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d"),
                     `Date of birth` = as.Date(`Date of birth`, "%Y-%m-%d")) %>%
              mutate(age_in_years = time_length(difftime(Date, `Date of birth`), "years")) 


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
  mutate(bmi = `Weight (kg)` / (`Height (cm)` / 100)^2)


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

ggsave(filename = paste(path_Figures, "1. Injury Trend Over Years.jpeg", sep = "/"), 
       plot = p_injury_trend, 
       device = "jpeg", 
       dpi = 1600, width = 6, height = 4.5)


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

ggsave(filename = paste(path_Figures, "2. Injury Types.jpeg", sep = "/"), 
       plot = p_injury_types, 
       device = "jpeg", 
       dpi = 1600, width = 6, height = 4.5)

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

ggsave(filename = paste(path_Figures, "3. Injury Lengths.jpeg", sep = "/"), 
       plot = p_injury_lenghts, 
       device = "jpeg", 
       dpi = 1600, width = 6.75, height = 4.5)

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

ggsave(filename = paste(path_Figures, "4. Hamstring Injury Lengths.jpeg", sep = "/"), 
       plot = p_hamstring_lenghts, 
       device = "jpeg", 
       dpi = 1600, width = 6, height = 4.5)


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

ggsave(filename = paste(path_Figures, "5. Hamstring Injury vs PL season minutes.jpeg", sep = "/"), 
       plot = p_by_minutes, 
       device = "jpeg", 
       dpi = 1600, width = 6, height = 4.5)

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

ggsave(filename = paste(path_Figures, "5a. Hamstring Injury vs Position.jpeg", sep = "/"), 
       plot = p_by_position1, 
       device = "jpeg", 
       dpi = 1600, width = 4, height = 4.5)

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


ggsave(filename = paste(path_Figures, "5b. Hamstring Injury vs Position.jpeg", sep = "/"), 
       plot = p_by_position2, 
       device = "jpeg", 
       dpi = 1600, width = 6, height = 4.5)

##TODO: finalize formatting, grid
#Note1 - Goalkeepers: very different dynamics, as shown by the Injury rate %
#Note2 - Missing: don't trust that data very much


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

ggsave(filename = paste(path_Figures, "5. Hamstring Injury vs Minutes played.jpeg", sep = "/"), 
       plot = p_minutes_played, 
       device = "jpeg", 
       dpi = 1600, width = 6, height = 4.5)


##Visualize: Injury vs Age
p_hamstring_vs_age <- hamstring_only %>%
                        mutate(`Age (Years)` = cut(age_in_years, 
                                                   breaks = c(-Inf, 20, 22.5, 25, 27.5, 30, 32.5, 35, Inf), 
                                                   include.lowest = T)) %>%
                        group_by(`Age (Years)`) %>%
                        summarize(game_count = n(),
                                  injury_count = (sum(injury_type != 0))) %>%
                        ungroup() %>%
                        mutate(injury_rate_pct = injury_count / game_count * 100) %>%
                        f_breakdown_by_bins(s_title = "Age vs Injury rates",
                                            s_x = "`Age (Years)`")

ggsave(filename = paste(path_Figures, "6. Hamstring Injury vs Age.jpeg", sep = "/"), 
       plot = p_hamstring_vs_age, 
       device = "jpeg", 
       dpi = 1600, width = 6, height = 4.5)


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
                                              s_x = "`Weight (kg)`")

ggsave(filename = paste(path_Figures, "7. Hamstring Injury vs Weight.jpeg", sep = "/"), 
       plot = p_hamstring_vs_weight, 
       device = "jpeg", 
       dpi = 1600, width = 6, height = 4.5)


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
                                              s_x = "`Height (cm)`")

ggsave(filename = paste(path_Figures, "8. Hamstring Injury vs Height.jpeg", sep = "/"), 
       plot = p_hamstring_vs_height, 
       device = "jpeg", 
       dpi = 1600, width = 6, height = 4.5)

## BMI:
p_hamstring_vs_bmi <- hamstring_only %>%
                        mutate(BMI = cut(bmi, 
                                         breaks = c(-Inf, 20, 21, 22, 23, 24, 25, 26, 27, Inf), 
                                         include.lowest = T)) %>%
                        group_by(BMI) %>%
                        summarize(game_count = n(),
                                  injury_count = (sum(injury_type != 0))) %>%
                        ungroup() %>%
                        mutate(injury_rate_pct = injury_count / game_count * 100) %>%
                        f_breakdown_by_bins(s_title = "BMI vs Injury rates",
                                            s_x = "BMI")

ggsave(filename = paste(path_Figures, "9. Hamstring Injury vs BMI.jpeg", sep = "/"), 
       plot = p_hamstring_vs_bmi, 
       device = "jpeg", 
       dpi = 1600, width = 6, height = 4.5)


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
                                                s_x = "`Kick-off Hour`")

ggsave(filename = paste(path_Figures, "10. Hamstring Injury vs Kick-off time.jpeg", sep = "/"), 
       plot = p_hamstring_vs_kickoff, 
       device = "jpeg", 
       dpi = 1600, width = 6, height = 4.5)


##By Year
p_hamstring_vs_year <- hamstring_only %>%
                        mutate(Year = year(Date)) %>%
                          group_by(Year) %>%
                          summarize(game_count = n(),
                                    injury_count = (sum(injury_type != 0))) %>%
                          ungroup() %>%
                          mutate(injury_rate_pct = injury_count / game_count * 100) %>%
                          f_breakdown_by_bins(s_title = "Hamstring Injury vs Year",
                                              s_subtitle = "Number of games, injury rate by year",
                                              s_x = "Year")

ggsave(filename = paste(path_Figures, "11. Hamstring Injury vs Year.jpeg", sep = "/"), 
       plot = p_hamstring_vs_year, 
       device = "jpeg", 
       dpi = 1600, width = 6, height = 4.5)


##By Month --> visualize by year as well


p_hamstring_vs_month <- hamstring_only %>%
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


ggsave(filename = paste(path_Figures, "12. Hamstring Injury vs Month.jpeg", sep = "/"), 
       plot = p_hamstring_vs_month, 
       device = "jpeg", 
       dpi = 1600, width = 6, height = 4.5)


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


ggsave(filename = paste(path_Figures, "12b. Hamstring Injury vs Month.jpeg", sep = "/"), 
       plot = p_hamstring_vs_month_w_conf, 
       device = "jpeg", 
       dpi = 1600, width = 6, height = 4.5)

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

#######################################
## Comparison with categorical variables

f_breakdown_by_variable(hamstring_only, "Position")
##Note: Position is missing for a few records. Zero injuries. TODO: Investigate structure of missing data!
##Goalkeepers getting injured a lot less.


f_breakdown_by_variable(hamstring_only, "Foot")
##Note: injury count is significantly lower when Foot is missing

##TODO: create some groups based on country of birth / nationality ?
f_breakdown_by_variable(hamstring_only, "Nationality")


###################################
#### Fixing NAs & encode categicals

#NAs:

hamstring_only <- hamstring_only %>%
                    select(mid, pid, injured, Position, Foot, age_in_years, `Height (cm)`, `Weight (kg)`, bmi,
                           Date, starts_with("all_"), starts_with("pl_"))

hamstring_only <- hamstring_only %>% mutate(Position = ifelse(Position == "", "Missing", Position))
hamstring_only <- hamstring_only %>% mutate(Foot     = ifelse(Foot     == "", "Missing", Foot))

## Encode categoricals:
position_encoded <- hamstring_only %>%
                      select(mid, pid, Position) %>%
                      mutate(Position = paste("Position", Position, sep = "_")) %>%
                      mutate(i = 1) %>% tidyr::spread(key = Position, value = i, fill = 0) %>%
                      select(-Position_Goalkeeper) # assigning Goalkeeper as the "base value" for position

foot_encoded     <- hamstring_only %>%
                      select(mid, pid, Foot) %>%
                      mutate(Foot = paste("Foot", Foot, sep = "_")) %>%
                      mutate(i = 1) %>% tidyr::spread(key = Foot, value = i, fill = 0) %>%
                      select(-Foot_Right) # assigning Right as the "base value" for Foot

hamstring_only <- hamstring_only %>%
                    select(-Position, -Foot) %>%
                    left_join(position_encoded, by = c("mid", "pid")) %>%
                    left_join(foot_encoded,     by = c("mid", "pid"))

rm(position_encoded)
rm(foot_encoded)


## Games/minutes fields are populated with NAs for 25 rows:
## why is this data missing?
hamstring_only <- hamstring_only %>%
                    filter(!is.na(all_games_season) &  !is.na(pl_games_season))