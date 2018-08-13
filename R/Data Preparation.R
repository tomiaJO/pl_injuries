## Library Import
require(data.table)
require(tidyverse)
require(lubridate)
require(gridExtra)

## Cleanup environment
rm(list = ls())
gc()


## Import folder structure
source("GlobalVariables.R")


## Set saving of figures
save_plots <- T


## Import EDA functions
source(paste(path_Functions, "f_sec_scale.R",               sep = "/"))
source(paste(path_Functions, "f_breakdown_by_bins.R",       sep = "/"))
source(paste(path_Functions, "f_breakdown_by_variable.R",   sep = "/"))
source(paste(path_Functions, "f_calculate_ci.R",            sep = "/"))
source(paste(path_Functions, "f_breakdown_w_ci.R",          sep = "/"))
source(paste(path_Functions, "f_conditional_ggsave.R",      sep = "/"))
source(paste(path_Functions, "f_plot_ir.R",                 sep = "/"))
source(paste(path_Functions, "f_save_plots.R",              sep = "/"))
source(paste(path_Functions, "f_player_history.R",          sep = "/"))
source(paste(path_Functions, "f_breakdown_injury_length.R", sep = "/"))

## Import ggplot themes
source(paste(path_Functions, "ggplot_themes.R",           sep = "/"))

## Load data
injuries          <- fread(paste(path_RawData, "injury_data_pg.csv",     sep = "/"))
country_to_region <- fread(paste(path_Data,    "country_to_region.csv",  sep = "/"))


## Formatting clean-up for date variables
injuries <- injuries %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d"),
                     `Date of birth` = as.Date(`Date of birth`, "%Y-%m-%d")) %>%
              mutate(`Age (Years)` = time_length(difftime(Date, `Date of birth`), "years")) 


## Add Weekday
injuries <- injuries %>%
              mutate(Weekday = weekdays(Date)) %>%
              mutate(Weekday = factor(Weekday, 
                                      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))


## Add Month
Month_mapping <- data.frame(Month_Num = c(1:12), Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                            Month_in_Season = c(6, 7, 8, 9, 10, 11, 12, 1, 2, 3, 4, 5))

injuries <- injuries %>%
              mutate(Month_Num = month(Date)) %>%
              left_join(Month_mapping, by = "Month_Num") %>%
              mutate(Month = reorder(Month, Month_in_Season)) %>%
              select(-Month_Num, -Month_in_Season)

## Add Year
injuries <- injuries %>%
              mutate(Year = year(Date))


## Fix formatting: for height & weight
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


## Add BMI column:
injuries <- injuries %>%
              mutate(BMI = `Weight (kg)` / (`Height (cm)` / 100)^2)

## Add Team & Opponent:
injuries <- injuries %>% 
              mutate(Team     = ifelse(home == 1, home_team, away_team)) %>%
              mutate(Opponent = ifelse(home == 0, home_team, away_team))

## Fix formatting of "home" column
injuries <- injuries %>% 
              mutate(home = factor(home, levels = c(1, 0), labels = c("Home", "Away")))

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


## Drop teams that are not present all years
## TODO: explain why drop all others
injuries <- injuries %>%
              mutate(Team = ifelse(Team %in% c("Chelsea",
                                               "Manchester City",
                                               "Arsenal",
                                               "Everton",
                                               "Tottenham Hotspur",
                                               "Manchester United",
                                               "Liverpool",
                                               "Stoke City",
                                               "West Bromwich Albion",
                                               "Sunderland",
                                               "West Ham United",
                                               "Swansea City",
                                               "Newcastle United"), Team, "Other")) %>%
              mutate(Opponent = ifelse(Opponent %in% c("Chelsea",
                                                       "Manchester City",
                                                       "Arsenal",
                                                       "Everton",
                                                       "Tottenham Hotspur",
                                                       "Manchester United",
                                                       "Liverpool",
                                                       "Stoke City",
                                                       "West Bromwich Albion",
                                                       "Sunderland",
                                                       "West Ham United",
                                                       "Swansea City",
                                                       "Newcastle United"), Opponent, "Other"))


##Visualize: Injury type counts
p_injury_types <- injuries %>%
                    filter(injury_type != '0') %>%
                    #filter(grepl(x = injury_type, pattern = "Strain", fixed = T, ignore.case = T)) %>%
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
                    labs(title    = "Lower body muscle strains are among the most common injuries",
                         x        = "Injury Type",
                         y        = "Number of injuries",
                         caption  = "Note: Injuries that happened less than 25x are group as 'Other'") +
                    story_theme() + theme(plot.title = element_text(size = 10.5))

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


## Keep only hamstring
injuries <- injuries %>% 
              filter(injury_type %in% c("0", 
                                        "Hamstring", 
                                        "Groin Strain", 
                                        "Calf Muscle Strain", 
                                        "Thigh Muscle Strain")) %>%
              mutate(injury_type = factor(injury_type, levels = c("0", 
                                                                  "Hamstring", 
                                                                  "Groin Strain", 
                                                                  "Calf Muscle Strain", 
                                                                  "Thigh Muscle Strain")))


## Hamstring zoom:
tmp <- injuries %>%
        filter(injury_type != "0") %>%
        summarize(median_injury_length = median(injury_length),
                  perc95_injury_length = quantile(injury_length, .95))

median_injury_length <- tmp %>% pull(median_injury_length)
perc95_injury_length <- tmp %>% pull(perc95_injury_length)

p_hamstring_lenghts <- injuries %>%
                        filter(injury_type != "0") %>%
                        mutate(injury_length_capped_180 = ifelse(injury_length >= 180, 180, injury_length)) %>% 
                        ggplot(aes(x = injury_length_capped_180)) +
                        geom_density(fill = "steelblue2", color = "steelblue2")

## add highlight
d <- ggplot_build(p_hamstring_lenghts)$data[[1]]

p_hamstring_lenghts <- p_hamstring_lenghts +
                        geom_area(data = subset(d, x < 14), 
                                  aes(x = x, y = y), 
                                  fill = "darksalmon", color = "darksalmon")

## add median / .95 perc lines
p_hamstring_lenghts <- p_hamstring_lenghts +
                        geom_vline(xintercept = median_injury_length, color = "firebrick") +
                          geom_text(aes(x = median_injury_length, y = 0.03), 
                                    label = paste("Median:",  median_injury_length, "days", sep = " "), 
                                    hjust = -0.1, vjust = 0, color = "firebrick") +
                          geom_vline(xintercept = perc95_injury_length, color = "firebrick") +
                          geom_text(aes(x = perc95_injury_length, y = 0.02), 
                                    label = paste(".95 percentile:",  perc95_injury_length, "days", sep = " "), 
                                    hjust = -0.1, vjust = 0, color = "firebrick")

## formatting
p_hamstring_lenghts <- p_hamstring_lenghts +
                        labs(title    = "Typical injuries last 1 to 11 weeks",
                             subtitle = "Density plot, Length of injuries",
                             x        = "Injury Length (days)",
                             y        = "Density",
                             caption  = "Note: Injury length was capped at 180 days") + 
                        theme(legend.position = "bottom") +
                        story_theme()

f_conditional_ggsave(save = save_plots, 
                     p = p_hamstring_lenghts, 
                     filepath = paste(path_Figures, "4. Hamstring Injury Lengths.jpeg", sep = "/"), 
                     w = 6, 
                     h = 4.5)


## Keep only injuries with "significant" missed time
injuries <- injuries %>%
              filter(injury_type == "0" | injury_length >= 14)


## REMOVE: minimal playing time
injuries <- injuries %>%
              filter(!is.na(pl_mins_season)) %>%
              filter(!is.na(all_mins_season))

season_min_cutoff <- 180

tmp <- injuries %>%
        group_by(pl_mins_season < season_min_cutoff) %>%
        summarize(game_count = n(),
                  injury_count = sum(injured),
                  injury_rate_pct = sum(injured) / n(),
                  ci95_lower_injury_rate_pct = f_calculate_ci(injury_rate_pct, game_count, 0.95)$lower,
                  ci95_upper_injury_rate_pct = f_calculate_ci(injury_rate_pct, game_count, 0.95)$upper) %>%
        ungroup()

keep_injury_rate_pct <- formatC(tmp[tmp$`pl_mins_season < season_min_cutoff` == FALSE, ]$injury_rate_pct * 100, digits = 2, format = "f")
cut_injury_rate_pct  <- formatC(tmp[tmp$`pl_mins_season < season_min_cutoff` == TRUE,  ]$injury_rate_pct * 100, digits = 2, format = "f")

p_by_minutes <- injuries %>%
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
                     filepath = paste(F, "5. Injury rates vs PL season minutes.jpeg", sep = "/"), 
                     w = 6, 
                     h = 4.5)

## REMOVE: goalkeepers & missing position
p_by_position1 <- injuries %>%
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
                         title = "Positional breakdown",
                         subtitle = "Games Played") +
                    story_theme()


l_by_position2 <- injuries %>%
                    mutate(Position = ifelse(Position == "", "Missing", Position)) %>%
                    f_plot_ir(s_x = "Position")

p_by_position2 <- l_by_position2$data %>%
                    mutate(Position = factor(Position, levels = c("Missing", "Goalkeeper", "Defender", "Midfielder", "Attacker"))) %>%
                    ggplot(aes(x = Position, y = injury_rate_pct, label = formatC(injury_rate_pct, digits = 2, format = "f"))) + 
                      geom_point(stat='identity', color = "firebrick", size = 6) +
                      geom_segment(aes(y = ci95_lower_injury_rate_pct, 
                                       yend = ci95_upper_injury_rate_pct, 
                                       x = Position, 
                                       xend = Position), 
                                   color = "firebrick") +
                      geom_text(color = "white", size = 2) +
                      coord_flip() + 
                      labs(y        = "Injury Rate (%)",
                           x        = "Position",
                           title    = "",
                           subtitle = "Injury Rates") +
                      story_theme() +
                      theme(axis.text.y = element_blank(),
                            axis.title.y = element_blank())

## Grid formatting
g <- arrangeGrob(p_by_position1, p_by_position2, layout_matrix = rbind(c(1,1,1,2,2)))

f_conditional_ggsave(save = save_plots, 
                     p = g, 
                     filepath = paste(path_Figures, "6. Injuries vs Position.jpeg", sep = "/"), 
                     w = 6, 
                     h = 4.5)


## REMOVE positions
injuries <- injuries %>%
                    #Note1 - Goalkeepers: very different dynamics, as shown by the Injury rate %
                    filter(Position != "Goalkeeper") %>%
                    #Note2 - Missing: don't trust that data very much
                    filter(Position != "")


##Visualize: Injury vs Minutes played in a given game - less time, less chance to get injured?
p_minutes_played <- injuries %>%
                      mutate(`Minutes played` = cut(minutes, 
                                                    breaks = c(0, 15, 30, 45, 60, 75,  Inf), 
                                                    include.lowest = T,
                                                    labels = c("-15", "15-30", "30-45", "45-60", "60-75", "75+"))) %>%
                      f_plot_ir(s_x = "`Minutes played`", 
                                s_title = "Injuries vs Minutes Played")

f_save_plots(save_plots = save_plots, 
             p = p_minutes_played, 
             n = "7", 
             title = "Injuries vs Minutes Played", 
             w = 6, 
             h = 4, 
             save_all = FALSE)


##Visualize: Injury vs Age
p_hamstring_vs_age <- injuries %>%
                        mutate(`Age (Years)` = cut(`Age (Years)`, 
                                                   breaks = c(-Inf, 20, 22.5, 25, 27.5, 30, 32.5, 35, Inf), 
                                                   include.lowest = T)) %>%
                        f_plot_ir(s_x     = "`Age (Years)`", 
                                  s_title = "Injuries vs Age")

f_save_plots(save_plots, p_hamstring_vs_age, "8", "Injuries vs Age", 6, 4.5)


## By Weight
p_hamstring_vs_weight <- injuries %>%
                          mutate(`Weight (kg)` = cut(`Weight (kg)`, 
                                                     breaks = c(-Inf, 60, 65, 70, 75, 80, 85, 90, Inf), 
                                                     include.lowest = T)) %>%
                          f_plot_ir(s_x     = "`Weight (kg)`", 
                                    s_title = "Injuries vs Weight")

f_save_plots(save_plots, p_hamstring_vs_weight, "9", "Injuries vs Weight", 6, 4.5)


## By Height:
p_hamstring_vs_height <- injuries %>%
                          mutate(`Height (cm)` = cut(`Height (cm)`, 
                                                     breaks = seq(0, 220, by = 5), 
                                                     include.lowest = T)) %>%
                          f_plot_ir(s_x     = "`Height (cm)`", 
                                    s_title = "Injuries vs Height")

f_save_plots(save_plots, p_hamstring_vs_height, "10", "Injuries vs Height", 6, 4.5)


## By BMI:
p_hamstring_vs_bmi <- injuries %>%
                        mutate(BMI = cut(BMI, 
                                         breaks = c(-Inf, 20, 21, 22, 23, 24, 25, 26, 27, Inf), 
                                         include.lowest = T)) %>%
                        f_plot_ir(s_x     = "BMI", 
                                  s_title = "Injuries vs BMI")

f_save_plots(save_plots, p_hamstring_vs_bmi, "11", "Injuries vs BMI", 6, 4.5)


## By Kick-off time
injuries <- injuries %>%
              mutate(`Kick-off` = as.numeric(substr(`Kick-off`, 1, 2)) + as.numeric(as.numeric(substr(`Kick-off`, 4, 5)) > 30)) %>%
              mutate(`Kick-off` = cut(`Kick-off`, 
                                      breaks = c(0, 15, 16, 17, 18, 19, Inf), 
                                      include.lowest = T, 
                                      labels = c("- 15.00", "15-16", "16-17", "17-18", "18-19", "19:00 -")))
  
##TODO: something is not right, last break should be 24.. only works with Inf now
p_hamstring_vs_kickoff <- injuries %>%
                            f_plot_ir(s_x     = "`Kick-off`", 
                                      s_title = "Injuries vs Kick-off Time")

f_save_plots(save_plots, p_hamstring_vs_kickoff, "12", "Injuries vs Kick-off Time", 6, 4.5)

 
## By Year
p_hamstring_vs_year <- injuries %>%
                          f_plot_ir(s_x     = "Year", 
                                    s_title = "Injuries vs Year")

f_save_plots(save_plots, p_hamstring_vs_year, "13", "Injuries vs Year", 6, 4.5)
                          

## By Month --> visualize by year as well
p_hamstring_vs_month <- injuries %>%
                          f_plot_ir(s_x     = "Month",
                                    s_title = "Injuries vs Month")

f_save_plots(save_plots, p_hamstring_vs_month, "14", "Injuries vs Month", 6, 4.5)

## TODO: Break-out Month by Year

## By Weekday
p_hamstring_vs_weekday <- injuries %>%
                            f_plot_ir(s_x     = "Weekday", 
                                      s_title = "Injuries vs Weekday")

f_save_plots(save_plots, p_hamstring_vs_weekday, "15", "Injuries vs Weekday", 6, 4.5)


## By Venue
p_hamstring_vs_venue <- injuries %>%
                          group_by(home, Venue) %>%
                          mutate(game_count = n()) %>%
                          ungroup() %>%
                          mutate(Venue = ifelse(game_count < 500, "Other", Venue)) %>%
                          mutate(Venue = reorder(Venue, -game_count)) %>%
                          f_plot_ir(s_x        = "Venue", 
                                    s_facet    = "home",
                                    s_title    = "Injuries vs Venue",
                                    s_subtitle = "Split by Home/Away")

f_save_plots(save_plots, p_hamstring_vs_venue, "16", "Injuries vs Venue", 4.5, 6)


## Group "small" venues into "Other":
injuries <- injuries %>%
              group_by(home, Venue) %>%
              mutate(game_count = n()) %>%
              ungroup() %>%
              mutate(Venue = ifelse(game_count < 500, "Other", Venue)) %>%
              select(-game_count)


## By nationality (grouped)
injuries <- injuries %>%
              mutate(Nationality = ifelse(Nationality == "", "Missing", Nationality)) %>%
              left_join(country_to_region, by = c("Nationality" = "Country")) %>%
              rename("Region - Nationality" = "Region") %>%
              mutate(`Region - Nationality` = factor(`Region - Nationality`, 
                                                     levels = c("UK", "Western Europe",
                                                                "Eastern Europe", "Americas",
                                                                "Africa & Middle East", "Asia")))
  
p_hamstring_vs_nationality <- injuries %>%
                                f_plot_ir(s_x = "`Region - Nationality`",
                                          s_title = "Injuries vs Nationality (Regionalized)")

f_save_plots(save_plots, p_hamstring_vs_nationality, "17", "Injuries vs Nationality (Regionalized)", 6, 4)

## By country of birth
injuries <- injuries %>%
              mutate(`Country of birth` = ifelse(`Country of birth` == "", "Missing", `Country of birth`)) %>%
              left_join(country_to_region, by = c("Country of birth" = "Country")) %>%
              rename("Region - Birth" = "Region") %>%
              mutate(`Region - Birth` = factor(`Region - Birth`, 
                                                     levels = c("UK", "Western Europe",
                                                                "Eastern Europe", "Americas",
                                                                "Africa & Middle East", "Asia")))

p_hamstring_vs_birth_country <- injuries %>%
                                  f_plot_ir(s_x = "`Region - Birth`",
                                            s_title = "Injuries vs Birth Country (Regionalized)")

f_save_plots(save_plots, p_hamstring_vs_birth_country, "18", "Injuries vs Birth Country (Regionalized)", 6, 4)


## By Team
p_hamstring_vs_team <- injuries %>%
                        f_plot_ir(s_x = "Team",
                                  s_title = "Injury rate vs Team",
                                  s_facet = "home")

f_save_plots(save_plots, p_hamstring_vs_team, "19", "Injuries vs Team", 4.5, 6)


## By Opponent
p_hamstring_vs_opponent <- injuries %>%
                              f_plot_ir(s_x = "Opponent",
                                        s_title = "Injuries vs Opponent",
                                        s_facet = "home")

f_save_plots(save_plots, p_hamstring_vs_opponent, "20", "Injuries vs Opponent", 4.5, 6)


#######################################
## By player history
history_career <- f_player_history(injuries) %>%
                    rename("Injured Career" = injured_before) %>%
                    rename("Games Career"   = games_before)

history_90days <- f_player_history(injuries, 90) %>%
                    rename("Injured last 90days" = injured_before) %>%
                    rename("Games last 90days"   = games_before)

history_1year  <- f_player_history(injuries, 365) %>%
                    rename("Injured last year" = injured_before) %>%
                    rename("Games last year"   = games_before)

injuries <- injuries %>%
              left_join(history_career, by = c("pid" = "pid", "Date" = "Date")) %>%
              mutate(`Injured Career` = ifelse(is.na(`Injured Career`), 0, `Injured Career`),
                     `Games Career`   = ifelse(is.na(`Games Career`),   0, `Games Career`)) %>%
              left_join(history_90days, by = c("pid" = "pid", "Date" = "Date")) %>%
              mutate(`Injured last 90days` = ifelse(is.na(`Injured last 90days`), 0, `Injured last 90days`),
                     `Games last 90days`   = ifelse(is.na(`Games last 90days`),   0, `Games last 90days`)) %>%
              left_join(history_1year, by = c("pid" = "pid", "Date" = "Date")) %>%
              mutate(`Injured last year` = ifelse(is.na(`Injured last year`), 0, `Injured last year`),
                     `Games last year`   = ifelse(is.na(`Games last year`),   0, `Games last year`))


#######################################
## By game counts & minutes

## PL Games in Season
p_hamstring_vs_pl_games_season <- injuries %>%
                                    mutate(pl_games_season = cut(pl_games_season, 
                                                               breaks = c(-Inf, 5, 10, 15, 20, 25, Inf), 
                                                               include.lowest = T)) %>%
                                    f_plot_ir(s_x     = "pl_games_season", 
                                              s_title = "Injuries vs PL Games in Season")

f_save_plots(save_plots, p_hamstring_vs_pl_games_season, "21", "Injuries vs PL Games in Season", 6, 4.5)


## All Games in Season
p_hamstring_vs_all_games_season <- injuries %>%
                                    mutate(all_games_season = cut(all_games_season, 
                                                                 breaks = c(-Inf, 5, 10, 15, 20, 25, Inf), 
                                                                 include.lowest = T)) %>%
                                    f_plot_ir(s_x     = "all_games_season", 
                                              s_title = "Injuries vs All Games in Season")

f_save_plots(save_plots, p_hamstring_vs_all_games_season, "22", "Injuries vs All Games in Season", 6, 4.5)

## CREATE non-PL variables
injuries <- injuries %>%
              mutate(non_pl_games_season = all_games_season - pl_games_season,
                     non_pl_games_28     = all_games_28     - pl_games_28,
                     non_pl_games_4      = all_games_14     - pl_games_14,
                     non_pl_games_4      = all_games_4      - pl_games_4,
                     non_pl_min_season   = all_mins_season  - pl_mins_season,
                     non_pl_min_28       = all_mins_28      - pl_mins_28,
                     non_pl_min_14       = all_mins_14      - pl_mins_14,
                     non_pl_min_4        = all_mins_4       - pl_mins_4)

## Non-PL Games in Season
p_hamstring_vs_nonpl_games_season <- injuries %>%
                                      mutate(non_pl_games_season = cut(non_pl_games_season,
                                                                   breaks = c(-Inf, 2.5, 5, 7.5, 10, 12.5, 15, Inf),
                                                                   include.lowest = T)) %>%
                                      f_plot_ir(s_x     = "non_pl_games_season", 
                                                s_title = "Injuries vs Non-PL Games in Season")

f_save_plots(save_plots, p_hamstring_vs_nonpl_games_season, "23", "Injuries vs Non-PL Games in Season", 6, 4.5)
                                      

###################################
## Fixing NAs & encode categicals
injuries <- injuries %>% 
              filter(Foot != "")


###################################
## For now, keep only these variables:
names(injuries)
original_sample <- injuries %>% 
                      select(mid, 
                             pid,
                             Year,
                             injured,
                             injury_length,
                             `Kick-off`, 
                             Team,
                             Opponent,
                             Venue,
                             home,
                             starts_with("all_"),
                             starts_with("pl_"),
                             starts_with("non_pl_"),
                             Foot,          
                             Position,
                             `Region - Nationality`,
                             `Region - Birth`,
                             `Age (Years)`,          
                             Month,
                             Weekday,
                             `Height (cm)`,           
                             `Weight (kg)`,
                             BMI,
                             `Injured Career`,       
                             `Games Career`,
                             `Injured last 90days`,
                             `Games last 90days`,
                             `Injured last year`,    
                             `Games last year` )


## Encode categoricals:
position_encoded <- original_sample %>%
                      select(mid, pid, Position) %>%
                      mutate(Position = paste("Position", Position, sep = "_")) %>%
                      mutate(i = 1) %>% tidyr::spread(key = Position, value = i, fill = 0)

foot_encoded     <- original_sample %>%
                      select(mid, pid, Foot) %>%
                      mutate(Foot = paste("Foot", Foot, sep = "_")) %>%
                      mutate(i = 1) %>% tidyr::spread(key = Foot, value = i, fill = 0)

kickoff_encoded  <- original_sample %>%
                      select(mid, pid, `Kick-off`) %>%
                      mutate(`Kick-off` = paste("Kick-off", `Kick-off`, sep = "_")) %>%
                      mutate(i = 1) %>% tidyr::spread(key = `Kick-off`, value = i, fill = 0)

month_encoded    <- original_sample %>%
                      select(mid, pid, Month) %>%
                      mutate(Month = paste("Month", Month, sep = "_")) %>%
                      mutate(i = 1) %>% tidyr::spread(key = Month, value = i, fill = 0)

weekday_encoded  <- original_sample %>%
                      select(mid, pid, Weekday) %>%
                      mutate(Weekday = paste("Day", Weekday, sep = "_")) %>%
                      mutate(i = 1) %>% tidyr::spread(key = Weekday, value = i, fill = 0)

venue_encoded    <- original_sample %>%
                      select(mid, pid, Venue) %>%
                      mutate(Venue = paste("Venue", Venue, sep = "_")) %>%
                      mutate(i = 1) %>% tidyr::spread(key = Venue, value = i, fill = 0)

team_encoded     <- original_sample %>%
                      select(mid, pid, Team) %>%
                      mutate(Team = paste("Team", Team, sep = "_")) %>%
                      mutate(i = 1) %>% tidyr::spread(key = Team, value = i, fill = 0)

opponent_encoded <- original_sample %>%
                      select(mid, pid, Opponent) %>%
                      mutate(Opponent = paste("Opponent", Opponent, sep = "_")) %>%
                      mutate(i = 1) %>% tidyr::spread(key = Opponent, value = i, fill = 0)

nation_encoded   <- original_sample %>%
                        select(mid, pid, `Region - Nationality`) %>%
                        mutate(`Region - Nationality` = paste("Nationality", `Region - Nationality`, sep = "_")) %>%
                        mutate(i = 1) %>% tidyr::spread(key = `Region - Nationality`, value = i, fill = 0)

birth_encoded    <- original_sample %>%
                      select(mid, pid, `Region - Birth`) %>%
                      mutate(`Region - Birth` = paste("Birth", `Region - Birth`, sep = "_")) %>%
                      mutate(i = 1) %>% tidyr::spread(key = `Region - Birth`, value = i, fill = 0)

## remove original encoding
original_sample <- original_sample %>%
                      select(-Position, -Foot, -`Kick-off`, -Month, -Venue, -Team, -Opponent,
                             -`Region - Nationality`, -`Region - Birth`) 

## add dummy versions
original_sample <- original_sample %>%
                      left_join(position_encoded, by = c("mid", "pid")) %>%
                      left_join(foot_encoded,     by = c("mid", "pid")) %>%
                      left_join(kickoff_encoded,  by = c("mid", "pid")) %>%
                      left_join(month_encoded,    by = c("mid", "pid")) %>%
                      left_join(venue_encoded,    by = c("mid", "pid")) %>%
                      left_join(weekday_encoded,  by = c("mid", "pid")) %>%
                      left_join(nation_encoded,   by = c("mid", "pid")) %>%
                      left_join(birth_encoded,    by = c("mid", "pid")) %>%
                      left_join(team_encoded,     by = c("mid", "pid")) %>%
                      left_join(opponent_encoded, by = c("mid", "pid"))



rm(position_encoded)
rm(foot_encoded)
rm(kickoff_encoded)
rm(month_encoded)
rm(venue_encoded)
rm(weekday_encoded)
rm(nation_encoded)
rm(birth_encoded)
rm(team_encoded)
rm(opponent_encoded)


###################################
## save data for modeling
saveRDS(object = original_sample, file = paste(path_Data, "original_sample.RDS", sep = "/"))

original_sample$injured %>% sum() / original_sample %>% nrow()
