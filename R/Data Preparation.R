##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Initialize R setup
source("GlobalStartup.R")

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load data
injuries          <- readRDS(file = paste(path_Data, "injuries.RDS", sep = "/"))
country_to_region <- data.table::fread(paste(path_Data, "country_to_region.csv", sep = "/"))


injuries %>% count(Year)
##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## FIX formatting: kick-off
injuries <- injuries %>%
              mutate(`Kick-off` = as.numeric(substr(`Kick-off`, 1, 2)) + as.numeric(as.numeric(substr(`Kick-off`, 4, 5)) > 30)) %>%
              mutate(`Kick-off` = cut(`Kick-off`, 
                                      breaks = c(0, 15, 16, 17, 18, 19, Inf), 
                                      include.lowest = T, 
                                      labels = c("- 15.00", "15-16", "16-17", "17-18", "18-19", "19:00 -")))

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## FIX formatting: for height & weight
injuries <- injuries %>%
              mutate(`Height (cm)` = gsub(pattern = " cm", replacement = "", x = Height)) %>%
              mutate(`Height (cm)` = as.numeric(`Height (cm)`)) %>%
              select(everything(), -Height)
            
            injuries <- injuries %>%
              mutate(`Weight (kg)` = gsub(pattern = " kg", replacement = "", x = Weight)) %>%
              mutate(`Weight (kg)` = as.numeric(`Weight (kg)`)) %>%
              select(everything(), -Weight)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ADD Weekday
injuries <- injuries %>%
              mutate(Weekday = weekdays(Date)) %>%
              mutate(Weekday = factor(Weekday, 
                                      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ADD Month
Month_mapping <- data.frame(Month_Num = c(1:12), Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                            Month_in_Season = c(6, 7, 8, 9, 10, 11, 12, 1, 2, 3, 4, 5))

injuries <- injuries %>%
              mutate(Month_Num = month(Date)) %>%
              left_join(Month_mapping, by = "Month_Num") %>%
              mutate(Month = reorder(Month, Month_in_Season)) %>%
              select(-Month_Num, -Month_in_Season)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ADD BMI column:
injuries <- injuries %>%
              mutate(BMI = `Weight (kg)` / (`Height (cm)` / 100)^2)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ADD Team & Opponent:
injuries <- injuries %>% 
              mutate(Team     = ifelse(home == "Home", home_team, away_team)) %>%
              mutate(Opponent = ifelse(home == "Away", home_team, away_team))

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ADD non-PL variables
injuries <- injuries %>%
              mutate(non_pl_games_season = all_games_season - pl_games_season,
                     non_pl_games_28     = all_games_28     - pl_games_28,
                     non_pl_games_14     = all_games_14     - pl_games_14,
                     non_pl_games_4      = all_games_4      - pl_games_4,
                     non_pl_min_season   = all_mins_season  - pl_mins_season,
                     non_pl_min_28       = all_mins_28      - pl_mins_28,
                     non_pl_min_14       = all_mins_14      - pl_mins_14,
                     non_pl_min_4        = all_mins_4       - pl_mins_4)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ADD avg. minutes
injuries <- injuries %>% 
              mutate(pl_avgmin_season     = ifelse(pl_games_season     == 0, 0, pl_mins_season    / pl_games_season),
                     pl_avgmin_28         = ifelse(pl_games_28         == 0, 0, pl_mins_28        / pl_games_28),
                     pl_avgmin_14         = ifelse(pl_games_14         == 0, 0, pl_mins_14        / pl_games_28),
                     pl_avgmin_4          = ifelse(pl_games_4          == 0, 0, pl_mins_4         / pl_games_4),
                     non_pl_avgmin_season = ifelse(non_pl_games_season == 0, 0, non_pl_min_season / non_pl_games_season),
                     non_pl_avgmin_28     = ifelse(non_pl_games_28     == 0, 0, non_pl_min_28     / non_pl_games_28),
                     non_pl_avgmin_14     = ifelse(non_pl_games_14     == 0, 0, non_pl_min_14     / non_pl_games_28),
                     non_pl_avgmin_4      = ifelse(non_pl_games_4      == 0, 0, non_pl_min_4      / non_pl_games_4))

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ADD player history
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

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ADD future injury columns
future_7days  <- f_future_injury(df = injuries, days_forward = 7)
future_7days  <- future_7days %>%
                  rename("injured_7days"  = "injured_future")

future_14days <- f_future_injury(df = injuries, days_forward = 14)
future_14days <- future_14days %>%
                  rename("injured_14days" = "injured_future")

future_21days <- f_future_injury(df = injuries, days_forward = 21)
future_21days <- future_21days %>%
                  rename("injured_21days" = "injured_future")

future_28days <- f_future_injury(df = injuries, days_forward = 28)
future_28days <- future_28days %>%
                  rename("injured_28days" = "injured_future")

injuries <- injuries %>%
              left_join(future_7days,  by = c("pid", "Date")) %>%
              left_join(future_14days, by = c("pid", "Date")) %>%
              left_join(future_21days, by = c("pid", "Date")) %>%
              left_join(future_28days, by = c("pid", "Date")) %>%
              select(injured, injured_7days, injured_14days, injured_21days, injured_28days, everything())


##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## GROUP Nationality
injuries <- injuries %>%
              mutate(Nationality = ifelse(Nationality == "", "Missing", Nationality)) %>%
              left_join(country_to_region, by = c("Nationality" = "Country")) %>%
              rename("Region - Nationality" = "Region") %>%
              mutate(`Region - Nationality` = factor(`Region - Nationality`, 
                                                     levels = c("UK", "Western Europe",
                                                                "Eastern Europe", "Americas",
                                                                "Africa & Middle East", "Asia")))

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## GROUP Country of Birth
injuries <- injuries %>%
              mutate(`Country of birth` = ifelse(`Country of birth` == "", "Missing", `Country of birth`)) %>%
              left_join(country_to_region, by = c("Country of birth" = "Country")) %>%
              rename("Region - Birth" = "Region") %>%
              mutate(`Region - Birth` = factor(`Region - Birth`, 
                                               levels = c("UK", "Western Europe",
                                                          "Eastern Europe", "Americas",
                                                          "Africa & Middle East", "Asia")))

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## GROUP teams that are not present all years
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

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## GROUP "small" venues into "Other":
injuries <- injuries %>%
              group_by(home, Venue) %>%
              mutate(game_count = n()) %>%
              ungroup() %>%
              mutate(Venue = ifelse(game_count < 500, "Other", Venue)) %>%
              select(-game_count)


##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## VISUALIZE: Injury count over years & teams
p1 <- injuries %>%
        filter(Team != "Other") %>%
        group_by(Year) %>%
        summarize(`Lost player days` = sum(injury_length)) %>%
        ggplot(aes(x = Year, y = `Lost player days`)) +
          geom_bar(stat = "identity", fill = "firebrick") +
          labs(subtitle = "Total injury length per year",
               caption = "Note: Only teams present in the Premier League \nall years between 2010-2017 are included") +
          story_theme() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- injuries %>%
        filter(Team != "Other") %>%
        group_by(Team) %>%
        summarize(`Lost player days` = sum(injury_length)) %>%
        ggplot(aes(x = Team, y = `Lost player days`)) +
        geom_bar(stat = "identity", fill = "steelblue2") +
        labs(subtitle = "Total injury length per Team") +
        story_theme() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

g <- arrangeGrob(p1,
                 p2,
                 ncol = 2,
                 top = grid::textGrob("Lost player days due to lower body strains", 
                                      gp = grid::gpar(fontfamily = "Garamond", fontsize = 18, lineheight = 1.25),
                                      hjust = .9))

ggsave(filename = paste(path_Figures, "xxx. Lost player days due to lower body strains.jpeg", sep = "/"),
       plot = g,
       device = "jpeg",
       width = 8,
       height = 4,
       dpi = 500)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## VISUALIZE: Injury vs Minutes played in a given game
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
             w = 5, 
             h = 4, 
             save_all = FALSE)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## VISUALIZE time window for injuries
## common sense check
injuries %>%
  summarize(sum(injured),
            sum(injured_7days),
            sum(injured_14days),
            sum(injured_21days),
            sum(injured_28days))

p_future_rates <- injuries %>%
                    summarize(game_count         = n(),
                              injury_rate        = sum(injured)        / game_count,
                              injury_rate_7days  = sum(injured_7days)  / game_count,
                              injury_rate_14days = sum(injured_14days) / game_count,
                              injury_rate_21days = sum(injured_21days) / game_count,
                              injury_rate_28days = sum(injured_28days) / game_count) %>%
                    select(-game_count) %>%
                    rename("Game day"  = "injury_rate") %>%
                    rename("7 days"    = "injury_rate_7days") %>%
                    rename("14 days"   = "injury_rate_14days") %>%
                    rename("21 days"   = "injury_rate_21days") %>%
                    rename("28 days"   = "injury_rate_28days") %>%
                    tidyr::gather(key = "Time Window", value = "Injury Rate") %>%
                    mutate(`Time Window` = factor(`Time Window`, levels = c("Game day", "7 days", "14 days", "21 days", "28 days")),
                           `Injury Rate` = as.numeric(`Injury Rate`) * 100) %>%
                    ggplot(aes(x = `Time Window`, y = `Injury Rate`, label = formatC(`Injury Rate`, digits = 2, format = "f"))) +
                      geom_line(aes(group = 1), colour = "grey40", size = 1.75) +
                      geom_point(colour = "white", size = 12) +
                      geom_text() +
                      scale_y_continuous(limit = c(0, 4.0), labels = function(x) sprintf("%.2f", x)) +
                      labs(title = "Average injury rates by time windows",
                           y = "Injury Rate (%)",
                           x = "Time Window") +
                      technical_theme() +
                      theme(axis.text.x = element_text(angle = 45, hjust = 1),
                            panel.grid  = element_blank(),
                            axis.ticks = element_line(),
                            axis.line = element_line())


f_conditional_ggsave(save = T,
                     p = p_future_rates,
                     filepath = paste(path_Figures, "xxx. Injury rates by Time Windows.jpeg", sep = "/"),
                     w = 6,
                     h = 4)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## VISUALIZE: Injury vs Age
p_hamstring_vs_age <- injuries %>%
                        mutate(`Age (Years)` = cut(`Age (Years)`, 
                                                   breaks = c(-Inf, 20, 22.5, 25, 27.5, 30, 32.5, 35, Inf), 
                                                   include.lowest = T)) %>%
                        f_plot_ir(s_x     = "`Age (Years)`", 
                                  s_title = "Injuries vs Age")

f_save_plots(save_plots, p_hamstring_vs_age, "8", "Injuries vs Age", 5, 4)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## VISUALIZE: By Weight
p_hamstring_vs_weight <- injuries %>%
                          mutate(`Weight (kg)` = cut(`Weight (kg)`, 
                                                     breaks = c(-Inf, 60, 65, 70, 75, 80, 85, 90, Inf), 
                                                     include.lowest = T)) %>%
                          f_plot_ir(s_x     = "`Weight (kg)`", 
                                    s_title = "Injuries vs Weight")

f_save_plots(save_plots, p_hamstring_vs_weight, "9", "Injuries vs Weight", 5, 4)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## VISUALIZE: By Height
p_hamstring_vs_height <- injuries %>%
                          mutate(`Height (cm)` = cut(`Height (cm)`, 
                                                     breaks = seq(0, 220, by = 5), 
                                                     include.lowest = T)) %>%
                          f_plot_ir(s_x     = "`Height (cm)`", 
                                    s_title = "Injuries vs Height")

f_save_plots(save_plots, p_hamstring_vs_height, "10", "Injuries vs Height", 5, 4)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## VISUALIZE: By BMI
p_hamstring_vs_bmi <- injuries %>%
                        mutate(BMI = cut(BMI, 
                                         breaks = c(-Inf, 20, 21, 22, 23, 24, 25, 26, 27, Inf), 
                                         include.lowest = T)) %>%
                        f_plot_ir(s_x     = "BMI", 
                                  s_title = "Injuries vs BMI")

f_save_plots(save_plots, p_hamstring_vs_bmi, "11", "Injuries vs BMI", 5, 4)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## VISUALIZE: By Kick-off time
##TODO: something is not right, last break should be 24.. only works with Inf now
p_hamstring_vs_kickoff <- injuries %>%
                            f_plot_ir(s_x     = "`Kick-off`", 
                                      s_title = "Injuries vs Kick-off Time")

f_save_plots(save_plots, p_hamstring_vs_kickoff, "12", "Injuries vs Kick-off Time", 5, 4)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## VISUALIZE: By Year
p_hamstring_vs_year <- injuries %>%
                          f_plot_ir(s_x     = "Year", 
                                    s_title = "Injuries vs Year")

f_save_plots(save_plots, p_hamstring_vs_year, "13", "Injuries vs Year", 5, 4)
                          
##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## VISUALIZE: By Month
p_hamstring_vs_month <- injuries %>%
                          f_plot_ir(s_x     = "Month",
                                    s_title = "Injuries vs Month")

f_save_plots(save_plots, p_hamstring_vs_month, "14", "Injuries vs Month", 5, 4)

## TODO: Break-out Month by Year

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## VISUALIZE: By Weekday
p_hamstring_vs_weekday <- injuries %>%
                            f_plot_ir(s_x     = "Weekday", 
                                      s_title = "Injuries vs Weekday")

f_save_plots(save_plots, p_hamstring_vs_weekday, "15", "Injuries vs Weekday", 5, 4)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## VISUALIZE: By Venue
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

f_save_plots(save_plots, p_hamstring_vs_venue, "16", "Injuries vs Venue", 6, 7)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## VISUALIZE: By nationality (grouped)
p_hamstring_vs_nationality <- injuries %>%
                                f_plot_ir(s_x = "`Region - Nationality`",
                                          s_title = "Injuries vs Nationality (Regionalized)")

f_save_plots(save_plots, p_hamstring_vs_nationality, "17", "Injuries vs Nationality (Regionalized)", 5, 4)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## VISUALIZE: By country of birth (grouped)
p_hamstring_vs_birth_country <- injuries %>%
                                  f_plot_ir(s_x = "`Region - Birth`",
                                            s_title = "Injuries vs Birth Country (Regionalized)")

f_save_plots(save_plots, p_hamstring_vs_birth_country, "18", "Injuries vs Birth Country (Regionalized)", 5, 4)


##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## VISUALIZE: By Team
p_hamstring_vs_team <- injuries %>%
                        f_plot_ir(s_x = "Team",
                                  s_title = "Injury rate vs Team",
                                  s_facet = "home")

f_save_plots(save_plots, p_hamstring_vs_team, "19", "Injuries vs Team", 6, 7)


##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## VISUALIZE: By Opponent
p_hamstring_vs_opponent <- injuries %>%
                              f_plot_ir(s_x = "Opponent",
                                        s_title = "Injuries vs Opponent",
                                        s_facet = "home")

f_save_plots(save_plots, p_hamstring_vs_opponent, "20", "Injuries vs Opponent", 6, 7)


##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## VISUALIZE: By player history
## TODO

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## VISUALIZE: By PL Games in Season
p_hamstring_vs_pl_games_season <- injuries %>%
                                    mutate(pl_games_season = cut(pl_games_season, 
                                                               breaks = c(-Inf, 5, 10, 15, 20, 25, Inf), 
                                                               include.lowest = T)) %>%
                                    f_plot_ir(s_x     = "pl_games_season", 
                                              s_title = "Injuries vs PL Games in Season")

f_save_plots(save_plots, p_hamstring_vs_pl_games_season, "21", "Injuries vs PL Games in Season", 5, 4)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## VISUALIZE: By All Games in Season
p_hamstring_vs_all_games_season <- injuries %>%
                                    mutate(all_games_season = cut(all_games_season, 
                                                                 breaks = c(-Inf, 5, 10, 15, 20, 25, Inf), 
                                                                 include.lowest = T)) %>%
                                    f_plot_ir(s_x     = "all_games_season", 
                                              s_title = "Injuries vs All Games in Season")

f_save_plots(save_plots, p_hamstring_vs_all_games_season, "22", "Injuries vs All Games in Season", 5, 4)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## VISUALIZE: By Non-PL Games in Season
p_hamstring_vs_nonpl_games_season <- injuries %>%
                                      mutate(non_pl_games_season = cut(non_pl_games_season,
                                                                   breaks = c(-Inf, 2.5, 5, 7.5, 10, 12.5, 15, Inf),
                                                                   include.lowest = T)) %>%
                                      f_plot_ir(s_x     = "non_pl_games_season", 
                                                s_title = "Injuries vs Non-PL Games in Season")

f_save_plots(save_plots, p_hamstring_vs_nonpl_games_season, "23", "Injuries vs Non-PL Games in Season", 5, 4)
                                      


##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## For now, keep only these variables:
names(injuries)
original_sample <- injuries %>% 
                      select(mid, 
                             pid,
                             Year,
                             injured,
                             injured_7days,
                             injured_14days,
                             injured_21days,
                             injured_28days,
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



##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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



##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## save data for modeling
saveRDS(object = original_sample, file = paste(path_Data, "original_sample.RDS", sep = "/"))
