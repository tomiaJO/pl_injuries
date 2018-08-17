##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Initialize R setup
source("GlobalStartup.R")

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load data
injuries <- fread(paste(path_RawData, "injury_data_pg.csv",     sep = "/"))



##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## FIX Date
injuries <- injuries %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d"),
                     `Date of birth` = as.Date(`Date of birth`, "%Y-%m-%d")) %>%
              mutate(`Age (Years)` = time_length(difftime(Date, `Date of birth`), "years")) 

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## FIX home
injuries <- injuries %>% 
              mutate(home = factor(home, levels = c(1, 0), labels = c("Home", "Away")))

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## REMOVE YEAR
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

injuries <- injuries %>%
              filter(Year %in% c(2010:2017))

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## REMOVE missing height & weight
injuries <- injuries %>%
              filter(!(is.null(Height) | Height == "")) %>%
              filter(!(is.null(Weight) | Weight == ""))

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## REMOVE missing foot
injuries <- injuries %>% 
              filter(Foot != "")

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## REMOVE goalkeepers & missing position
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


g <- arrangeGrob(p_by_position1, p_by_position2, layout_matrix = rbind(c(1,1,1,2,2)))

f_conditional_ggsave(save = save_plots, 
                     p = g, 
                     filepath = paste(path_Figures, "6. Injuries vs Position.jpeg", sep = "/"), 
                     w = 6, 
                     h = 4.5)

injuries <- injuries %>%
              #Note1 - Goalkeepers: very different dynamics, as shown by the Injury rate %
              filter(Position != "Goalkeeper") %>%
              #Note2 - Missing: don't trust that data very much
              filter(Position != "")

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## REMOVE non "lower body strain" injuries
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
                    labs(title    = "Lower body muscle strains are among the \nmost common injuries",
                         x        = "Injury Type",
                         y        = "Number of injuries",
                         caption  = "Note: Injuries that happened less than 25x are group as 'Other'") +
                    story_theme() + theme(plot.title = element_text(size = 10.5))

f_conditional_ggsave(save = save_plots, 
                     p = p_injury_types, 
                     filepath = paste(path_Figures, "2. Injury Types.jpeg", sep = "/"), 
                     w = 6, 
                     h = 4.5)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## VISUALIZE: injury lengths
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
                      labs(title    = "There is significant variation among \nseriousness of injuries",
                           x        = "Injury Type",
                           y        = "Length of injuries",
                           caption  = "Note: Injuries that happened less than 25x are group as 'Other'") +
                      technical_theme() + 
                      theme(plot.title = element_text(size = 10.5),
                            axis.text.y  = element_blank())

f_conditional_ggsave(save = save_plots, 
                     p = p_injury_lenghts, 
                     filepath = paste(path_Figures, "3. Injury Lengths.jpeg", sep = "/"), 
                     w = 7, 
                     h = 4.5)


f_conditional_ggsave(save = save_plots, 
                     filepath = paste(path_Figures, "3. Injury Types.jpeg", sep = "/"), 
                     p = arrangeGrob(p_injury_types, p_injury_lenghts, layout_matrix = rbind(c(1,1,1,2,2))),
                     w = 10, 
                     h = 4.5)

## Keep only lower body strains
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


##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## REMOVE injury length less than two weeks
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

injuries <- injuries %>%
              filter(injury_type == "0" | injury_length >= 14)

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## REMOVE NA playing times
injuries <- injuries %>%
              filter(!is.na(pl_mins_season)) %>%
              filter(!is.na(all_mins_season))

##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## NOT REMOVE: NA small playing times
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



##### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## SAVE data
saveRDS(object = injuries, file = paste(path_Data, "injuries.RDS", sep = "/"))