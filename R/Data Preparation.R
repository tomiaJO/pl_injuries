##SETUP
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(lubridate)

library(caret)

rm(list = ls())
injuries_raw <- fread("./../Data/injury_soccer/injury_data_pg.csv") 

#EDA functions
source("f_sec_scale.R")
source("f_breakdown_by_bins.R")
source("f_breakdown_by_variable.R")

##Formatting clean-up for date variables
injuries <- injuries_raw %>%
              mutate(Date = as.Date(Date, "%Y-%m-%d"),
                     `Date of birth` = as.Date(`Date of birth`, "%Y-%m-%d")) %>%
              mutate(age_in_years = time_length(difftime(Date, `Date of birth`), "years")) 

##Visualize: Injury trend over years
p<- injuries %>%
      mutate(Year = year(Date)) %>%
      group_by(Year) %>%
      summarize(game_count = n(),
                injury_count = (sum(injury_type != 0))) %>%
      ungroup() %>%
      mutate(injury_rate_pct = injury_count / game_count * 100) %>%
      f_breakdown_by_bins(s_x = "Year", s_title = "Injury data is missing pre-2010")

ggsave(filename = "Injury Trend Over Years.jpeg", plot = p, device = "jpeg", 
       dpi = 1600, width = 6, height = 4)

##Visualize: Injury type counts
p2 <- injuries %>%
        filter(injury_type != '0') %>%
        count(injury_type, sort = T) %>%
        mutate(injury_type = ifelse(n < 25, 'Other', injury_type)) %>%
        mutate(injury_type = reorder(injury_type, n)) %>%
        group_by(injury_type) %>%
        summarize(n = sum(n)) %>%
        ungroup() %>%
        ggplot(aes(x = injury_type, y = n)) +
        geom_bar(stat = "identity", fill = "lightblue") +
        geom_text(aes(label = n), hjust = 1.05, vjust = 0.325, fontface = "bold") +
        coord_flip() + 
        labs(title    = "The most common injury is to the hamstring",
             subtitle = "Number of injuries by type",
             x        = "Injury Type",
             y        = "Count of injuries",
             caption  = "Note: Injuries that happened less than 25x are group as 'Other'") + 
        theme_minimal() +
        theme(panel.grid.minor = element_blank()) +
        theme(plot.subtitle = element_text(color = "gray60"),
              plot.caption  = element_text(color = "gray60")) +
        theme(axis.title.x = element_text(hjust = 0.36))

ggsave(filename = "Injury Types.jpeg", plot = p2, device = "jpeg", 
       dpi = 1600, width = 6, height = 4)


install.packages("benchmarkme")
library(benchmarkme)

res <- benchmark_std(runs = 3)
plot(res)
