library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)

rm(list = ls())
injuries_raw <- fread("./../Data/injury_soccer/injury_data_pg.csv") 

head(injuries_raw)
glimpse(injuries_raw)

injuries <- injuries_raw %>%
                  select(Date,                 `Kick-off`,        mid,
                         pid,                  `First name`,     `Last name`,       minutes,
                         injured,               injury_length,    injury_type,
                         `Country of birth`,    Nationality,     `Date of birth`,
                         Position,              Foot,             Height,           Weight,
                         starts_with("all_"))


##Data Cleanings

##HEIGHT
##note: it's fine to remove missing Height data - only 6 injuries
injuries %>%
  mutate(height_available = !(is.null(Height) | Height == "")) %>%
  group_by(height_available) %>%
  summarize(total_count  = n(),
            injury_count = sum(injured))

injuries <- injuries %>%
              filter(!(is.null(Height) | Height == "")) %>%
              mutate(`Height (cm)` = gsub(pattern = " cm", replacement = "", x = Height)) %>%
              mutate(`Height (cm)` = as.numeric(`Height (cm)`)) %>%
              select(everything(), -Height)

##WEIGHT
##note: it's fine to remove missing Weight data - only 9 injuries
injuries %>%
  mutate(weight_available = !(is.null(Weight) | Weight == "")) %>%
  group_by(weight_available) %>%
  summarize(total_count  = n(),
            injury_count = sum(injured))

injuries <- injuries %>%
              filter(!(is.null(Weight) | Weight == "")) %>%
              mutate(`Weight (kg)` = gsub(pattern = " kg", replacement = "", x = Weight)) %>%
              mutate(`Weight (kg)` = as.numeric(`Weight (kg)`)) %>%
              select(everything(), -Weight)

injuries %>%
  select(everything(), -pid, -mid, -`Last name`, `First name`) %>%
  tidyr::gather(variable, value, -injured) %>%
  ggplot(aes(y = as.factor(variable), 
             fill = as.factor(injured), 
             x = percent_rank(value))) +
  geom_density_ridges(alpha = .2)

##metric: injury / minute?

## CHECK: INJURY TYPES
injuries %>%
  filter(!injury_type == 0) %>%
  count(injury_type, sort = TRUE) %>%
  mutate(injury_type = ifelse(n>=20, injury_type, "other")) %>%
  mutate(injury_type = reorder(injury_type, n)) %>%
  ggplot(aes(x = injury_type, y = n)) +
    geom_bar(stat = "identity") +
    coord_flip()

##CHECK: by avg. minutes played before the game


## CHECK: PLAYER HEIGHT
#by player count & injury rate per game
injuries %>%
  mutate(`Height group` = cut(`Height (cm)`, 
                              seq(from = 160, by = 5, to = 205))) %>%
  group_by(`Height group`) %>%
  summarize(`Player Count` = n_distinct(pid),
            `Injury Rate`  = sum(injured) / n()) %>%
  #mutate(Height = reorder(Height, n)) %>%
  ggplot(aes(x = `Height group`, group = 1)) +
    geom_bar(aes(y = `Player Count`), stat = "identity") +
    geom_line(aes(y = `Injury Rate` * 25000)) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal()

#by minutes
injuries %>%
  mutate(`Height group` = cut(`Height (cm)`, 
                              seq(from = 160, by = 5, to = 205))) %>%
  group_by(`Height group`) %>%
  summarize(`Minutes played` = sum(minutes),
            `Injury Rate`    = sum(injured) / sum(minutes)) %>%
  #mutate(Height = reorder(Height, n)) %>%
  ggplot(aes(x = `Height group`, group = 1)) +
  geom_bar(aes(y = `Minutes played`), stat = "identity") +
  geom_line(aes(y = `Injury Rate` * 10000000000)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()


## CHECK: PLAYER HEIGHT
#by player count & injury rate per game
injuries %>% 
  mutate(`Weight group` = cut(`Weight (kg)`, 
                              seq(from = 50, by = 5, to = 120))) %>%
  group_by(`Weight group`) %>%
  summarize(`Player Count` = n_distinct(pid),
            `Injury Rate`  = sum(injured) / n()) %>%
  #mutate(Height = reorder(Height, n)) %>%
  ggplot(aes(x = `Weight group`, group = 1)) +
  geom_bar(aes(y = `Player Count`), stat = "identity") +
  geom_line(aes(y = `Injury Rate` * 25000)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

## CHECK: PLAYER HEIGHT vs WEIGHT
#TODO: color by if player was ever injured
injuries %>%
  distinct(pid, `Height (cm)`, `Weight (kg)`) %>%
  # mutate(`Height group` = cut(`Height (cm)`, 
  #                             seq(from = 160, by = 5, to = 205))) %>%
  # mutate(`Weight group` = cut(`Weight (kg)`, 
  #                             seq(from = 50, by = 5, to = 120))) %>%
  # group_by(`Height group`, `Weight group`) %>%
  # summarize(n=n()) %>%
  ggplot(aes(x=`Height (cm)`, y=`Weight (kg)`)) +
    geom_point(alpha=0.25) +
    geom_smooth()


injuries %>% 
  group_by(Position, pid, `Height (cm)`, `Weight (kg)`) %>%
  summarize(ever_injured = max(injured)) %>%
  ungroup() %>%
  # summarize(n = n(),
  #           injured_n = sum(ever_injured))
  mutate(ever_injured = ifelse(ever_injured == 1, T, F)) %>%
  ggplot(aes(x=`Height (cm)`, y=`Weight (kg)`)) +
    geom_point(alpha=0.65, aes(color = ever_injured)) +
    geom_smooth() +
    facet_grid(~Position) +
    theme_minimal()

##bmi (grouped) chart with injury rate as Y axis
injuries %>%
  filter(!is.na(Position), Position != "") %>%
  mutate(bmi = `Weight (kg)` / (`Height (cm)` / 100)^2) %>%
  mutate(`bmi group` = cut(`bmi`, 
                            breaks = c(-Inf, 18, 20, 22, 24, 26, 28, Inf), include.lowest = T)) %>%
  group_by(Position, `bmi group`) %>%
  summarize(`Player Count` = n_distinct(pid),
            `Injury Rate`  = sum(injured) / n()) %>%
  #mutate(Height = reorder(Height, n)) %>%
  ggplot(aes(x = `bmi group`, group = 1)) +
    geom_bar(aes(y = `Player Count`), stat = "identity") +
    geom_line(aes(y = `Injury Rate` * 25000)) +
    facet_wrap(~Position) %>%
    scale_y_continuous(labels = scales::comma) +
    theme_minimal()

## CHECK: by position
## CHECK: by nationality
## CHECK: by age
## CHECK: by previous injury history
## CHECK: minutes when injuries happen --> heatmap? by half?