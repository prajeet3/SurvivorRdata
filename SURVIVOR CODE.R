#setup
library(tidyverse)
library(lubridate)


#import

summary <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/summary.csv')
spec(summary)


#inspect
dim(summary)
names(summary)
glimpse(summary)


#wrangle data
summary %>%
  mutate(across(where(is.character), as.factor))

summary %>% 
  mutate(days_filming = time_length(interval(filming_started, filming_ended), 
                                    unit = "days")) %>%
  mutate(days_aired = time_length(interval(premiered, ended), 
                                  unit = "days")) %>%
  select(-premiered:-filming_ended) %>%
  
  glimpse()

#pivot data -> combine for our show type
summary %>%
  pivot_longer(
    col = viewers_premier:viewers_reunion,
    names_to = "show_type",
    names_prefix = "viewers_",
    values_to = "total_views",
    values_drop_na = TRUE) %>%
  glimpse()

summary_tidy <- summary %>%
  mutate(across(where(is.character), as.factor)) %>%
  
  mutate(days_filming = time_length(interval(filming_started, filming_ended), 
                                    unit = "days")) %>%
  mutate(days_aired = time_length(interval(premiered, ended), 
                                  unit = "days")) %>%
  select(-premiered:-filming_ended) %>%
  pivot_longer(
    col = viewers_premier:viewers_reunion,
    names_to = "show_type",
    names_prefix = "viewers_",
    values_to = "total_views",
    values_drop_na = TRUE) %>%
  glimpse()


location %>%
  glimpse()
#scatterplot looking at relationship between viewers and country


ggplot(summary_tidy, aes(x = viewers_mean, y = total_views)) +
  geom_point(aes(color = country)) +
  geom_smooth(method = lm)

#scatterplot looking at total views over seasons

ggplot(summary_tidy, aes(x = season, y = total_views)) +
  geom_point(aes(color = country)) +
  geom_smooth(method = lm)
