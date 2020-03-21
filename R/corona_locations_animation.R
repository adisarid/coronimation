# Generating an animation of where Corona visited in Israel
# The script generates a static map and an animation of corona locations in Israel.

library(tidyverse)

corona_db <- map_df(dir("data/", full.names = T), read_csv) %>% 
  distinct(OBJECTID, .keep_all = T) %>% 
  set_names(c("object_id",
              "patient_name",
              "location",
              "comments",
              "timestamp",
              "hours",
              "location_type",
              "x",
              "y"
              )) %>% 
  mutate(timestamp_log = lubridate::dmy(timestamp)) %>%
  mutate(is_tourist = ifelse(str_detect(patient_name, "תייר"),
                             "תייר",
                             "מקומי"))

israel_boundaries <- borders(database = "world", regions = "israel",
                             size = 0.5)

# Creates a static map ----------------------------------------------------

static_coronamap <- ggplot(corona_db, aes(x, y)) + 
  israel_boundaries + 
  geom_point() +
  coord_equal() + 
  theme_void() + 
  guides(color = guide_legend(""))


# Making it dynamic with gganimate ----------------------------------------

# This is just the "Vanilla version of gganimate. We'll try later on to improve even further

library(gganimate)

coronimation <- ggplot(corona_db, aes(x, y, group = object_id)) +
  labs(title = "Corona exposure in Israel",
       subtitle = "{closest_state}",
       caption = "Based on MOH data, see http://bit.ly/corona_il\n
       Created by Adi Sarid https://adisarid.github.io") +
  geom_point() +
  coord_equal() + 
  transition_states(timestamp_log, transition_length = 0.5, state_length = 0) + 
  enter_fade() + 
  shadow_mark(color = "grey") +
  israel_boundaries + 
  saridr::theme_sarid() +
  guides(color = guide_legend("")) +
  theme(plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(hjust = 0),
        axis.text = element_blank(),
        axis.title = element_blank())
