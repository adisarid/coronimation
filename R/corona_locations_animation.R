# Generating an animation of where Corona visited in Israel
# The script generates a static map and an animation of corona locations in Israel.

library(tidyverse)

corona_db <- read_csv("data/2020-03-13 - corona_locations.csv") %>% 
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
  mutate(timestamp_log = lubridate::dmy(timestamp))

israel_boundaries <- borders(database = "world", regions = "israel",
                             size = 0.5)

# Creates a static map ----------------------------------------------------

static_coronamap <- ggplot(corona_db, aes(x, y)) + 
  israel_boundaries + 
  geom_point() +
  coord_equal() + 
  theme_void()


# Making it dynamic with gganimate ----------------------------------------

# This is just the "Vanilla version of gganimate. We'll try later on to improve even further

library(gganimate)

coronimation <- ggplot(corona_db, aes(x, y, group = object_id)) +
  labs(title = "נקודות חשיפה לקורונה בישראל",
       subtitle = "{closest_state}",
       caption = "\u202bמבוסס על נתוני משרד הבריאות http://bit.ly/corona_il\n
       \u202bנוצר על ידי עדי שריד adi@sarid-ins.co.il") +
  geom_point() +
  coord_equal() + 
  transition_states(timestamp_log, transition_length = 1, state_length = 1) + 
  shadow_mark() + 
  enter_fade() + 
  exit_reset() +
  israel_boundaries + 
  saridr::theme_sarid() +
  theme(plot.title = element_text(hjust = 1),
        plot.subtitle = element_text(hjust = 1),
        axis.text = element_blank(),
        axis.title = element_blank())
