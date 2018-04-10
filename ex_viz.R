
# SETUP -------------------------------------------------------------------

# load packages
library(ggplot2)
library(dplyr)
library(lubridate)
library(hrbrthemes)
library(DT)
library(ggalt)
library(ggrepel)

# get functions/data
source("ex_load.R")


# COLLECTION VIZ --------------------------------------------------------

# release year density plot
traktor_collection %>%
  filter(release_year <= year(Sys.Date())) %>%
  ggplot(aes(release_year, ..count..)) +
  geom_density()

# import date density plot
ggplot(data = traktor_collection, aes(import_date, ..count..)) +
  geom_density()

# bpm density plot
traktor_collection %>%
  filter(bpm <= 300) %>%
  ggplot(aes(bpm, ..count..)) +
  geom_density()


# SET VIZ ------------------------------------------------------------------

ex_set <- filter(playlist_data, start_date == max(start_date))

# plot set progress
ggplot(ex_set, aes(y=track_no, x=set_time, xend=set_time+duration,
                   label=paste(artist_name, "-", track_title))) +
  geom_dumbbell(size=2, size_x = 2, size_xend = 2,
                color="#e3e2e1", colour_x = "#ED5B67", colour_xend = "#91C5CB") +
  geom_text_repel(nudge_x = max(ex_set$start_time), size=3, segment.size = 0) +
  scale_y_continuous(trans = "reverse", breaks = unique(ex_set$track_no)) +
  scale_x_time() +
  theme_ipsum(grid = FALSE) 
  