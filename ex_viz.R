
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

traktor_collection <- traktor_collection %>%
  filter(release_year <= year(Sys.Date()),
         bpm <= 300)

# COLLECTION VIZ --------------------------------------------------------

# release year density plot
ggplot(traktor_collection, aes(release_year, ..count..)) +
  geom_density()

# import date density plot
ggplot(data = traktor_collection, aes(import_date, ..count..)) +
  geom_density()

# bpm density plot
ggplot(traktor_collection, aes(bpm, ..count..)) +
  geom_density() +
  theme_ipsum(base_family = "Work Sans Light", grid = "Y",
              base_size = 16) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"),
        axis.title.x = element_text(size = 20, vjust=-0.35)) +
  coord_cartesian(xlim = c(min(traktor_collection$bpm),
                           max(traktor_collection$bpm)+50)) +
  scale_x_continuous(limits = c(min(traktor_collection$bpm),
                                max(traktor_collection$bpm)))


# SET VIZ ------------------------------------------------------------------

ex_set <- filter(playlist_data, start_date == max(start_date))

# plot set progress
ggplot(ex_set, aes(y=track_no, x=set_time, xend=set_time+duration,
                   label=paste3(artist_name, track_title))) +
  geom_dumbbell(size=2, size_x = 2, size_xend = 2,
                color="#e3e2e1", colour_x = "#ED5B67", colour_xend = "#91C5CB") +
  geom_text_repel(nudge_x = max(ex_set$start_time), size=3, segment.size = 0,
                  direction = "y") +
  scale_y_continuous(trans = "reverse", breaks = unique(ex_set$track_no)) +
  scale_x_time() +
  theme_ipsum(grid = FALSE) 
  