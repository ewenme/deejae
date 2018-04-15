
# SETUP -------------------------------------------------------------------

# packages
library(dplyr)
library(xml2)
library(lubridate)
library(purrr)
library(stringr)

# set traktor folder
traktor_dir <- "/Users/Ewen/Documents/Native Instruments/Traktor 2.11.0/"

# get traktor history files
traktor_history_files <- list.files(paste0(traktor_dir, "History/"),
                                    full.names = TRUE)

# get functions
source("extract_funcs.R")


# LOAD --------------------------------------------------------------------

# load traktor collection
traktor_collection <- read_traktor_collection(x = "./example_data/traktor_collection.nml")

# load rekordbox collection
rekordbox_collection <- read_rekordbox_collection(x = "./example_data/rekordbox_collection.xml")

# load traktor history
playlist_data <- map(traktor_history_files, read_traktor_history) %>%
  bind_rows()


# CLEAN ----------------------------------------------------------------------

playlist_data <- playlist_data %>%
  # arrange by start time
  arrange(start_date, start_time) %>%
  # create track_no of set field
  group_by(start_date) %>%
  mutate(track_no = row_number()) %>%
  # add set time field
  mutate(set_time=(start_time - first(start_time)),
         end_time=set_time+duration) %>% 
  ungroup()

# separate sets with silence
foo <- playlist_data %>%
  group_by(start_date) %>%
  mutate(gap = abs(set_time - lag(end_time)),
         gap = if_else(is.na(gap), 0, gap),
         set_break = if_else(gap >= 360, 1, 0),
         new_set = cumsum(set_break),
         new_set_formatted = if_else(new_set == 0, paste(start_time),
                                     paste0(start_time, " (", new_set, ")")))
