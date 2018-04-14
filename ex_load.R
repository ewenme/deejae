
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
playlist_data <- map(traktor_history_files, read_traktor_history)

foo <- bind_rows(playlist_data)
