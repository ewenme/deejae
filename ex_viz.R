
# SETUP -------------------------------------------------------------------

# load packages
library(ggplot2)
library(dplyr)
library(lubridate)
library(hrbrthemes)
library(DT)

# get functions/data
source("ex_load.R")


# EXPERIMENTAL VIZ --------------------------------------------------------

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


# JS TABLES ---------------------------------------------------------------

datatable(traktor_collection, style = "default")

