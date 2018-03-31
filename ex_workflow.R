
# SETUP -------------------------------------------------------------------

library(xml2)
library(dplyr)

hist_dir <- "/Users/Ewen/Documents/Native Instruments/Traktor 2.11.0/History"

# LOAD --------------------------------------------------------------------

# list files in traktor history dir
hist_files <- list.files(hist_dir)

# try reading one
test <- read_xml(x=paste0(hist_dir, "/", hist_files[1]))


# CLEAN -------------------------------------------------------------------




