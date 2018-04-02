
# SETUP -------------------------------------------------------------------

# set traktor folder
traktor_dir <- "/Users/Ewen/Documents/Native Instruments/Traktor 2.11.0/"

# get functions
source("extract_funcs.R")


# LOAD --------------------------------------------------------------------

# load collection
traktor_collection <- read_traktor_collection(x = paste0(traktor_dir, "collection.nml"))

# list files in traktor history dir
traktor_hist_files <- list.files(paste0(traktor_dir, "History"))

# try reading NML files
nml_files <- map(hist_dir_files, function(x) read_xml(paste0(hist_dir, "/", x)))


# CLEAN -------------------------------------------------------------------




