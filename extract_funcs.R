# function to read traktor (.nml) collection
read_traktor_collection <- function(x) {
  
  # read collection file
  collection <- xml2::read_xml(x = x)
  
  # extract collection entries
  collection_entries <- xml2::xml_child(collection, search = 3) %>%
    xml2::xml_find_all(xpath = ".//ENTRY")
  
  # extract parent attrs
  parents <- tibble::tibble(
    audio_id = xml2::xml_attr(collection_entries, "AUDIO_ID"),
    track_title = xml2::xml_attr(collection_entries, "TITLE"),
    artist_name = xml2::xml_attr(collection_entries, "ARTIST")
  )
  
  # extract loaction attrs
  location_tree <- xml2::xml_find_first(collection_entries, xpath = ".//LOCATION")
  location <- tibble::tibble(
    volume = xml2::xml_attr(location_tree, "VOLUME"),
    dir = xml2::xml_attr(location_tree, "DIR"),
    file = xml2::xml_attr(location_tree, "FILE"),
    uid = paste0(volume, dir, file)
  )
  
  # extract album attrs
  album_tree <- xml2::xml_find_first(collection_entries, xpath = ".//ALBUM")
  album <- tibble::tibble(
    album_title = xml2::xml_attr(album_tree, "TITLE")
  )
  
  # extract tempo attrs
  tempo_tree <- xml2::xml_find_first(collection_entries, xpath = ".//TEMPO")
  tempo <- tibble::tibble(
    bpm = xml2::xml_attr(tempo_tree, "BPM")
  )
  
  # extract info attrs
  info_tree <- xml2::xml_find_first(collection_entries, xpath = ".//INFO")
  info <- tibble::tibble(
    genre = xml2::xml_attr(info_tree, "GENRE"),
    release_year = xml2::xml_attr(info_tree, "RELEASE_DATE"),
    track_length = xml2::xml_attr(info_tree, "PLAYTIME"),
    play_count = xml2::xml_attr(info_tree, "PLAYCOUNT"),
    import_date = xml2::xml_attr(info_tree, "IMPORT_DATE"),
    last_played = xml2::xml_attr(info_tree, "LAST_PLAYED"),
    key = xml2::xml_attr(info_tree, "KEY")
  )
  
  # bind extracted features
  data <- dplyr::bind_cols(parents, location, album, tempo, info)
  
  # fix col classes
  data <- data %>%
    mutate_at(c("import_date", "last_played"), lubridate::ymd) %>%
    mutate_at(c("bpm", "track_length", "play_count"), as.numeric) %>%
    mutate(release_year = lubridate::year(lubridate::ymd(release_year)))
  
  return(data)
  
}


# function to read traktor history (.nml) file
read_traktor_history <- function(x) {
  
  # get track info ------------------------------------
  
  track_data <- read_traktor_collection(x)
  
  # get playlist info --------------------------------------
  
  playlist_data <- xml2::read_xml(x)
  
  playlist_entries <- xml2::xml_child(playlist_data, 5) %>%
    xml2::xml_find_all(xpath = ".//ENTRY")
  
  # extract key attrs
  key_tree <- xml2::xml_find_first(playlist_entries, xpath = ".//PRIMARYKEY")
  key <- tibble::tibble(
    key = xml2::xml_attr(key_tree, "KEY"),
    type = xml2::xml_attr(key_tree, "TYPE")
  )
  
  # extract extended attrs
  extended_tree <- xml2::xml_find_first(playlist_entries, xpath = ".//EXTENDEDDATA")
  extended <- tibble::tibble(
    deck = xml2::xml_attr(extended_tree, "DECK"),
    duration = xml2::xml_attr(extended_tree, "DURATION"),
    extended_type = xml2::xml_attr(extended_tree, "EXTENDEDTYPE"),
    public = xml2::xml_attr(extended_tree, "PLAYEDPUBLIC"),
    start_date = xml2::xml_attr(extended_tree, "STARTDATE"),
    start_time = xml2::xml_attr(extended_tree, "STARTTIME")
  )
  
  # bind extracted features
  playlist_data <- dplyr::bind_cols(key, extended)
  
  # join track/playlist info
  data <- inner_join(track_data, playlist_data, by=c("uid"="key"))
  
  # fix col classes
  data <- data %>%
    mutate_at(c("duration", "deck", "public", "start_time", "start_date"), as.numeric) %>%
    # remove non-public plays
    filter(public == 1) %>%
    # arrange by start time
    arrange(start_date, start_time) %>%
    # create set index
    group_by(start_date) %>%
    mutate(track_no = 1:n()) %>%
    mutate(set_time=round((start_time - first(start_time))/60, digits = 2),
           duration=duration/60) %>% ungroup()
  
  return(data)
  
}


# function to read rekordbox (.xml) collection
read_rekordbox_collection <- function(x) {
  
  # read collection file
  collection <- xml2::read_xml(x = x)
  
  # extract collection entries
  collection_entries <- xml2::xml_child(collection, search = 2) %>%
    xml2::xml_find_all(xpath = ".//TRACK")
  
  # extract attrs
  data <- tibble::tibble(
    track_title = xml2::xml_attr(collection_entries, "Name"),
    artist_name = xml2::xml_attr(collection_entries, "Artist"),
    album_title = xml2::xml_attr(collection_entries, "Album"),
    bpm = xml2::xml_attr(collection_entries, "AverageBpm"),
    genre = xml2::xml_attr(collection_entries, "Genre"),
    release_year = xml2::xml_attr(collection_entries, "Year"),
    track_length = xml2::xml_attr(collection_entries, "TotalTime"),
    play_count = xml2::xml_attr(collection_entries, "PlayCount"),
    import_date = xml2::xml_attr(collection_entries, "DateAdded"),
    key = xml2::xml_attr(collection_entries, "Tonality")
  )
  
  # fix col classes
  data <- data %>%
    mutate_at(c("bpm", "track_length", "play_count", "release_year"), as.numeric) %>%
    mutate(import_date = lubridate::ymd(import_date))
  
  return(data)
  
}


