# function to read traktor (.nml) collection
read_traktor_collection <- function(x) {
  
  # read collection file
  collection <- xml2::read_xml(x = x)
  
  # extract collection entries
  collection_entries <- xml2::xml_child(collection, search = 3) %>%
    xml2::xml_find_all(xpath = ".//ENTRY")
  
  # extract parent attrs
  parents <- tibble::tibble(
    track_title = xml2::xml_attr(collection_entries, "TITLE"),
    artist_name = xml2::xml_attr(collection_entries, "ARTIST")
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
  data <- dplyr::bind_cols(parents, album, tempo, info)
  
  # fix col classes
  data <- data %>%
    mutate_at(c("import_date", "last_played"), lubridate::ymd) %>%
    mutate_at(c("bpm", "track_length", "play_count"), as.numeric) %>%
    mutate(release_year = lubridate::year(lubridate::ymd(release_year)))
  
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
