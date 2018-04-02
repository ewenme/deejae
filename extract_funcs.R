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
    mutate_at(c("bpm", "track_length", "play_count"), as.numeric)
  
  return(data)
  
}
