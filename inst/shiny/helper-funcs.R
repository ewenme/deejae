# function to read traktor (.nml) collection
read_traktor_collection <- function(x) {

  # read collection file
  collection <- read_xml(x = x) %>%
    xml_child(search = "COLLECTION") %>%
    xml_find_all(xpath = ".//ENTRY")

  # extract various attrs
  location <- xml_find_first(collection, xpath = ".//LOCATION")
  album <- xml_find_first(collection, xpath = ".//ALBUM")
  tempo <- xml_find_first(collection, xpath = ".//TEMPO")
  info <- xml_find_first(collection, xpath = ".//INFO")

  # merge attrs into a data frame
  df <- tibble(
    track_title = xml_attr(collection, "TITLE"),
    artist_name = xml_attr(collection, "ARTIST"),
    album_title = xml_attr(album, "TITLE"),
    uid = paste0(xml_attr(location, "VOLUME"), xml_attr(location, "DIR"),
                 xml_attr(location, "FILE")),
    bpm = xml_attr(tempo, "BPM"),
    genre = xml_attr(info, "GENRE"),
    release_year = xml_attr(info, "RELEASE_DATE"),
    track_length = xml_attr(info, "PLAYTIME"),
    play_count = xml_attr(info, "PLAYCOUNT"),
    import_date = xml_attr(info, "IMPORT_DATE"),
    last_played = xml_attr(info, "LAST_PLAYED"),
    key = xml_attr(info, "KEY")
  )

  # fix col classes
  df <- df %>%
    mutate_at(c("import_date", "last_played"), ymd) %>%
    mutate_at(c("bpm", "track_length", "play_count"), as.numeric) %>%
    mutate(release_year = year(ymd(release_year)),
           track_length_formatted = secondsToString(track_length))

  return(df)

}

# function to read traktor history (.nml) file
read_traktor_history <- function(x) {

  # get track data
  track_df <- read_traktor_collection(x)

  # get playlist data
  playlist_data <- read_xml(x) %>%
    xml_child("PLAYLISTS") %>%
    xml_find_all(xpath = ".//ENTRY")

  # extract key attrs
  key <- xml_find_first(playlist_data, xpath = ".//PRIMARYKEY")
  extended <- xml_find_first(playlist_data, xpath = ".//EXTENDEDDATA")

  # create playlist data frame
  playlist_df <- tibble(
    uid = xml_attr(key, "KEY"),
    record_type = xml_attr(key, "TYPE"),
    deck = xml_attr(extended, "DECK"),
    duration = xml_attr(extended, "DURATION"),
    public = xml_attr(extended, "PLAYEDPUBLIC"),
    start_date = xml_attr(extended, "STARTDATE"),
    start_time = xml_attr(extended, "STARTTIME")
  )

  # join track / playlist info
  data <- inner_join(track_df, playlist_df, by = "uid")

  # fix col classes
  data <- data %>%
    mutate_at(c("duration", "deck", "public", "start_time", "start_date"), as.numeric) %>%
    # remove non-public plays
    filter(public == 1)

  return(data)

}


# function to convert seconds values to string
secondsToString <- function(x, digits=2){
  unlist(
    lapply(x,
           function(i){
             # fractional seconds
             fs <- as.integer(round((i - round(i))*(10^digits)))
             fmt <- ''
             if (i >= 3600)
               fmt <- '%H:%M:%S'
             else if (i >= 60)
               fmt <- '%M:%S'
             else
               fmt <- '%OS'

             i <- format(as.POSIXct(strptime("0:0:0","%H:%M:%S")) + i, format=fmt)
             if (fs > 0)
               sub('[0]+$','',paste(i,fs,sep='.'))
             else
               i
           }
    )
  )
}


tidy_selections <- function(x) {

  df <- x %>%
    # arrange by set date / start time
    arrange(set_date, start_time) %>%
    group_by(set_date) %>%
    mutate(
      # set track no. field
      track_no = row_number(),
      # add set time field
      set_time = (start_time - first(start_time)),
      # set max duration of last two tracks to 15 mins
      duration = if_else(track_no >= max(track_no) - 1 & duration > 900,
                         900, duration),
      # add track end time field
      end_time = set_time + duration,
      # calc gap b/w start time & e/o prev. track
      gap = abs(set_time - lag(end_time)),
      gap = if_else(is.na(gap), 0, gap),
      # define set 'break' as gap > 15 mins
      set_break = if_else(gap > 900, 1, 0),
      # rename set dates if new set
      new_set = cumsum(set_break)) %>%
    # remove sets smaller than five tracks
    group_by(set_date, new_set) %>%
    filter(n() >= 5) %>%
    group_by(set_date) %>%
    mutate(
      new_set = cumsum(set_break),
      set_date_formatted = if_else(
        new_set == 0, paste(set_date_formatted),
        paste0(set_date_formatted, " (", new_set, ")")
      )
    ) %>%
    ungroup() %>%
    # remove intermediary fields
    select(-new_set, -set_break, -gap)

  df <- df %>%
    # reset set time fields (now new sets defined)
    group_by(set_date_formatted) %>%
    mutate(
      # set track no. field
      track_no = row_number(),
      # add set time field
      set_time = (start_time - first(start_time)),
      # set max duration of last two tracks to 15 mins
      duration = if_else(
        track_no >= max(track_no) - 1 & duration > 900,
        900, duration
      ),
      # add track end time field
      end_time = set_time + duration,
      # add 'set quarter' field
      set_stage = ntile(set_time, n=4)
    ) %>%
    # separate sets if >= 5 mins silence
    ungroup()

  return(df)
}

# function for pasting together strings and ignoring NAs
paste3 <- function(...,sep=" - ") {
  L <- list(...)
  L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
  ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
             gsub(paste0(sep,sep),sep,
                  do.call(paste,c(L,list(sep=sep)))))
  is.na(ret) <- ret==""
  ret
}
