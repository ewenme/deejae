
extract_collection_entries <- function(x) {
  
  # extract collection entries
  collection_entries <- xml2::xml_child(x, search = 3) %>%
    xml2::xml_find_all(xpath = ".//ENTRY")
  
  # extract parent attrs
  parents <- data.frame(t(data.frame(as.list(xml2::xml_attrs(collection_entries)))), 
                        stringsAsFactors = F, row.names = NULL)
  
  # extract child attrs
  child_cats <- c("TEMPO", "MUSICAL_KEY")
  
  children <- lapply(child_cats, function(x) 
    data.frame(t(data.frame(as.list(xml2::xml_attrs(xml2::xml_find_all(collection_entries,
                                                                       xpath = paste0(".//", 
                                                                                      x)))),
                            stringsAsFactors = FALSE)), row.names = NULL)
  )
  
  # bind child attrs
  children <- bind_cols(children)
  
  # bind parent/child cols
  entries <- bind_cols(parents, children)
  
}