# package load
library(shiny)
library(shinyalert)
library(shinythemes)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(lubridate)
library(dplyr)
library(stringr)
library(hrbrthemes)
library(purrr)
library(xml2)
library(lubridate)
library(ggplot2)
library(ggalt)
library(ggrepel)

# get data extraction functions
source("extract_funcs.R")

# Define UI for application ------------------------------------------------

ui <- navbarPage(
  
  # overall layout/styles ---------------
  "deejae", theme = shinytheme("paper"),
  selected = "collection", collapsible = TRUE,
  useShinyalert(),  # Set up shinyalert
  shinyjs::useShinyjs(), # set up shinyjs
  tags$head(
    # Include custom CSS
    includeCSS("styles.css"),
    tags$style("#collection_plot{height: calc(100vh - 200px) !important;}"),
    tags$style("#selection_plot{height: calc(100vh - 200px) !important;}")
    ),
  
  # collection UI -----------------
  
  tabPanel("collection", fluidRow(
    
    # user selections
    
    column(3, wellPanel(
      # input: collection type
      conditionalPanel(
        condition = "output.collection_cond == true",
        radioButtons(
          inputId = "collection_type", label = "select collection",
          choices = c(rekordbox = "rekordbox", traktor = "traktor")
          )),
      # input: collection upload
      conditionalPanel(
        condition = "output.collection_cond == true",
        fileInput(
          inputId = "collection_upload", label = "upload collection",
          accept = c(".nml", ".xml"), buttonLabel = "browse",
          placeholder = "  no file selected", multiple = FALSE)
        ),
      # input: x-variable
      conditionalPanel(
        condition = "output.collection_cond == false",
        selectInput(inputId = "xvar", label = "wot 2 plot",
                    c("artists"="artist_name", "albums"="album_title",
                      "BPM"="bpm", "release years"="release_year"),
                    selected = "bpm")
        ),
      # input: import date
      conditionalPanel(
        condition = "output.collection_cond == false",
        sliderInput(inputId = "import_date_slider", label = "date added",
                    min = 2000, max = year(Sys.Date()), step = 1, sep = "",
                    value = c(2000, year(Sys.Date())))
        ))),
    
    # main panel
    
    column(9, tabsetPanel(
      # collection plot view
      tabPanel("visualise",
               tags$br(),
               withSpinner(plotOutput(outputId = "collection_plot"),
                           type = 8)
               ),
      # collection table view
      tabPanel("table view",
               withSpinner(DT::dataTableOutput(outputId = "collection_table"), 
                           type = 8)
      )))
    )),
  
  # selection UI -----------------
  
    # set history page
    tabPanel(title="selection", fluidRow(column(3, wellPanel(
    
    # input: history file upload
    conditionalPanel(
      condition = "output.set_cond == true",
      fileInput(
        inputId = "selection_upload", 
        label = "upload history (traktor only)",
        accept = c(".nml"), buttonLabel = "browse",
        placeholder = "  no file selected", multiple = TRUE
        )
      ),
    
    # input: set or summary switcher
    conditionalPanel(
      condition = "output.set_cond == false",
      radioButtons(
        inputId = "set_view", label = "selection type",
        choices = list("all selections" = 1, "set selection" = 2),
        selected = 1
      )
    ),
    
    tags$hr(),
    
    # set view
    
    # input: select set
    conditionalPanel(
      condition = "output.set_cond == false && input.set_view == 2",
      selectInput(
        inputId = "set_select", label = "choose a set",
        choices = ""
        )
      ),
    
    # all selections view
    
    # input: x-variable
    conditionalPanel(
      condition = "output.set_cond == false && input.set_view == 1",
      selectInput(inputId = "selection_xvar", label = "wot 2 plot",
                  c("artists"="artist_name", "BPM"="bpm", 
                    "release years"="release_year"),
                  selected = "bpm")
      ),
    
    # input: set quarter
    conditionalPanel(
      condition = "output.set_cond == false && input.set_view == 1",
      sliderInput(inputId = "set_quarter", label = "set stage",
                  min = 1, max = 4, value = c(1, 4), step = 1,
                  pre = "Q")
    )
    )),
    
    column(9, tabsetPanel(
      
      # set plot view
      tabPanel("visualise",
               tags$br(),
               withSpinner(plotOutput(outputId = "selection_plot"),
                                                type = 8)
               ),
      # set table view
      tabPanel("table view",
               withSpinner(DT::dataTableOutput(outputId = "selection_table"),
                                                type = 8)
               )))
    )),
     
  # about page -----------------
  
  tabPanel(title="about", 
           fluidRow(column(6, includeMarkdown("about.Rmd")
                           )
                    )
           )
  )


# Define server logic ------------------------------------------------
server <- function(input, output, session) {
  
  # data objects ----------------------------
  
  # uploaded user collection
  collection_data <- reactive({
    
    # check for collection upload
    upload <- input$collection_upload
    if (is.null(upload)) return(NULL)
    
    # read collection data
    if (input$collection_type == "rekordbox") {
      
      df <- read_rekordbox_collection(x = input$collection_upload$datapath)
      
    } else if (input$collection_type == "traktor") {
      
      df <- read_traktor_collection(x = input$collection_upload$datapath)
      
    }
    
    # filter collection for dodgy observations
    df <- df %>%
      filter(release_year <= year(Sys.Date()), release_year >= 1800,
             bpm <= 300)
      
    return(df)
    
  })
  
  # collection filtered by app inputs
  collection_data_filtered <- reactive({
    
    df <- collection_data()
    
    # respond to user inputs
    df <- df %>%
      filter(year(import_date) >= input$import_date_slider[1], 
             year(import_date) <= input$import_date_slider[2])
    
    return(df)
    
  })
  
  # uploaded traktory history data
  selection_data <- reactive({
    
    # check for history upload
    upload <- input$selection_upload
    if (is.null(upload)) return(NULL)
    
    # filenames object
    filenames <- input$selection_upload$name
    
    # read history data files
    df <- map(input$selection_upload$datapath, read_traktor_history)
    
    # set names of data files to filenames
    names(df) <- filenames
    
    # bind rows of history data files, id col as filename
    df <- bind_rows(df, .id="import_file")
    
    df <- df %>%
      # reduce import file name field
      mutate(import_file = str_extract(import_file, "history.*"))
    
    # create formatted set date
    df$set_date <- str_remove_all(str_extract(df$import_file, "_(.*?)_"), "_")
    df$set_date <- ymd(df$set_date)
    df$set_date_formatted <- as.character(format(df$set_date, "%d %B, %Y"))

    df <- df %>%
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
      mutate(new_set = cumsum(set_break),
             set_date_formatted = if_else(new_set == 0, paste(set_date_formatted),
                                          paste0(set_date_formatted, " (", new_set, ")"))) %>% 
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
        duration = if_else(track_no >= max(track_no) - 1 & duration > 900,
                           900, duration),
        # add track end time field
        end_time = set_time + duration,
        # add 'set quarter' field
        set_quarter = ntile(track_no, n=4)
        ) %>%
      # separate sets if >= 5 mins silence
      ungroup()
  
    return(df)
    
  })
  
  # selection data filtered by app inputs
  selection_data_filtered <- reactive({
    
    req(input$selection_upload)

    df <- selection_data()
    
    # filter for current set choice
    df <- dplyr::filter(df, set_quarter %in% input$set_quarter)
    
    return(df)
    
  })
  
  # set data filtered by app inputs
  set_data <- reactive({
    
    req(input$selection_upload)
    req(input$set_select)
    
    df <- selection_data()
    
    # filter for current set choice
    df <- dplyr::filter(df, set_date_formatted %in% input$set_select)
    
    return(df)
    
  })
  
  # collection page -----------------
  
  # collection upload success pop-up 
  observeEvent(input$collection_upload, {
    # Show a modal when the button is pressed
    shinyalert(title = "collection uploaded.", type = "success",
               closeOnClickOutside = TRUE)
  })
  
  # update slider input based on user collection
  observe({
    
    req(input$collection_upload)
    
    dataset <- collection_data()
    min_import <- min(year(dataset$import_date))
    max_import <- max(year(dataset$import_date))
    
    filtered_dataset <- collection_data_filtered()
    min_slide <- min(year(filtered_dataset$import_date))
    max_slide <- max(year(filtered_dataset$import_date))
    
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session, "import_date_slider", value = c(min_slide, max_slide),
                      min = min_import, max = max_import)
  })
  
  # condition to use in the collection conditional UI
  output$collection_cond <- reactive({
    is.null(input$collection_upload)
  })
  outputOptions(output, "collection_cond", suspendWhenHidden = FALSE)
  
  # collection plot view
  output$collection_plot <- renderPlot({
    
    req(input$collection_upload)
    
    # init data
    df <- collection_data_filtered()
    
    # set plot dependent on x variable
    if (input$xvar %in% c("bpm", "release_year")) {
      
      # density plot
      p <- ggplot(data = df, aes_string(x=input$xvar)) +
        geom_density(colour="#E100FF") +
        ylab("density") +
        theme_ipsum(base_family = "Work Sans Regular", grid = "Y",
                    base_size = 16)
      
    } else if (input$xvar %in% c("artist_name", "album_title")) {
      
      # bar plot
      p <- df %>%
        count(.dots=input$xvar) %>%
        top_n(10, wt=n) %>%
        filter(!is.na(input$xvar) | input$xvar != "NA") %>%
        ggplot(aes_string(x=paste0("reorder(", input$xvar, ", -n)"))) +
        geom_col(aes(y=n), fill="#E100FF") +
        ylab("# tracks") +
        coord_flip() +
        theme_ipsum(base_family = "Work Sans Regular", grid = "X",
                    base_size = 16)
        
    }
    
    # add common plot elements
    p +
      labs(title = paste(input$xvar, "popularity in your", input$collection_type, 
                         "collection,", input$import_date_slider[1], "-", 
                         input$import_date_slider[2]),
           x=NULL) +
      theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16))
      
    
  })
  
  # collection table view
  output$collection_table <- DT::renderDataTable({

    # input$collection_upload will be NULL initially. After the user selects
    # and uploads a file, head of that data file will be shown.

    req(input$collection_upload)

    # create datatable
    df <- collection_data_filtered()
    df <- subset(df, select = c(track_title, artist_name, album_title,
                                bpm, release_year, import_date, last_played,
                                play_count, track_length_formatted
                                ))
    
    DT::datatable(df, rownames = FALSE,
                  colnames = c("track", "artist", "album", "bpm", "release year",
                               "date added", "last played", "play count",
                               "track time"),
                  options = list(
                    order = list(list(1, 'asc')),
                    dom = 'tp',
                    pageLength = 5
                  ))
  })
  
  
  # sets page ------------------------------------------------------
  
  # sets upload success pop-up 
  observeEvent(input$selection_upload, {
    # Show a modal when the button is pressed
    shinyalert(title = "history file(s) uploaded.", type = "success",
               closeOnClickOutside = TRUE)
  })
  
  # update set select input based on user collection
  observe({
    
    req(input$selection_upload)

    updateSelectInput(session, "set_select", 
                      choices = unique(selection_data()$set_date_formatted)
                      )
  })
  
  # condition to use in the set selection conditional UI
  output$set_cond <- reactive({
    is.null(input$selection_upload)
  })
  outputOptions(output, "set_cond", suspendWhenHidden = FALSE)
  
  # sets plot
  output$selection_plot <- renderPlot({
    
    # get user inputs
    req(input$selection_upload)
    
    if (input$set_view == 1) {
      
      # get all selections data
      df <- selection_data_filtered() 
      
      req(input$selection_xvar)
      
      if (input$selection_xvar %in% c("bpm", "release_year")) {
        
        p <- ggplot(data = df, aes_string(x=input$selection_xvar)) +
          geom_density(colour="#E100FF") +
          ylab("density") +
          theme_ipsum(base_family = "Work Sans Regular", grid = "Y",
                      base_size = 16)
        
      } else if (input$selection_xvar %in% c("artist_name")) {
        
        p <- df %>%
          count(.dots=input$selection_xvar) %>%
          top_n(10, wt=n) %>%
          filter(!is.na(input$selection_xvar) | input$selection_xvar != "NA") %>%
          ggplot(aes_string(x=paste0("reorder(", input$selection_xvar, ", -n)"))) +
          geom_col(aes(y=n), fill="#E100FF") +
          ylab("# tracks") +
          coord_flip() +
          theme_ipsum(base_family = "Work Sans Regular", grid = "X",
                      base_size = 16)
      }
      
      # set common plot elements
      p +
        labs(title = paste(input$selection_xvar, "popularity in your selections"),
             x=NULL) +
        theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"),
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16))
        
    } else if (input$set_view == 2) {
    
      # get set data
      df <- set_data()
    
      # text size
      obj_size <- case_when(
        nrow(df) <= 10 ~ 6,
        nrow(df) <= 20 ~ 5,
        nrow(df) <= 30 ~ 4,
        nrow(df) <= 40 ~ 3,
        nrow(df) <= 50 ~ 2,
        nrow(df) > 50 ~ 1
        )
    
      # plot set progress
      ggplot(data = df, aes(y=track_no, x=set_time, xend=end_time,
                          label=paste3(artist_name, track_title))) +
      geom_dumbbell(size=obj_size, size_x = obj_size, size_xend = obj_size,
                    color="#e3e2e1", colour_x = "#7F00FF", colour_xend = "#E100FF",
                    alpha=0.8, dot_guide=TRUE, dot_guide_size=0.25) +
      geom_text_repel(nudge_x = max(df$set_time), size=obj_size, segment.size = 0,
                      direction = "x", family = "Work Sans Regular") +
      scale_y_continuous(trans = "reverse", breaks = unique(df$track_no)) +
      scale_x_time() +
      labs(x="set time", y="track #") +
      theme_ipsum(base_family = "Work Sans Regular", grid = "X",
                  base_size = 16) +
      theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16))
    }
  })
  
  # sets table view
  output$selection_table <- DT::renderDataTable({
    
    # input$collection_upload will be NULL initially. After the user selects
    # and uploads a file, head of that data file will be shown.
    
    req(input$selection_upload)
    
    if (input$set_view == 1) {
      
      df <- selection_data_filtered() }
    
    else if (input$set_view == 2) {
      
      df <- set_data()
      
    }
    
    # create datatable
    df <- subset(df, select = c(track_no, set_time, track_title, artist_name, album_title,
                                bpm, release_year, import_date, last_played,
                                play_count, track_length_formatted))
    
    DT::datatable(df, rownames = FALSE,
                  colnames = c("#", "set time", "track", "artist", "album", "bpm", 
                               "release year","date added", "last played", "play count",
                               "track length"),
                  options = list(
                    order = list(list(1, 'asc')),
                    dom = 'tp',
                    pageLength = 5
                  ))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

