# package load
library(shiny)
library(shinyalert)
library(shinythemes)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(xml2)
library(lubridate)
library(dplyr)
library(stringr)
library(purrr)
library(ggplot2)
library(ggalt)
library(ggrepel)
library(hrbrthemes)

# get data extraction functions
source("extract_funcs.R")

# Define UI for application ------------------------------------------------

ui <- navbarPage(
  
  # overall layout/styles ---------------
  title = "deejae", theme = shinytheme("paper"),
  selected = "start", collapsible = TRUE,
  useShinyalert(),  # Set up shinyalert
  shinyjs::useShinyjs(), # set up shinyjs
  tags$head(
    # Include custom CSS
    includeCSS("styles.css"),
    # resize plot
    tags$style("#set_plot{height: calc(100vh - 200px) !important;}")
    ),
  
  # start page -------------------
  tabPanel(title="start", 
           fluidRow(column(12, includeMarkdown("start.Rmd")
                           )
                    )
           ),
  
  # app UI -----------------
  
    # set app page
    tabPanel(title="app", fluidRow(column(3, wellPanel(
    
    # input: history files upload
    conditionalPanel(
      condition = "output.set_cond == true",
      fileInput(
        inputId = "history_upload", 
        label = "upload traktor history",
        accept = c(".nml"), buttonLabel = "browse",
        placeholder = "no file selected", multiple = TRUE
        )
      ),
    
    # input: set-by-set or summary view
    conditionalPanel(
      condition = "output.set_cond == false",
      radioButtons(
        inputId = "set_view", label = "selection type",
        choices = list("set-by-set" = 1, "all sets" = 2),
        selected = 1)
    ),
    
    tags$hr(),
    
    # set view
    
    # input: select set
    conditionalPanel(
      condition = "output.set_cond == false && input.set_view == 1",
      selectInput(
        inputId = "set_select", label = "choose a set",
        choices = "")
      ),
    
    # all selections view
    
    # input: plot x-variable
    conditionalPanel(
      condition = "output.set_cond == false && input.set_view == 2",
      selectInput(inputId = "set_xvar", label = "wot 2 plot",
                  c("artists"="artist_name", "BPM"="bpm", 
                    "release years"="release_year"),
                  selected = "bpm")
      ),
    
    # input: stage of set slider
    conditionalPanel(
      condition = "output.set_cond == false && input.set_view == 1",
      sliderInput(inputId = "set_stage", label = "set stage",
                  min = 1, max = 4, value = c(1, 4), step = 1,
                  pre = "Q")
    )
    )),
    
    column(9, tabsetPanel(
      
      # output: set plot
      tabPanel("visualise",
               tags$br(),
               withSpinner(plotOutput(outputId = "set_plot"),
                                                type = 8)
               ),
      # output: set table view
      tabPanel("table view",
               withSpinner(DT::dataTableOutput(outputId = "set_table"),
                                                type = 8)
               )))
    ))
  )


# Define server logic ------------------------------------------------
server <- function(input, output, session) {
  
  # data objects ----------------------------
  
  # uploaded history data
  selection_data <- reactive({
    
    # check for history upload
    upload <- input$history_upload
    if (is.null(upload)) return(NULL)
    
    # filenames object
    filenames <- input$history_upload$name
    
    # read history data files
    df <- map(input$history_upload$datapath, read_traktor_history)
    
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
    
  })
  
  # selection data filtered by app inputs
  selection_data_filtered <- reactive({
    
    req(input$history_upload)

    df <- selection_data()
    
    # filter for current set choice
    df <- dplyr::filter(df, set_stage %in% input$set_stage)
    
    return(df)
    
  })
  
  # set data filtered by app inputs
  set_data <- reactive({
    
    req(input$history_upload)
    req(input$set_select)
    
    df <- selection_data()
    
    # filter for current set choice
    df <- dplyr::filter(df, set_date_formatted %in% input$set_select)
    
    return(df)
    
  })
  
  # app server ------------------------------------------------------
  
  # sets upload success pop-up 
  observeEvent(input$history_upload, {
    # Show a modal when the button is pressed
    shinyalert(title = "history file(s) uploaded.", type = "success",
               closeOnClickOutside = TRUE)
  })
  
  # update set select input based on user collection
  observe({
    
    req(input$history_upload)

    updateSelectInput(session, "set_select", 
                      choices = unique(selection_data()$set_date_formatted)
                      )
  })
  
  # condition to use in the set selection conditional UI
  output$set_cond <- reactive({
    is.null(input$history_upload)
  })
  outputOptions(output, "set_cond", suspendWhenHidden = FALSE)
  
  # sets plot
  output$set_plot <- renderPlot({
    
    # get user inputs
    req(input$history_upload)
    
    if (input$set_view == 2) {
      
      # get all selections data
      df <- selection_data_filtered() 
      
      req(input$set_xvar)
      
      if (input$set_xvar %in% c("bpm", "release_year")) {
        
        p <- ggplot(data = df, aes_string(x=input$set_xvar)) +
          geom_density(colour="#E100FF") +
          ylab("% of selections") +
          scale_y_percent() +
          theme_ipsum(base_family = "Work Sans", grid = "Y",
                      base_size = 16)
        
      } else if (input$set_xvar %in% c("artist_name")) {
        
        p <- df %>%
          count(.dots=input$set_xvar) %>%
          top_n(10, wt=n) %>%
          na.omit() %>%
          ggplot(aes_string(x=paste0("reorder(", input$set_xvar, ", n)"))) +
          geom_col(aes(y=n), fill="#E100FF") +
          ylab("# of selections") +
          coord_flip() +
          theme_ipsum(base_family = "Work Sans", grid = "X",
                      base_size = 16)
      }
      
      # set common plot elements
      p +
        labs(title = paste0(str_replace_all(input$set_xvar, "_", " "), ", ",
                            as.character(format(min(df$set_date), "%B %Y")),
                            " - ", as.character(format(max(df$set_date), "%B %Y")),
                            " selections"),
             x=NULL) +
        theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"),
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16))
        
    } else if (input$set_view == 1) {
    
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
                    alpha=0.6, dot_guide=TRUE, dot_guide_size=0.25) +
      geom_text_repel(nudge_x = max(df$set_time), size=obj_size, segment.size = 0,
                      direction = "x", family = "Work Sans Light") +
      scale_y_continuous(trans = "reverse") +
      scale_x_time() +
      labs(x="set time", y="track #") +
      theme_ipsum(base_family = "Work Sans", grid = "X",
                  base_size = 16) +
      theme(axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16))
    }
  })
  
  # sets table view
  output$set_table <- DT::renderDataTable({
    
    # input$collection_upload will be NULL initially. After the user selects
    # and uploads a file, head of that data file will be shown.
    
    req(input$history_upload)
    
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

