# package load
library(shiny)
library(shinyalert)
library(shinythemes)
library(shinycssloaders)
library(shinyjs)
library(lubridate)
library(dplyr)
library(stringr)
library(ewen)
library(purrr)

# get data extraction functions
source("extract_funcs.R")

# Define UI for application ------------------------------------------------

# set overall layout/styles
ui <- navbarPage("deejae", theme = shinytheme("paper"),
                 selected = "collection", collapsible = TRUE,
                 useShinyalert(),  # Set up shinyalert
                 shinyjs::useShinyjs(), # set up shinyjs
                 tags$head(
                   # Include custom CSS
                   includeCSS("styles.css")
                 ),
                 
              # collection page UI -----------------
              
              tabPanel("collection", fluidRow(
                
                # user selections
                column(3, wellPanel(
                  
                  # input: collection type
                  conditionalPanel(condition = "output.collection_cond == true",
                  radioButtons(
                    inputId = "collection_type", label = "collection select",
                    choices = c(rekordbox = "rekordbox", traktor = "traktor")
                  )),
                  # input: collection upload
                  conditionalPanel(condition = "output.collection_cond == true",
                  fileInput(
                    inputId = "collection_upload", label = "collection upload",
                    accept = c(".nml", ".xml"), buttonLabel = "browse",
                    placeholder = "  no file selected", multiple = FALSE)
                  ),
                  # input: x-variable
                  conditionalPanel(condition = "output.collection_cond == false",
                  selectInput(inputId = "xvar", label = "wot 2 plot?", 
                              c("artists"="artist_name", "albums"="album_title",
                                "BPM"="bpm", "release years"="release_year"),
                              selected = "artist tracks added")
                  ),
                  # input: import date
                  conditionalPanel(condition = "output.collection_cond == false",
                  sliderInput(inputId = "import_date_slider", label = "date added",
                              min = 2000, max = year(Sys.Date()), step = 1, sep = "",
                              value = c(2000, year(Sys.Date())))
                ))),
                
                # main panel
                column(9, tabsetPanel(
                  
                  # collection table view
                  tabPanel("table",
                           withSpinner(DT::dataTableOutput(outputId = "collection_table"), 
                                       type = 8)
                  ),
                  # collection plot view
                  tabPanel("plot",
                  withSpinner(plotOutput(outputId = "collection_plot"),
                              type = 8)
                       )))
              )),
              
              # sets page UI -----------------
              
              tabPanel(title="sets (traktor only)", fluidRow(
                
                         column(3, wellPanel(
                           # input: collection file upload
                           fileInput(
                             inputId = "history_upload", label = "history upload",
                             accept = c(".nml"), buttonLabel = "browse",
                             placeholder = "  no file selected", multiple = TRUE
                           ),
                           selectInput(inputId = "set_choice", label = "choose set",
                                       choices = "")
                         )),
                         column(9, wellPanel(
                           plotOutput(outputId = "sets_plot")
                         ))
              )),
              
              # about page -----------------
              
              tabPanel(title="about",
                       fluidRow(
                         column(6,
                                includeMarkdown("about.Rmd")
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
  history_data <- reactive({
    
    # check for history upload
    upload <- input$history_upload
    if (is.null(upload)) return(NULL)
    
    # read history data
    df <- map_dfr(input$history_upload$datapath, read_traktor_history)
    
    # filter collection for dodgy observations
    df <- df %>%
      group_by(start_date) %>%
      filter(n() >= 5) %>% ungroup()
  
    return(df)
    
  })
  
  # collection page SERVER -----------------
  
  # collection upload success pop-up 
  observeEvent(input$collection_upload, {
    # Show a modal when the button is pressed
    shinyalert(title = "collection uploaded.", type = "success",
               text = textOutput(outputId = "collection_summary"),
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
  
  # collection table view
  output$collection_table <- DT::renderDataTable({
    
    # input$collection_upload will be NULL initially. After the user selects
    # and uploads a file, head of that data file will be shown.
    
    req(input$collection_upload)
    
    # create datatable
    df <- collection_data_filtered()
    df <- subset(df, select = c(track_title, artist_name, album_title, 
                                bpm, release_year, import_date, last_played,
                                play_count, track_length_formatted))
    
    DT::datatable(df, rownames = FALSE,
                  colnames = c("track", "artist", "album", "bpm", "release year",
                               "date added", "last played", "play count", 
                               "track length"),
                  options = list(
                    order = list(list(1, 'asc')),
                    dom = 'tp',
                    pageLength = 10
                  )) 
    
  })
  
  
  
  output$collection_plot <- renderPlot({
    
    req(input$collection_upload)
    
    df <- collection_data_filtered()
    
    if (input$xvar %in% c("bpm", "release_year")) {
    
      ggplot(data = df, aes_string(x=input$xvar)) +
        geom_density() +
        labs(title = paste(input$xvar, "popularity in your", input$collection_type, 
                           "collection,", input$import_date_slider[1], "-", 
                           input$import_date_slider[2]),
             x=NULL, y="density") +
        theme_work(base_size = 14) +
        theme(axis.text.x = element_text(size=12),
              plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
    } else if (input$xvar %in% c("artist_name", "album_title")) {
    
      df %>%
        count(.dots=input$xvar) %>%
        top_n(15, wt=n) %>%
        ggplot(aes_string(x=paste0("reorder(", input$xvar, ", -n)"))) +
        geom_col(aes(y=n)) +
        labs(title = paste(input$xvar, "popularity in your", input$collection_type, 
                           "collection,", input$import_date_slider[1], "-", 
                           input$import_date_slider[2]),
             x=NULL, y="# tracks") +
        coord_flip() +
        theme_work(base_size = 14) +
        theme(axis.text.x = element_text(size=12),
              plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
      
    }
    
  })
  
  # sets page ------------------------------------------------------
  
  # get unique set start times
  sets_options <- reactive({
    
    df <- history_data()
    unique(df$start_date)
  })
  
  # update set select with reactive set times val
  observe({
    updateSelectInput(session, "set_choice",
                      choices = sets_options()
    )})
  
  # sets plot
  output$sets_plot <- renderPlot({
    
    # get user inputs
    req(input$history_upload)
    
    df <- history_data()
    
    set <- filter(df, start_date==input$set_choice)
    
    # plot set progress
    ggplot(data = set, aes(y=track_no, x=set_time, xend=set_time+duration,
                           label=paste(artist_name, "-", track_title))) +
      geom_dumbbell(size=2, size_x = 2, size_xend = 2,
                    color="#e3e2e1", colour_x = "#ED5B67", colour_xend = "#91C5CB") +
      geom_text_repel(nudge_x = max(set$set_time), size=4, segment.size = 0) +
      scale_y_continuous(trans = "reverse", breaks = unique(set$track_no)) +
      labs(title=paste("my", input$set_choice, "set"), x="set time", y="track #") +
      theme_work(base_size = 14) +
      theme(axis.text.x = element_text(size=12),
            plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

