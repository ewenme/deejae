# package load
library(shiny)
library(shinyalert)
library(shinythemes)
library(shinycssloaders)
library(lubridate)
library(dplyr)
library(tidytext)
library(wordcloud)
library(stringr)
library(ewen)
library(purrr)

# get functions
source("extract_funcs.R")

# Define UI for application ------------------------------------------------

ui <- navbarPage("deejae", theme = shinytheme("paper"),
                 selected = "upload", collapsible = TRUE,
                 useShinyalert(),  # Set up shinyalert
                 loadEChartsLibrary(), # set up ECharts
                 tags$head(
                   # Include custom CSS
                   includeCSS("styles.css")
                 ),
                 
                 # upload page -----------------
                 
                 # user entry section
                 tabPanel("upload",
                 fluidRow(
                   column(4, wellPanel(
                     
                     # input: collection type
                     radioButtons(
                       inputId = "collection_type", label = "collection select",
                       choices = c(rekordbox = "rekordbox", traktor = "traktor")
                     ),
                     
                     # horizontal line
                     tags$hr(),
                     
                     # input: collection file upload
                     fileInput(
                       inputId = "collection_upload", label = "collection upload",
                       accept = c(".nml", ".xml"), buttonLabel = "browse",
                       placeholder = "  no file selected", multiple = FALSE
                     ),
                     
                     wellPanel(
                       
                       plotOutput(outputId = "track_cloud")
                     )
                   )),
                   
                   # collection preview
                   column(8, wellPanel(
                     
                     # some summary text
                     h3(textOutput(outputId = "collection_summary")),
                     
                     # horizontal line
                     tags$hr(),
                     
                     # collection table view
                     withSpinner(DT::dataTableOutput(outputId = "collection_preview"), 
                                 type = 8)
                     ))
                 )
                 ),
                 
              # collection page -----------------
              
              tabPanel("collection",
              fluidRow(
                
                # user selections
                column(3, wellPanel(
                  
                  # x-var selection
                  selectInput(inputId = "xvar", label = "wot 2 look at?", 
                              c("artists"="artist_name", "BPM"="bpm", 
                                "release years"="release_year"),
                              selected = "artist tracks added"),
                  
                  # horizontal line
                  tags$hr(),
                  
                  sliderInput(inputId = "import_date_slider", label = "import date range",
                              min = 2000, max = year(Sys.Date()), step = 1, sep = "",
                              value = c(2000, year(Sys.Date())))
                )),
                
                # viz output
                column(9, wellPanel(
                  plotOutput(outputId = "collection_plot")
                       ))
              )),
              
              # sets page -----------------
              
              tabPanel(title="sets (traktor only)",
                       fluidRow(
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
  
  # tidy text of track titles
  tidy_trax <- reactive({
    
    df <- traktor_collection
    
    df$track_title <- trimws(gsub("\\w*[0-9]+\\w*\\s*", "", df$track_title))
    
    # unnest tokens
    df %>%
      unnest_tokens(output = word, input = track_title, token = "words") %>%
      # remove stop words
      anti_join(stop_words) %>%
      # remove common track suffixes
      filter(!str_detect(word, "mix|feat|original|remix|dub|extended|edit|original|vocal|production|instrumental|version|track|untitled|ft|rework|refix|dj|vip|rmx"))
    
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
  
  
  # upload page -----------------
  
  # collection summary text
  output$collection_summary <- renderText({
    
    req(input$collection_upload)
    
    paste("There are", nrow(collection_data()), "tracks in your", input$collection_type, "collection.")
    
  })
  
  # track title word cloud
  output$track_cloud <- renderPlot({
    
    req(input$collection_upload)
    
    df <- tidy_trax()
    
    # plot
    df %>%
      count(word) %>%
      with(wordcloud(word, n, max.words = 50))
    
  })
  
  # collection table view
  output$collection_preview <- DT::renderDataTable({
    
    # input$collection_upload will be NULL initially. After the user selects
    # and uploads a file, head of that data file will be shown.
    
    req(input$collection_upload)
    
    # create datatable
    data_preview <- collection_data()
    data_preview <- subset(data_preview, select = c(track_title, artist_name,
                                                    album_title, import_date))
    
    DT::datatable(data_preview, rownames = FALSE,
                  colnames = c("track", "artist", "album", "date added"),
                  options = list(
                    order = list(list(1, 'asc')),
                    dom = 'tp',
                    pageLength = 10
                  )) 
    
  })
  
  # collection upload success pop-up 
  observeEvent(input$collection_upload, {
    # Show a modal when the button is pressed
    shinyalert(title = "collection uploaded.", type = "success",
               closeOnClickOutside = TRUE)
  })
  
  # collection page -----------------
  
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
      
    } else if (input$xvar %in% c("artist_name")) {
    
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

