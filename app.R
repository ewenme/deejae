# package load
library(shiny)
library(shinyalert)
library(shinythemes)
library(shinycssloaders)
library(lubridate)
library(dplyr)

# get functions
source("extract_funcs.R")

# Define UI for application 
ui <- navbarPage("deejae", theme = shinytheme("paper"),
                 selected = "upload", collapsible = TRUE,
                 useShinyalert(),  # Set up shinyalert
                 
                 # page for uploading data -----------------
                 
                 # user entry section
                 tabPanel("upload",
                 fluidRow(
                   column(3, wellPanel(
                     
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
                     )
                   )),
                   
                   # collection preview
                   column(9, wellPanel(
                     
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
                 
              # page for exploring collection through import time -----------------
              
              tabPanel("time machine",
              fluidRow(
                
                # user selections
                column(3, wellPanel(
                  
                  # x-var selection
                  selectInput("xvar", "wot 2 look at?", 
                              c("artists"="artist_name", "bpm"="bpm", 
                                "release year"="release_year"),
                              selected = "import date"),
                  
                  # horizontal line
                  tags$hr(),
                  
                  sliderInput(inputId = "import_date_slider", label = "import date range",
                              min = 2000, max = year(Sys.Date()), 
                              value = c(2000, year(Sys.Date())))
                )),
                
                # viz output
                column(9, wellPanel(
                  
                  plotOutput(outputId = "time_machine_plot")
                       ))
              ))
)


# Define server logic
server <- function(input, output, session) {
  
  # data objects ----------------------------
  
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
  
  # collection filtered by user inputs
  collection_data_filtered <- reactive({
    
    df <- collection_data()
    
    # respond to user inputs
    df <- df %>%
      filter(year(import_date) >= input$import_date_slider[1], 
             year(import_date) <= input$import_date_slider[2])
    
    return(df)
    
  })
  
  
  # page for uploading data -----------------
  
  # collection summary text
  output$collection_summary <- renderText({
    
    req(input$collection_upload)
    
    paste("There are", nrow(collection_data()), "tracks in your", input$collection_type, "collection.")
    
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
  
  # collection upload pop-up 
  observeEvent(input$collection_upload, {
    # Show a modal when the button is pressed
    shinyalert(title = "collection uploaded.", type = "success",
               closeOnClickOutside = TRUE)
  })
  
  # page for exploring collection -----------------
  
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
                      min = min_import, max = max_import, step = 1)
  })
  
  output$time_machine_plot <- renderPlot({
    
    df <- collection_data_filtered()
    
    if (input$xvar %in% c("bpm", "release_year")) {
    
      ggplot(data = df, aes_string(x=input$xvar)) +
        geom_density() +
        theme_ipsum()
      
    } else if (input$xvar %in% c("artist_name")) {
    
      df %>%
        count(.dots=input$xvar) %>%
        top_n(15, wt=n) %>%
        ggplot(aes_string(x=paste0("reorder(", input$xvar, ", -n)"))) +
        geom_col(aes(y=n)) +
        coord_flip() +
        theme_ipsum()
      
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

