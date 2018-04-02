# package load
library(shiny)
library(shinyalert)
library(shinythemes)
library(shinycssloaders)
library(highcharter)
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
                 
              # page for exploring collection -----------------
              
              tabPanel("explore",
              fluidRow(
                
                # user selections
                column(3, wellPanel(
                  
                  # x-var selection
                  selectInput("xvar", "wot 2 look at?", 
                              c("import date"="import_date", "bpm"="bpm", 
                                "release year"="release_year"),
                              selected = "import date")
                )),
                
                # viz output
                column(9, wellPanel(
                  
                  highchartOutput(outputId = "density_plot")
                       ))
              ))
)


# Define server logic
server <- function(input, output) {
  
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
    
    # filter collection
    df <- df %>%
      filter(release_year <= year(Sys.Date()), bpm <= 300)
    
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
  
  output$density_plot <- renderHighchart({
    
    collection_data() %>%
      group_by(input$xvar) %>%
      summarise(count = n()) %>%
      hchart("spline") %>%
      hc_xAxis() 
    
    test <- quo(xvar)
    
    my_summarise <- function(df, group_var) {
      df %>%
        group_by(!! group_var) %>%
        summarise(count = n()) %>%
        hchart("spline", hcaes(x = !! group_var)) 
    }
    
    my_summarise(rekordbox_collection, xvar)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

