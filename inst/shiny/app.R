library(shiny)
library(shinyalert)
library(shinycssloaders)
library(shinyhelper)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(colourpicker)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggalt)
library(hrbrthemes)
library(xml2)
library(extrafont)

# get data extraction functions
source("helper-funcs.R")

# get font
update_geom_font_defaults(family = "IBMPlexSans-Light")


# UI ----------------------------------------------------------------------

ui <- fluidPage(

  # global settings / styles
  title = "deejae", theme = shinytheme("paper"),

  useShinyalert(),
  useShinyjs(),

  tags$head(
    includeCSS("www/styles.css"),
    tags$style("#set_plot{height:100vh !important;}")
    ),

  fluidRow(

    # sidebar / inputs
    column(
      2,

      h2("deejae"),

      # history files upload
      conditionalPanel(
        condition = "output.set_cond == true",
        helper(
          fileInput(
            "history_upload", label = "   upload traktor history",
            accept = c(".nml"), buttonLabel = "browse",
            placeholder = "no file selected", multiple = TRUE
          ),
          content = "help_upload")
      ),

      # choose set-by-set or summary view
      conditionalPanel(
        condition = "output.set_cond == false",
        radioButtons(
          "set_view", label = "set view",
          choices = list("set-by-set" = 1, "all sets" = 2),
          selected = 1)
      ),

      # set view
      conditionalPanel(
        condition = "output.set_cond == false && input.set_view == 1",

        # select set
        selectInput(
          "set_select", label = "choose a set",
          choices = ""),

        # set plot colours
        splitLayout(
          colourInput("track_start_col", label = "track start",
                      value = "#7F00FF", showColour = "background"),
          colourInput("track_end_col", label = "track end",
                      value = "#E100FF", showColour = "background"),
          cellArgs = list (style = "overflow:visible")
          )),

      conditionalPanel(
        condition = "output.set_cond == false && input.set_view == 2",

        # plot x-variable
        selectInput("set_xvar", label = "wot 2 plot",
                    c("tracks"="track_title", "artists"="artist_name",
                      "BPM"="bpm", "release years"="release_year"),
                    selected = "bpm"),

        # stage of set slider
        sliderInput("set_stage", label = "set stage (quarter)",
                    min = 1, max = 4, value = c(1, 4), step = 1,
                    pre="Q"),

        # plot colour
        colourInput("plot_col", label = "plot colour",
                    value = "#7F00FF", showColour = "background")
        ),

      # credits
      HTML(paste("<p>Made by <a href='https://twitter.com/ewen_'>@ewen_</a>.",
                 "Peep the <a href='https://github.com/ewenme/deejae'>code</a>.</p>"))
      ),

    # chart area
    column(
      10,
      withSpinner(
        plotOutput("set_plot", width = "100%"),
        type = 8)
      )
    )
  )


# server ------------------------------------------------------------------

server <- function(input, output, session) {

  observe_helpers()

  # uploaded history data
  selection_data <- reactive({

    # check for history upload
    upload <- input$history_upload
    if (is.null(upload)) return(NULL)

    # filenames object
    filenames <- input$history_upload$name

    # read history data files
    df <- lapply(input$history_upload$datapath, read_traktor_history)

    # set names of data files to filenames
    names(df) <- filenames

    # bind rows of history data files, id col as filename
    df <- bind_rows(df, .id = "import_file")

    # reduce import file name field
    df$import_file <- str_extract(df$import_file, "history.*")

    # create formatted set date
    df$set_date <- str_remove_all(str_extract(df$import_file, "_(.*?)_"), "_")
    df$set_date <- ymd(df$set_date)
    df$set_date_formatted <- as.character(format(df$set_date, "%d %B, %Y"))

    tidy_selections(df)

  })

  # selection data filtered by app inputs
  selection_data_filtered <- reactive({

    req(input$history_upload)

    df <- selection_data()

    # filter for current set choice
    df <- dplyr::filter(df, set_stage >= input$set_stage[1],
                        set_stage <= input$set_stage[2])

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
          geom_density(colour=input$plot_col) +
          ylab("% of selections") +
          scale_y_percent() +
          theme_ipsum_ps(grid = "Y", base_size = 16)

      } else if (input$set_xvar %in% c("artist_name")) {

        p <- df %>%
          count(.dots=input$set_xvar) %>%
          top_n(10, wt=n) %>%
          na.omit() %>%
          ggplot(aes_string(x=paste0("reorder(", input$set_xvar, ", n)"))) +
          geom_col(aes(y=n), fill=input$plot_col) +
          ylab("# of selections") +
          coord_flip() +
          theme_ipsum_ps(grid = "X", base_size = 16)

      } else if (input$set_xvar %in% c("track_title")) {

        p <- df %>%
          group_by(track_title, artist_name) %>%
          summarise(n=n()) %>% ungroup() %>%
          mutate(artist_track = paste3(artist_name, track_title)) %>%
          top_n(10, wt=n) %>%
          na.omit() %>%
          ggplot(aes(x=reorder(artist_track, n))) +
          geom_col(aes(y=n), fill=input$plot_col) +
          ylab("# of selections") +
          coord_flip() +
          theme_ipsum_ps(grid = "X", base_size = 16)

        }

      # set common plot elements
      p +
        labs(title = str_to_lower(paste0(str_replace_all(input$set_xvar, "_", " "), ", ",
                            as.character(format(min(df$set_date), "%B %Y")),
                            " - ", as.character(format(max(df$set_date), "%B %Y")),
                            " selections")), x=NULL) +
        theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"),
              axis.title.x = element_text(size = 16),
              axis.title.y = element_text(size = 16),
              axis.text.x = element_text(size = 14),
              axis.text.y = element_text(size = 14))

    } else if (input$set_view == 1) {

      # get set data
      df <- set_data()

      # text size
      obj_size <- case_when(
        nrow(df) <= 10 ~ 7,
        nrow(df) <= 20 ~ 6,
        nrow(df) <= 30 ~ 5,
        nrow(df) <= 40 ~ 4,
        nrow(df) <= 50 ~ 3,
        nrow(df) > 50 ~ 2
        )

      # plot set progress
      ggplot(data = df, aes(y=track_no, x=set_time, xend=end_time,
                          label=paste3(artist_name, track_title))) +
      geom_dumbbell(size=obj_size, size_x = obj_size, size_xend = obj_size,
                    color="#e3e2e1", colour_x = input$track_start_col,
                    colour_xend = input$track_end_col,
                    dot_guide=TRUE, dot_guide_size=0.25) +
      geom_text(aes(x = end_time), size=obj_size, hjust=-0.1,
                family = "IBMPlexSans-Light") +
      scale_y_continuous(trans = "reverse") +
      scale_x_time() +
      coord_cartesian(clip = "off") +
      labs(x="set time", y="track #") +
      theme_ipsum_ps(grid = "X", base_size = 16) +
      theme(axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            plot.margin = margin(6, 300, 6, 6))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

