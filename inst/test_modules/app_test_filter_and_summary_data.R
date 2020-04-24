library(shinymodules)
library(shiny)
library(DT)
library(data.table)
library(sparkline)
library(magrittr)
library(PerformanceAnalytics)
library(htmlwidgets)

ui <- fluidPage(
  shiny::fluidRow(
    shiny::column(12,
                  shiny::selectInput("data_load", label = "Choose data",
                                     choices = c("mtcars", "iris", "nycflights"))
    ),
    shiny::column(12, filter_data_UI(id = "id")
    ),
    shiny::column(12, summary_data_UI(id = "idshow")
    )
  )
)

server <- function(input, output, session) {
  
  {
    if (!exists("optional_stats")) {
      optional_stats <- "all"
    }
    # Create & update reactiveValues
    reactive_data <- shiny::reactiveValues(data_filtered = NULL, data_shown = NULL)
    observeEvent(input$data_load, {
      if (input$data_load == "mtcars") {
        reactive_data$data_filtered <- data.table::data.table(copy(mtcars))
      } else if (input$data_load == "iris") {
        tmp <- data.table::data.table(copy(iris))
        colnames(tmp)[1] <- "Bad name"
        reactive_data$data_filtered <- tmp
      } else if (input$data_load == "nycflights"){
        reactive_data$data_filtered <- data.table::data.table(copy(nycflights13::flights))
      }
      
      reactive_data$data_shown <- reactive_data$data_filtered
    })
    
    
    data_filtered <- callModule(module = filter_data, id = "id",
                                 data = shiny::reactive(reactive_data$data_filtered),
                                columns_to_filter = "all")
    
    observeEvent(data_filtered$data, {
      datafilt <- data_filtered$data
      reactive_data$data_shown <- datafilt
    })
    
    callModule(module = summary_data, id = "idshow",
               # optional_stats = "interquartile_range",
               data = shiny::reactive(reactive_data$data_shown), nb_modal2show = 6,
               columns_to_show = c("min", "max", "nb_valid", "var", "interquartile_range", "mode_max", "boxplot", "density"))
  }
  
}

# Run the application
shinyApp(ui = ui, server = server, options = list(display.mode = "showcase"))
# shinyApp(ui = ui, server = server)