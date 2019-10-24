library(shiny)
library(DT)
library(data.table)
library(sparkline)
library(magrittr)
library(PerformanceAnalytics)
library(htmlwidgets)
library(shinycssloaders)

ui <- fluidPage(
  shiny::fluidRow(
    shiny::column(12,
                  shiny::selectInput("data_load", label = "Choose data",
                                     choices = c("mtcars", "iris", "nycflights"))
    ),
    shiny::column(12, filter_dataUI(id = "id")
    ),
    shiny::column(12, show_dataUI(id = "idshow")
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
        reactive_data$data_filtered <- data.table::data.table(copy(iris))
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
    
    callModule(module = show_data, id = "idshow",
               data = shiny::reactive(reactive_data$data_shown), nb_modal2show = 6,
               columns_to_show = "all")
  }
  
}

# Run the application
shinyApp(ui = ui, server = server, options = list(display.mode = "showcase"))
# shinyApp(ui = ui, server = server)