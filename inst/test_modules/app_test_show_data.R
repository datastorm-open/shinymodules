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
                                     choices = c("mtcars", "iris"))
    ),
    shiny::column(12, show_dataUI(id = "id")
    )
  )
)

server <- function(input, output, session) {
  
  {
    if (!exists("optional_stats")) {
      optional_stats <- "all"
    }
    # Create & update reactiveValues
    reactive_data <- shiny::reactiveValues(data = NULL)
    observe({
      if (input$data_load == "mtcars") {
        reactive_data$data <- data.table::data.table(copy(mtcars))
      } else if (input$data_load == "iris") {
        reactive_data$data <- data.table::data.table(copy(iris))
      }
    })
    
    callModule(module = show_data, id = "id",
               data = shiny::reactive(reactive_data$data), 
               optional_stats = optional_stats)
  }
  
}

# Run the application
shinyApp(ui = ui, server = server, options = list(display.mode = "showcase"))
# shinyApp(ui = ui, server = server)