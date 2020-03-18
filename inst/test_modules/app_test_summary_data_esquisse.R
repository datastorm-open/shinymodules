library(shiny)
library(DT)
library(data.table)
library(sparkline)
library(magrittr)
library(PerformanceAnalytics)
library(htmlwidgets)
library(nycflights13)
library(esquisse)

ui <- fluidPage(
  shiny::fluidRow(
    shiny::column(12,
                  shiny::selectInput("data_load", label = "Choose data",
                                     choices = c("mtcars", "flights", "iris"), 
                                     selected = "flights")
    ),
    shiny::column(12, summary_data_UI(id = "id")
    )
  )
)

server <- function(input, output, session) {
  # optional_stats <- c("min", "max")
  
  if (!exists("optional_stats")) {
    optional_stats <- "all"
  }
  if (!exists("nb_modal2show")) {
    nb_modal2show <- 6
  }
  
  # Create & update reactiveValues
  reactive_data <- shiny::reactiveValues(data = NULL)
  observe({
    if (input$data_load == "mtcars") {
      reactive_data$data <- data.table::data.table(copy(mtcars))
    } else if (input$data_load == "flights") {
      reactive_data$data <- data.table::data.table(copy(nycflights13::flights))
    } else if (input$data_load == "iris") {
      data <- copy(iris)
      data[[2]] <- NA_integer_
      reactive_data$data <- data.table::data.table(data)
    }
  })
  callModule(module = summary_data, id = "id",
             data = shiny::reactive(reactive_data$data), 
             optional_stats = optional_stats, show_warnings = FALSE,
             nb_modal2show = nb_modal2show, columns_to_show = c(colnames(reactive_data$data), "invalid"))
}

# Run the application
shinyApp(ui = ui, server = server, options = list(display.mode = "showcase"))
# shinyApp(ui = ui, server = server)