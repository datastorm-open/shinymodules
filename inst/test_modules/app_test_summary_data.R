library(shiny)
library(DT)
library(data.table)
library(htmlwidgets)
library(nycflights13)

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
             optional_stats = "all", 
             show_warnings = FALSE,
             nb_modal2show = 6, 
             columns_to_show = reactive(c(colnames(reactive_data$data), "invalid")))
}

# Run the application
shinyApp(ui = ui, server = server)