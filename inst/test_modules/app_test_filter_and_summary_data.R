library(shinymodules)
library(shiny)
library(data.table)

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
  
  # Create & update reactiveValues
  target_data <- shiny::reactiveValues(data = NULL)
  
  observeEvent(input$data_load, {
    if (input$data_load == "mtcars") {
      target_data$data <- data.table::data.table(copy(mtcars))
    } else if (input$data_load == "iris") {
      tmp <- data.table::data.table(copy(iris))
      colnames(tmp)[1] <- "Sepal Length"
      target_data$data <- tmp
    } else if (input$data_load == "nycflights"){
      target_data$data <- data.table::data.table(copy(nycflights13::flights))
    }
  })
  
  # call filter_data module getting back filtered data
  data_filtered <- callModule(module = filter_data, id = "id",
                              data = shiny::reactive(target_data$data),
                              columns_to_filter = "all")
  
  # used summary_data on data_filtered, output of filter_data
  callModule(module = summary_data, id = "idshow",
             # optional_stats = "interquartile_range",
             data = shiny::reactive(data_filtered$data), nb_modal2show = 6,
             columns_to_show = c("min", "max", "nb_valid", "var", 
                                 "interquartile_range", "mode_max", 
                                 "boxplot", "density")
  )
}

# Run the application
shinyApp(ui = ui, server = server)