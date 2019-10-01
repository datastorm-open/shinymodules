library(shiny)
library(DT)
library(data.table)
library(magrittr)
library(rAmCharts)
library(pipeR)
library(ggplot2)
ui <- fluidPage(
  shiny::fluidRow(
    shiny::column(12,
                  shiny::selectInput("data_load", label = "Choose data",
                                     choices = c("mtcars", "iris"))
    ),
    shiny::column(12, visualize_dataUI(id = "id")
    )
  )
)

server <- function(input, output, session) {
  
  {
    # Create & update reactiveValues
    reactive_data <- shiny::reactiveValues(data = NULL, data_filtered = NULL)
    observeEvent(input$data_load, {
      if (input$data_load == "mtcars") {
        reactive_data$data <- data.table::data.table(copy(mtcars))
      } else if (input$data_load == "iris") {
        reactive_data$data <- data.table::data.table(copy(iris))
      # } else if (input$data_load == "cif") {
      #   reactive_data$data <- data.table::fread(
      #     "../../../../CIF/Demande Thibaut/2019-08-23dt_dtx_after_2015.csv")
      #   reactive_data$data[, PRT_DT_LAST_DTX:= as.Date(
      #     PRT_DT_LAST_DTX, format = "%Y-%m-%d")]
      }
      reactive_data$data_filtered <- reactive_data$data
      
    })

    callModule(module = visualize_data, id = "id",
                                 data = shiny::reactive(reactive_data$data))

  }
  
}

# Run the application
shinyApp(ui = ui, server = server, options = list(display.mode = "showcase"))
# shinyApp(ui = ui, server = server)