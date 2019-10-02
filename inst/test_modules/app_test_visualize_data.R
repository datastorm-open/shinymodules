library(shiny)
library(DT)
library(data.table)
library(magrittr)
library(rAmCharts)
library(pipeR)
library(ggplot2)
require(RColorBrewer)
require(hexbin)

ui <- fluidPage(
  shiny::fluidRow(
    shiny::column(12,
                  shiny::selectInput("data_load", label = "Choose data",
                                     choices = c("mtcars", "iris", "nycflights"))
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
      } else if(input$data_load == "nycflights"){
        dt <- as.data.table(nycflights13::flights)[month(time_hour)==1]
        dt[, date_test := as.Date(time_hour)]
        reactive_data$data <- dt[sample(1:nrow(dt), 2000)]
        print(nrow(reactive_data$data))
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