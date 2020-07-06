library(shiny)
library(DT)
library(data.table)
library(shinymodules)

ui <- fluidPage(
  shiny::fluidRow(
    shiny::column(12,
                  shiny::selectInput("data_load", label = "Choose data",
                                     choices = c("mtcars", "iris"))
    ),
    shiny::column(12, filter_data_UI(id = "id")
    ),
    shiny::column(12, downloadButton("downloadData", "Download filtered data")
                  
    )
  )
)

server <- function(input, output, session) {
    # Create & update reactiveValues
    reactive_data <- shiny::reactiveValues(data = NULL, data_filtered = NULL)
    observeEvent(input$data_load, {
      if (input$data_load == "mtcars") {
        reactive_data$data <- data.table::data.table(copy(mtcars))
      } else if (input$data_load == "iris") {
        # just to check wrong R name (with space for example)
        tmp <- data.table::data.table(copy(iris))
        colnames(tmp)[1] <- "Sepal Length"
        reactive_data$data <- tmp
      }
      reactive_data$data_filtered <- reactive_data$data

    })
    
    # callModule(module = filter_data, id = "id",
    #            data = shiny::reactive(reactive_data$data))
    
    data_filtered <- callModule(module = filter_data, id = "id",
                                 data = shiny::reactive(reactive_data$data), 
                                 default_multisel_n = 1,
                                 max_char_values = 100,
                                 columns_to_filter = shiny::reactive(c(colnames(reactive_data$data), "invalid_name")))
    
    observeEvent(data_filtered$data, {
      datafilt <- data_filtered$data
      reactive_data$data_filtered <- datafilt
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(Sys.Date(), input$data_load, ".csv", sep = "")
      },
      content = function(file) {
        fwrite(reactive_data$data_filtered, file, sep = "|", row.names = FALSE)
      }
    )
}

# Run the application
shinyApp(ui = ui, server = server)