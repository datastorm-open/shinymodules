### Code

```R
# example data in global.R
library(nycflights13)
library(esquisse) # esquisse ggplot2 module

data_flights <- data.table::data.table(copy(nycflights13::flights))

# set limits for esquisse graphics module
limit_nrows_esquisse <- 50000

# ui
ui <- fluidPage(
  # filter
  filter_data_UI(id = "filter", titles = TRUE),
  hr(),
  # summary
  summary_data_UI(id = "sum_data"),
  hr(),
  h2("Visualization"),
  # graphics
  conditionalPanel(condition = "output.enable_esquisse !== undefined && output.enable_esquisse",
                   esquisse_ui(id = "esquisse",
                               header = FALSE,
                               controls = c("labs", "parameters", "appearance", "code"),
                               container = esquisseContainer(height = "700px"))
                   
                   
  ),
  conditionalPanel(condition = "output.enable_esquisse !== undefined && output.enable_esquisse === false",
                   div(h3("Too much rows (> 50K) in filtering data for esquisse module", style = "color : red"), align = "center")
  )
)

# server
shinyServer(function(input, output) {
  
  data <- reactive(data_flights)
  
  # call first filtering data
  data_filtered <- shiny::callModule(
    module = filter_data, 
    id = "filter", 
    data = data
  )
  
  # then call summary_data on filtered data                  
  callModule(module = summary_data, 
             id = "sum_data",
             data = reactive(data_filtered$data))
  
  # and esquisse
  data_esquisse <- reactiveValues(data = data.frame(), name = "esquisse")
  
  output$enable_esquisse <- reactive({nrow(data_filtered$data) <= limit_nrows_esquisse})
  outputOptions(output, "enable_esquisse", suspendWhenHidden = FALSE)
  
  observe({
    data_plot <- data_filtered$data
    if(!is.null(data_plot) && nrow(data_plot) <= limit_nrows_esquisse){
      data_esquisse$data <- data_filtered$data
      data_esquisse$name <- "esquisse"
    } else {
      data_esquisse$data <- data.frame()
      data_esquisse$name <- "esquisse"
    }
    
  })
  
  esquisse_server(
    id = "esquisse", 
    data_rv = data_esquisse
  )
})
```
