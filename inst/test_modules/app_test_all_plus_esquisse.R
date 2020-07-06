library(shiny)
library(shinymodules)
library(data.table)
library(nycflights13)
library(esquisse)

# pas une super idee de lancer la module graphique
# sur trop de point
limit_nrows_esquisse <- 50000
data_flights <- data.table::data.table(copy(nycflights13::flights))

# donnees pour le monitoring
data_monit <- data.table(obs = runif(100, 1, 10))
data_monit[, fit := obs + rnorm(100, 0, 10)]
data_monit[, date := seq(as.POSIXct("2019-10-07 00:00:00 UTC", tz = "UTC"),
                         as.POSIXct("2019-10-11 03:00:00 UTC", tz = "UTC"), by = 60*60)]
data_monit[, by_quali := factor(sample(rep(1:10, 10)))]
data_monit[, by_quanti := runif(100, 1, 20)]

ui <- fluidPage(
  shiny::fluidRow(
    tabsetPanel(
      tabPanel("Filter + summary + plot", 
               # filter
               filter_data_UI(id = "filter", titles = TRUE),
               hr(),
               # summary
               summary_data_UI(id = "sum_data"),
               hr(),
               # plot
               conditionalPanel(condition = "output.too_much_rows_for_esquisse != undefined && !output.too_much_rows_for_esquisse",
                                checkboxInput("show_esquisse", "Graphics module ?", value = FALSE),
                                conditionalPanel(condition = "input.show_esquisse",
                                                 # pathc esquisse 0.2.1
                                                 tags$div(
                                                   style = "width: 100%, height: 700px;",
                                                   
                                                   esquisserUI(id = "esquisse",
                                                               header = FALSE,
                                                               choose_data = FALSE,
                                                               disable_filters = FALSE)
                                                 )
                                )
               )
      ),
      tabPanel("Monritoring", 
               monitoring_data_UI(id = "monitoring")
      )
    )
  )
)

server <- function(input, output, session) {
  
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
  
  output$too_much_rows_for_esquisse <- reactive({nrow(data_filtered$data) > limit_nrows_esquisse})
  outputOptions(output, "too_much_rows_for_esquisse", suspendWhenHidden = FALSE)
  
  observe({
    data_plot <- data_filtered$data
    if(!is.null(data_plot) && nrow(data_plot) <= limit_nrows_esquisse){
      data_esquisse$data <- data_filtered$data
      data_esquisse$name <- "esquisse"
    } else {
      data_esquisse$data <- data.frame()
      data_esquisse$name <- ""
    }
    
  })
  
  callModule(module = esquisserServer, id = "esquisse", data = data_esquisse)
  
  # monitoring
  callModule(module = monitoring_data, 
             id = "monitoring", 
             data = reactive(data_monit),
             col_obs = "obs",
             col_fit = "fit",
             col_date = "date"
  )
}

# Run the application
shinyApp(ui = ui, server = server)