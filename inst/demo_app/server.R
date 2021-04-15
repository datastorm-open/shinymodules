# Define server logic required to draw a histogram
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
    
    # monitoring
    callModule(module = monitoring_data, 
               id = "monitoring", 
               data = reactive(data_monit),
               col_obs = "obs",
               col_fit = "fit",
               col_date = "date"
    )

})
