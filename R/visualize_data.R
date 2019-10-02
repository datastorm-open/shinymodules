#' @title UI part of the module visualize_data
#' @description This function has to be set in the UI part of a shiny application
#' @param id \code{character} An id that will be used to create a namespace
#' 
#' @return UI page
#' @export
#' @import shiny DT
#' 
#' @examples 
#' \dontrun{
#' # In UI :
#' visualize_dataUI(id = "id")
#' # In Server, with data in a reactiveValues 
#' # for example
#' data <- reactiveValues(data = iris)
#' optional_stats = "all"
#' callModule(module = visualize_data, id = "id", data = reactive(data$data))
#' 
#' ## For a complete example, you can run the function \link{run_example_app_visualize_data}
#' run_example_app_visualize_data(optional_stats)
#' 
visualize_dataUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      column(12,
             shiny::div(h2("Data visualization"))
      )
    ),
    shiny::fluidRow(
      shiny::column(3, 
                    shiny::uiOutput(ns("x_input_ui"))),
      shiny::column(3, 
                    shiny::uiOutput(ns("y_input_ui"))),
      shiny::column(2, 
                    shiny::uiOutput(ns("graph"))),
      shiny::column(1, 
                    shiny::uiOutput(ns("graph_type"))
                    
      ),
      shiny::column(2, 
                    shiny::uiOutput(ns("explore_aggregation"))
      ),

      shiny::column(1,tags$br(), shiny::div(
        shiny::actionButton(ns("goscatter"), "Go!"), align = "center"))
    ),
    
    shiny::conditionalPanel(
      condition = paste0("output['", ns("javascriptexplore"), "']"),
      shiny::conditionalPanel(condition = paste0(
        "output['", ns("ctrlplot"), "'] === 'xy'"),
        rAmCharts::amChartsOutput(ns("xyPlot"), "xy", height = "550px")
      ),
      
      shiny::conditionalPanel(condition = paste0(
        "output['", ns("ctrlplot"), "'] === 'stock'"),
        rAmCharts::amChartsOutput(ns("stockPlot"), "stock", height = "550px")
      ),
      shiny::conditionalPanel(condition = paste0(
        "output['", ns("ctrlplot"), "'] === 'heatmap'"),
        DT::DTOutput(ns("heatmap"))
      )
    ),
    shiny::conditionalPanel(condition = paste0(
      "output['", ns("javascriptexplore"), "'] === false"),
      shiny::plotOutput(ns("ggPlot"), height = "550px")
    )
  )
}



#' @title server part of the module visualize_data
#' @description This function has to be set in the Server part of a shiny application
#' It returns two table, one with statistics on numeric data and one with 
#' statistics on factor data
#' @param input Not a real parameter, should not be set manually. 
#' Done by callModule automatically.
#' @param output Not a real parameter, should not be set manually. 
#' Done by callModule automatically.
#' @param session Not a real parameter, should not be set manually. 
#' Done by callModule automatically.
#' @param data \code{reactivevalues} reactive data.table
#' 
#' @return Server logic
#' @export
#' @importFrom DT DTOutput renderDT
#' @import shiny data.table magrittr sparkline PerformanceAnalytics htmlwidgets 
#' RColorBrewer hexbin stats grDevices stats
#'
#' @examples 
#' \dontrun{
#' # In UI :
#' show_dataUI(id = "id")
#' # In Server, with data in a reactiveValues 
#' # for example
#' data <- reactiveValues(data = iris)
#' callModule(module = visualize, id = "id", data = reactive(data$data))
#' 
#' ## For a complete example, you can run the function \link{run_example_app_visualize_data}
#'
#' run_example_app_visualize_data(run_example_app_visualize_data)
#' 
#' }
#'
visualize_data <- function(input, output, session, data = NULL) {
  ns <- session$ns
  javascript.limit <- 10000
  palette_ggplot <- "RdYlGn"
  ####### VISUALISATION
  
  # VARIABLE X POUR L'EXPLORATION
  # depend de instant.exploratory()
  currentx <- reactive({
    input$x_input
  })
  

  output$x_input_ui <- shiny::renderUI({
    data <- data()
    values <- colnames(data)
    shiny::isolate({
      if(nrow(data) > 0){
        shiny::selectInput(inputId = ns("x_input"), " X : ", choices = values,
                    selected = values[1], multiple = FALSE)
      }
    })
  })
  
  # VARIABLE Y POUR L'EXPLORATION
  # depend de instant.exploratory()
  currenty <- shiny::reactive({
    input$y_input
  })

  output$y_input_ui <- shiny::renderUI({
    data <- data()
    values <- colnames(data)
    shiny::isolate({
      if(nrow(data) > 0){
        shiny::selectInput(inputId = ns("y_input"), " Y : ",
                    choices = c("NULL", rev(values)),
                    selected = currenty(), multiple = FALSE)
      }
    })
  })
  
  output$graph <- shiny::renderUI({
    varx <- input$x_input
    vary <- input$y_input
    shiny::isolate({
      data <-  data()
      
      if(!is.null(varx) | !is.null(vary)){
        classx <- class(data[, get(varx)])
        
        if (!is.null(vary) & vary != "NULL") {
          classy <- class(data[, get(vary)])
          
        } else {
          classy <- "NULL"
        }
        
        if(any(classx %in% c("numeric", "integer"))) {
          if (classy == "NULL") {
            ctype <- c("hist", "point", "line", "boxplot")
          } else if (any(classy %in% c("numeric", "integer"))) {
            ctype <- c("point", "line")
          } else if (any(classy %in% c("factor", "character","boolean"))) {
            ctype <- c("boxplot")
          } else if (any(classy %in% c("IDate", "Date", "POSIXlt", "POSIXct"))) {
            ctype <- c("timeseries")
          }
        }
        if(any(classy %in% c("numeric", "integer"))) {
          if(any(classx %in% c("factor", "character","boolean"))) {
            ctype <- c("boxplot")
          }
        }
        if (!any(classx %in% c("numeric", "integer", "IDate", "Date", "POSIXlt", "POSIXct")) &
            !any(classy %in% c("numeric", "integer", "IDate", "Date", "POSIXlt", "POSIXct"))) {
          ctype <- c("heatmap")
        }
        if (!any(classx %in% c("numeric", "integer")) & classy == "NULL") {
          ctype <- c("barplot")
        }
        if (any(classx %in% c("IDate", "Date", "POSIXlt", "POSIXct")) &
            any(classy %in% c("numeric", "integer"))) {
          ctype <- c("timeseries")
        }
        if (any(classx %in% c("IDate", "Date", "POSIXlt", "POSIXct")) &
            !any(classy%in%c("numeric", "integer"))) {
          ctype <- NULL
        }
        
        selectInput(ns("type_plot"), " type : ", ctype, multiple = FALSE)
      }
    })
    
  })
  
  output$graph_type <- shiny::renderUI({
    if(!is.null(input$type_plot)) {
      if (!(nrow(dataplot()) > javascript.limit & 
            input$type_plot %in% c("point", "line", "timeseries"))) {
       shiny::checkboxInput(ns("dynamic"), " dynamic", value = TRUE)
      }
    }
  })
  
  output$explore_aggregation <- shiny::renderUI({
    if(!is.null(input$type_plot)) {
      if(input$type_plot == "timeseries" 
         # & !(input$type_plot %in% c("line", "point", "timeseries") 
         # & nrow(dataplot()) > javascript.limit
         ) {
        shiny::selectInput(ns("aggregation"), "Aggregation :",
                           c("Average", "Sum", "Low", "High"))
      }
    }
  })
  
  dataplot <- shiny::reactive({
    # browser()
    data <- data()
    varx <- input$x_input
    vary <- input$y_input
    if (length(varx) > 0) {
      if(vary == "NULL"){
        data <- data[ , c(varx), with = FALSE]
      } 
      else{
        if (varx != "date" & vary != "date") {
          data <- data[ , c(varx, vary), with = FALSE]
        } else if(varx == "date") {
          data <- data[ , c("date", "time", vary), with = FALSE]
        } else if(vary == "date") {
          data <- data[ , c("date", "time", varx), with = FALSE]
        }
      }
    }
  })
  
  javascriptplot <- shiny::reactive({
    if(input$goscatter > 0) {
      shiny::isolate({
        data <- dataplot()
        print(input$type_plot %in% c("line", "point", "timeseries") &
                nrow(data) > javascript.limit)
        !((input$type_plot %in% c("line", "point", "timeseries") &
            nrow(data) > javascript.limit) || !input$dynamic)
        # (!(input$type_plot %in% c("line", "point", "timeseries") &&
        #      nrow(data) > javascript.limit) && input$dynamic)
      })
    }
  })
  
  output$javascriptexplore <- shiny::reactive({
    javascriptplot()
  })
  outputOptions(output, "javascriptexplore", suspendWhenHidden = FALSE)
  
  output$stockPlot <- rAmCharts::renderAmCharts({
    if(input$goscatter > 0){
      shiny::isolate({
        if(javascriptplot()){
          shiny::withProgress(message = 'Graphic...', value = 0.5,{
            data <- dataplot()
            if(!(input$type_plot %in% c("line", "point", "heatmap") & 
                 ncol(data) == 2)){
              print("time_series_check dynamic")
              .plotExploratory(data, type = input$type_plot,
                              aggregation = input$aggregation)
            }
          })
        }
      })
    }
  })
  
  output$xyPlot <- rAmCharts::renderAmCharts({
    if(input$goscatter > 0){
      shiny::isolate({
        if(javascriptplot()){
          shiny::withProgress(message = 'Graphic...', value = 0.5,{
            data <- dataplot()
            if(input$type_plot %in% c("line", "point") & ncol(data) == 2){
              .plotExploratory(data, type = input$type_plot)
            }
          })
        }
      })
    }
  })
  output$heatmap <- DT::renderDT({
    if(input$goscatter > 0){
      isolate({
        if(javascriptplot()){
          shiny::withProgress(message = 'Graphic...', value = 0.5,{
            data <- dataplot()
            if(input$type_plot == "heatmap" & ncol(data) == 2){
              .get_Heatmapdt(data)
            }
          })
        }
      })
      
    }
  })
  
  output$ggPlot <- shiny::renderPlot({
    if(input$goscatter > 0){
      shiny::isolate({
        if(!javascriptplot()){
          shiny::withProgress(message = 'Graphic...', value = 0.5,{
            data <- dataplot()
            print("time_series_check static")
              p <- .plotStaticExploratory(data, type = input$type_plot,
                                         aggregation = input$aggregation,
                                         palette_ggplot = palette_ggplot)
            print(p)
          })
        }
      })
    }
  })
  
  output$ctrlplot <- shiny::reactive({
    if(input$goscatter > 0){
      shiny::isolate({
        if(input$type_plot %in% c("line", "point") & ncol(dataplot()) == 2){
          "xy"
        } else if(input$type_plot == "heatmap" & ncol(dataplot()) == 2) {
          "heatmap"
        }else{
          "stock"
        }
      })
    }
  })
  shiny::outputOptions(output, "ctrlplot", suspendWhenHidden = FALSE)
}
