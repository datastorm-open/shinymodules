#' @title UI part of the module visualize_data
#' @description This function has to be set in the UI part of a shiny application
#' @param id \code{character} An id that will be used to create a namespace
#' @param titles \code{logical} Add titles on UI ? Default to TRUE
#' @param input Not a real parameter, should not be set manually. 
#' Done by callModule automatically.
#' @param output Not a real parameter, should not be set manually. 
#' Done by callModule automatically.
#' @param session Not a real parameter, should not be set manually. 
#' Done by callModule automatically.
#' @param data \code{reactivevalues} reactive data.table
#' @param palette_ggplot \code{character} Palette's name used for graphics,
#' default is BuPu, all possible values are palettes from the package RColorBrewer
#' 
#' @return UI page
#' @export
#' @import shiny
#' @importFrom DT renderDT DTOutput
#' 
#' @examples 
#' \dontrun{
#' 
#' ui = shiny::fluidPage(visualize_dataUI(id = "id", titles = TRUE))
#' server = function(input, output, session) {
#'   data <- reactiveValues(data = iris)
#'   shiny::callModule(module = visualize_data, id = "id", data = reactive(data$data))
#' }
#'
#' shiny::shinyApp(ui = ui, server = server)
#' 
#' ## Example apps
#' run_example_app_visualize_data()
#' }
#' 
#' 
#' @rdname visualize_data_module
#' 
visualize_dataUI <- function(id, titles = TRUE) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      column(12,
             if(titles) shiny::div(h2("Data visualization"))
      )
    ),
    
    shiny::conditionalPanel(
      condition = paste0("output['", ns("have_data_viz"), "'] === false"),
      shiny::div(h2("No data available"))
    ),
    
    shiny::conditionalPanel(
      condition = paste0("output['", ns("have_data_viz"), "'] === true"),
      shiny::fluidRow(
        shiny::column(3, 
                      shiny::uiOutput(ns("x_input_ui"))),
        shiny::column(3, 
                      shiny::uiOutput(ns("y_input_ui"))),
        shiny::column(2, 
                      shiny::uiOutput(ns("graph"))),
        shiny::column(1, shiny::br(),
                      shiny::checkboxInput(ns("dynamic"), " dynamic", value = TRUE)
                      
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
  )
}



#' @export
#' @importFrom DT DTOutput renderDT
#' @import shiny data.table magrittr sparkline PerformanceAnalytics htmlwidgets 
#' RColorBrewer hexbin stats grDevices stats
#' 
#' @rdname visualize_data_module
#' 
visualize_data <- function(input, output, session, data = NULL,
                           palette_ggplot = "BuPu") {
  ns <- session$ns
  javascript.limit <- 10000
  ####### VISUALISATION
  
  # VARIABLE X POUR L'EXPLORATION
  # depend de instant.exploratory()
  currentx <- reactive({
    input$x_input
  })
  
  data_viz <- shiny::reactive({
    data <- data()
    if(!"data.frame" %in% class(data)){
      data <- NULL
    } else {
      if(!"data.table" %in% class(data)){
        data <- data.table::as.data.table(data)
      }
    }
    data
  })
  
  output$have_data_viz <- shiny::reactive({
    !is.null(data_viz()) && "data.frame" %in% class(data_viz()) && nrow(data_viz()) > 0
  })
  shiny::outputOptions(output, "have_data_viz", suspendWhenHidden = FALSE)
  
  
  
  
  output$x_input_ui <- shiny::renderUI({
    data <- data_viz()
    values <- colnames(data)
    shiny::isolate({
      if(!is.null(data) && nrow(data) > 0 ){
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
    data <- data_viz()
    values <- colnames(data)
    shiny::isolate({
      if(!is.null(data) && nrow(data) > 0){
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
      data <-  data_viz()
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
  
  output$explore_aggregation <- shiny::renderUI({
    plot_type <- input$type_plot
    if(!is.null(plot_type)) {
      if(plot_type == "timeseries" 
         # & !(input$type_plot %in% c("line", "point", "timeseries") 
         # & nrow(dataplot()) > javascript.limit
      ) {
        shiny::selectInput(ns("aggregation"), "Aggregation :",
                           c("Average", "Sum", "Low", "High"))
      }
    }
  })
  
  dataplot <- shiny::reactive({
    
    data <- data_viz()
    varx <- input$x_input
    vary <- input$y_input
    if (!is.null(data) && nrow(data) > 0 && length(varx) > 0 & varx %in% colnames(data)) {
      if (vary == "NULL") {
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
    } else {
      varx <- colnames(data)[1]
      vary <- "NULL"
    }
  })
  
  javascriptplot <- shiny::reactive({
    if(input$goscatter > 0) {
      shiny::isolate({
        data <- dataplot()
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
            if(!(input$type_plot %in% c("line", "point", "heatmap") & ncol(data) == 2)){
              plotExploratory(data, type = input$type_plot,
                              aggregation = input$aggregation,
                              palette_ggplot = palette_ggplot)
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
              plotExploratory(data, type = input$type_plot,
                              palette_ggplot = palette_ggplot)
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
            p <- plotExploratory(data, type = input$type_plot,
                                 aggregation = input$aggregation,
                                 palette_ggplot = palette_ggplot, js = F)
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
