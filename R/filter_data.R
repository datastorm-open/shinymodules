#' @title UI part of the module filter_data
#' @description This function has to be set in the UI part of a shiny 
#' application. filter_data is a module used to filter a given data.table on 
#' chosen columns by the user. The server part returns a reactive value containing
#' the filtered data.table
#' @param id \code{character} An id that will be used to create a namespace
#' 
#' @return UI page
#' @export
#' @import shiny
#'
#' @examples 
#' \dontrun{
#' # In UI :
#' filter_dataUI(id = "id")
#' # In Server
#' data <- reactiveValues(data = iris)
#' callModule(module = filter_data, id = "id", data = reactive(data))
#' 
#' ## For a complete example, you can run the function \link{run_example_app_filter_data}
#' run_example_app_filter_data()
#' 
#' ## For another example which also uses the module \link{show_data}, you can
#' ## run \link{run_example_app_filter_and_show_data}
#' optional_stats <- "all"
#' run_example_app_filter_and_show_data(optional_stats)
#' } 

filter_dataUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      column(12,
             shiny::div(h2("Filters"))
      )
    ),
    shiny::fluidRow(
      column(1),
      column(7, shiny::uiOutput(ns("choicefilter"))),
      column(3, shiny::div(br(), shiny::actionButton(
        ns("updateFilter"), "Update filters"), align = "center"))
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", ns("updateFilter"), "'] > 0"),
      shiny::fluidRow(
        column(1),
        column(10, hr())
      ),
      shiny::uiOutput(ns("uifilter")),
      shiny::fluidRow(
        column(1),
        column(10, shiny::hr())
      ),
      shiny::div(shiny::actionButton(
        ns("validateFilter"), "Apply filtering on data"), 
        align = "center")
    )
    
  )
}

#' @title Server part of the module filter_data
#' @description This function has to be set in the UI part of a shiny 
#' application. filter_data is a module used to filter a given data.table on 
#' chosen columns by the user. The server part returns a reactive value containing
#' the filtered data.table
#' @param input Not a real parameter, should not be set manually. 
#' Done by callModule automatically.
#' @param output Not a real parameter, should not be set manually. 
#' Done by callModule automatically.
#' @param session Not a real parameter, should not be set manually. 
#' Done by callModule automatically.
#' @param data \code{reactivevalues} reactive data.table
#' @return Server logic
#' @export
#' @import shiny DT data.table magrittr
#'
#' @examples 
#' \dontrun{
#' # In UI :
#' filter_dataUI(id = "id")
#' # In Server
#' data <- reactiveValues(data = iris)
#' callModule(module = filter_data, id = "id", data = reactive(data))
#' 
#' ## For a complete example, you can run the function \link{run_example_app_filter_data}
#' run_example_app_filter_data()
#' 
#' ## For another example which also uses the module \link{show_data}, you can
#' ## run \link{run_example_app_filter_and_show_data}
#' optional_stats <- "all"
#' run_example_app_filter_and_show_data(optional_stats)
#' } 
filter_data <- function(input, output, session, data = NULL) {
  ns <- session$ns
  
  
  output$choicefilter <- renderUI({
    data <- data()
    
    values <- colnames(data)
    values <- values[!values %in% c("time", "shinyid")]
    
    selectInput(ns("chosenfilters"), "Filter on : ", 
                choices = values, selected = NULL, multiple = TRUE)
    
  })
  
  
  # choix des filres
  output$uifilter <- renderUI({
    if(input$updateFilter > 0){
      isolate({
        var <- input$chosenfilters
        data <- data()
        lapply(var, function(colname){
          
          x <- which(colnames(data) == colname)
          if(!colname %in% c("time", "shinyid")){
            
            ctrlclass <- class(data[, get(colname)])
            
            if(any(ctrlclass%in%c("factor", "character"))){
              values <- unique(data[, get(colname)])
              if(length(values) <= 10){
                selectedtype <- "multiple select"
              }else{
                selectedtype <- "single select"
              }
              choices <- c("single select", "multiple select")
              
            }else if(any(ctrlclass%in%c("numeric", "integer"))){
              
              selectedtype <- "range slider"
              choices <- c("single slider", "range slider")
              
            }else if(any(ctrlclass%in%c("IDate", "Date"))){
              
              selectedtype <- "range date"
              choices <- c("single date", "range date")
              
            }else{
              
              choices <- c("single slider", "range slider", 
                           "single select", "multiple select")
              selectedtype <- "single select"
              
            }
            
            fluidRow(
              column(1),
              column(2, 
                     h5(colname, style = "font-weight: bold;")
              ),
              column(2,
                     selectInput(ns(paste0("typefilter", x)), NULL,
                                 choices = choices,
                                 selectedtype)
              ),
              column(6, 
                     uiOutput(ns(paste0("uifilter", x)))
              )
            )
          }
          
        })
      })
    }
    
  })
  
  # creation des filtres
  # a optimiser
  observe({
    data <-data()
    ctrl <- lapply(1:ncol(data), function(var){
      if(paste0("typefilter", var) %in% names(input)){
        output[[paste0("uifilter", var)]] <- renderUI({
          
          selectedtype <- input[[paste0("typefilter", var)]]
          
          isolate({
            data <- data()
            colname <- colnames(data)[var]
            if(!colname %in% c("time", "shinyid")){
              ctrlclass <- class(data[, get(colname)])
              
              if(any(ctrlclass %in% c("factor", "character"))){
                if(selectedtype %in% c("single select", "multiple select")){
                  values <- unique(as.character(data[, get(colname)]))
                  
                }else if(selectedtype %in% c("single slider", "range slider")){
                  values <- range(as.numeric(as.character(data[, get(colname)])), 
                                  na.rm = TRUE)
                }
                
              }else if(any(ctrlclass %in% c("integer", "numeric"))){
                if(selectedtype %in% c("single select", "multiple select")){
                  values <- unique(as.character(data[, get(colname)]))
                  
                }else if(selectedtype %in% c("single slider", "range slider")){
                  values <- range(data[, get(colname)], na.rm = TRUE)
                }
              }else if(any(ctrlclass %in% c("IDate", "Date"))){
                values <- range(data[, get(colname)], na.rm = TRUE)
              }
              
              if(selectedtype == "multiple select"){
                selectizeInput(ns(paste0("filter", var)), label = NULL, 
                               choices = values, selected = values, 
                               multiple = TRUE, width="100%")
                
              }else if(selectedtype == "single select"){
                selectInput(ns(paste0("filter", var)), label = NULL, 
                            choices = values, 
                            multiple = FALSE, width="100%")
                
              }else if(selectedtype == "range slider"){
                sliderInput(ns(paste0("filter", var)), label = NULL, 
                            min = values[1], max = values[2],
                            round = TRUE, value = values)
                
              }else if(selectedtype == "single slider"){
                sliderInput(ns(paste0("filter", var)), label = NULL,
                            min = values[1], max = values[2],
                            round = TRUE, value = values[1])
                
              }else if(selectedtype == "single date"){
                dateInput(ns(paste0("filter", var)), label = NULL, 
                          value = values[1], min = values[1], 
                          max = values[2],format = "yyyy-mm-dd")
                
              }else if(selectedtype == "range date"){
                dateRangeInput(ns(paste0("filter", var)), label = NULL, 
                               start = values[1], end = values[2],
                               min = values[1], max = values[2], 
                               format = "yyyy-mm-dd")
              }
            }
          })
        })
      }
    })
  })
  
  
  listfilters <- shiny::reactive({
    # data <- data()
    if(input$validateFilter > 0){
      shiny::isolate({
        data <- data()
        var <- 1:ncol(data)
        varname <- colnames(data)
        colname <- c()
        fun <- c()
        values <- list()
        ctrl <- lapply(1:ncol(data), function(x){
          if(paste0("typefilter", x) %in% names(input)){
            
            name <-varname[x]
            selectedtype <- input[[paste0("typefilter", x)]]
            filter <-  input[[paste0("filter", x)]]
            
            if(selectedtype %in% c("range slider", "range date")){
              colname <<- c(colname, name, name)
              fun <<- c(fun, ">=", "<=")
              values[[length(values)+1]] <<- filter[1]
              values[[length(values)+1]] <<- filter[2]
              
            }else if(selectedtype %in% c("single slider", "single date")){
              colname <<- c(colname, name)
              fun <<- c(fun, "==")
              values[[length(values)+1]] <<- filter[1]
              
            }else if(selectedtype %in% c("multiple select", "single select")){
              colname <<- c(colname, name)
              fun <<- c(fun, "%in%")
              values[[length(values)+1]] <<- filter
            }
            NULL
          }
        })
        if(length(colname) > 0){
          res <- lapply(1:length(colname), function(x){
            list(column = colname[x], fun = fun[x], values = values[[x]])
          })
        }else{
          res <- NULL
        }
        res
      })
    }else{
      NULL
    }
  })
  
  filterdata <- reactiveValues(data = NULL)
  observe({
    if(is.null(listfilters())) {
      filterdata$data <- data()
    } else {
      isolate({
        filterdata$data <- .filterDataTable(data(), listfilters())
      })
    }
    
    
  })
  return(filterdata)
  
}