#' @title UI part of the module filter_data
#' @description This function has to be set in the UI part of a shiny 
#' application. filter_data is a module used to filter a given data.table on 
#' chosen columns by the user. The server part returns a reactive value containing
#' the filtered data.table
#' @param id \code{character} An id that will be used to create a namespace
#' @param titles \code{logical} Add titles on UI ? Default to TRUE
#' @param input Not a real parameter, should not be set manually. 
#' Done by callModule automatically.
#' @param output Not a real parameter, should not be set manually. 
#' Done by callModule automatically.
#' @param session Not a real parameter, should not be set manually. 
#' Done by callModule automatically.
#' @param data \code{data.frame/reactive} reactive data.table
#' @param columns_to_filter \code{character/reactive} vector o column names you want to 
#' allow the user to filter (default is all)
#' @param max_char_values \code{integer/reactive} Remove character / factor columns with more than \code{max_char_values} unique values
#' @param default_multisel_n \code{integer/reactive} Number of choices  selected by default in case of multiple selection. Defaut 10
#' @param labels \code{list/reactive} Title / subtitle / message
#' \itemize{
#'  \item{"title"}{ : Module title.}
#'  \item{"no_data "}{ : Printed message if no data.}
#'  \item{"filter"}{ : Seleciton filters.}
#'  \item{"reinitialize"}{ : Reinit button.}
#'  \item{"validate"}{ :  Validate button.}
#'}
#'
#' @return UI page
#' @export
#' @import shiny
#'
#' @examples 
#' \dontrun{
#' 
#' ui = shiny::fluidPage(filter_dataUI(id = "id", titles = TRUE))
#' server = function(input, output, session) {
#'   data <- reactiveValues(data = iris)
#'   shiny::callModule(module = filter_data, id = "id", data = reactive(data$data), 
#'   columns_to_filter = c("Sepal.Length", "Sepal.Width"))
#' }
#' 
#' shiny::shinyApp(ui = ui, server = server)
#' 
#' 
#' ui = shiny::fluidPage(filter_dataUI(id = "id", 
#'   labels = list(title = "Filtres",
#'   no_data = "Pas de données disponibles", 
#'   filter = "Filtrer sur les colonnes :", 
#'   reinitialize = "Réinitialisation des filtres", 
#'   validate = "Filtrer les données")
#' ))
#' 
#' server = function(input, output, session) {
#'   shiny::callModule(module = filter_data, id = "id", data = iris)
#' }
#' 
#' shiny::shinyApp(ui = ui, server = server)
#' 
#' ## Example apps
#' run_example_app_show_data()
#' run_example_app_filter_and_show_data()
#' } 
#'
#' @rdname filter_data_module
filter_dataUI <- function(id, titles = TRUE, 
                          labels = list(title = "Filters",
                                        no_data = "No data available", 
                                        filter = "Filter on :", 
                                        reinitialize = "Reinitialize filters", 
                                        validate = "Apply filtering on data")
) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      column(12,
             if(titles) shiny::div(h2(labels$title))
      )
    ),
    
    shiny::conditionalPanel(
      condition = paste0("output['", ns("have_data_filter"), "'] === false"),
      shiny::div(h2(labels$no_data))
    ),
    
    shiny::conditionalPanel(
      condition = paste0("output['", ns("have_data_filter"), "'] === true"),
      
      shiny::fluidRow(
        column(2, div(br(), h5(labels$filter), align = "center")),
        column(7, shiny::uiOutput(ns("choicefilter"))),
        column(3, shiny::div(br(), shiny::actionButton(
          ns("reinitializeFilter"), labels$reinitialize), align = "center"))
      ),
      
      # display only if there is at least one selected filter
      shiny::conditionalPanel(condition = paste0('input["', ns("chosenfilters"), '"] && input["', ns("chosenfilters"), '"].length > 0'),
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
                                ns("validateFilter"), labels$validate), 
                                align = "center")
      )
    )
  )
}

#' @export
#' @import shiny data.table magrittr
#'
#' @rdname filter_data_module
filter_data <- function(input, output, session, data = NULL,
                        columns_to_filter = "all", 
                        max_char_values = 1000,
                        default_multisel_n = 10) {
  
  ns <- session$ns
  
  # reactive controls
  if (! shiny::is.reactive(data)) {
    get_data <- shiny::reactive(data)
  } else {
    get_data <- data
  }
  
  if (! shiny::is.reactive(columns_to_filter)) {
    get_columns_to_filter <- shiny::reactive(columns_to_filter)
  } else {
    get_columns_to_filter <- columns_to_filter
  }
  
  if (! shiny::is.reactive(max_char_values)) {
    get_max_char_values <- shiny::reactive(max_char_values)
  } else {
    get_max_char_values <- max_char_values
  }
  
  if (! shiny::is.reactive(default_multisel_n)) {
    get_default_multisel_n <- shiny::reactive(default_multisel_n)
  } else {
    get_default_multisel_n <- default_multisel_n
  }
  
  data_to_filter <- shiny::reactive({ 
    data <- get_data()
    if(!"data.frame" %in% class(data)){
      data <- NULL
    } else {
      if(!"data.table" %in% class(data)){
        data <- data.table::as.data.table(data)
      }
      setcolorder(data, colnames(data)[order(colnames(data))])
    }
    data
  })
  
  output$have_data_filter <- shiny::reactive({
    !is.null(data_to_filter()) && "data.frame" %in% class(data_to_filter()) && nrow(data_to_filter()) > 0
  })
  shiny::outputOptions(output, "have_data_filter", suspendWhenHidden = FALSE)
  
  filternames <- reactiveValues(filter = NULL)
  
  output$choicefilter <- renderUI({
    columns_to_filter <- get_columns_to_filter()
    max_char_values <- get_max_char_values()
    if( !is.null(data_to_filter()) && "data.frame" %in% class(data_to_filter()) && nrow(data_to_filter()) > 0){
      if(!"all" %in% columns_to_filter){
        columns_to_filter <- intersect(columns_to_filter, colnames(data_to_filter()))
        if(length(columns_to_filter) == 0) columns_to_filter <- NULL
      }
      
      if(!is.null(max_char_values) && !is.na(max_char_values) && max_char_values >= 1){
        class <- sapply(data_to_filter(), function(x) class(x)[1])
        ind_char <- which(class %in% c("factor", "character"))
        if(length(ind_char) > 0){
          ctrl_n <- data_to_filter()[, sapply(.SD, function(x) length(unique(x))), .SDcols = ind_char]
          rm_columns_to_filter <- names(ctrl_n)[ctrl_n >= max_char_values]
          if(length(rm_columns_to_filter) > 0){
            if (is.null(columns_to_filter) || "all" %in% columns_to_filter) {
              columns_to_filter <- setdiff(colnames(data_to_filter()), rm_columns_to_filter)
            } else {
              columns_to_filter <- setdiff(columns_to_filter, rm_columns_to_filter)
            }
          }
        }
      }
      
      if (is.null(columns_to_filter) | "all" %in% columns_to_filter) {
        data <- data_to_filter()
      } else {
        data <- data_to_filter()[, .SD, .SDcols = columns_to_filter]
      }
      setcolorder(data, colnames(data)[order(colnames(data))])
      values <- colnames(data)
      
      selectInput(ns("chosenfilters"), "", 
                  choices = values, selected = NULL, multiple = TRUE, width = "100%")
    }
  })
  
  # Initialize all filters (one per specified column)
  output$uifilter <- renderUI({
    data <- data_to_filter()
    columns_to_filter <- get_columns_to_filter()
    isolate({
      if (! is.null(data)) {
        
        # check columns_to_filter
        if (! "all" %in% columns_to_filter) {
          columns_to_filter <- intersect(columns_to_filter, colnames(data))
          if(length(columns_to_filter) == 0) columns_to_filter <- NULL
        }
        if (is.null(columns_to_filter) || "all" %in% columns_to_filter) {
          var <- colnames(data)
        } else {
          data <- data[, .SD, .SDcols = columns_to_filter]
          var <- columns_to_filter
        }
        
        filternames$filter <- var
        
        if(length(var) > 0){
          lapply(var, function(colname) {
            ctrlclass <- class(data[, get(colname)])
            
            if (any(ctrlclass %in% c("factor", "character", "logical"))) {
              values <- unique(data[, get(colname)])
              
              if (length(values) <= 10) {
                selectedtype <- "multiple select"
              } else {
                selectedtype <- "single select"
              }
              choices <- c("single select", "multiple select")
              
            } else if (any(ctrlclass %in% c("numeric", "integer"))) {
              selectedtype <- "range slider"
              choices <- c("single slider", "range slider", "less than", "less than or equal to", "greater than", "greater than or equal to")
              
            } else if (any(ctrlclass %in% c("POSIXct", "POSIXlt"))) {
              selectedtype <- "range slider"
              choices <- c("single slider", "range slider")
              
            } else if (any(ctrlclass %in% c("IDate", "Date"))) {
              selectedtype <- "range date"
              choices <- c("single date", "range date")
              
            } else {
              choices <- c("single slider", "range slider", 
                           "single select", "multiple select")
              selectedtype <- "single select"
              
            }
            
            # check that the column is selected to display it (a little artificial, using its position in the input vector)
            conditionalPanel(condition = paste0('input["', ns("chosenfilters"), '"] && input["', ns("chosenfilters"), '"].indexOf("', colname, '") > -1'),
                             fluidRow(
                               column(width = 2, offset = 1,  
                                      h5(colname, style = "font-weight: bold;")
                               ),
                               column(2,
                                      selectInput(ns(paste0("typefilter", colname)), NULL,
                                                  choices = choices,
                                                  selectedtype)
                               ),
                               column(6, 
                                      uiOutput(ns(paste0("uifilter", colname)))
                               )
                             )
            )
          })
        } else {
          fluidRow(
            column(width = 10, offset = 1, h4("No filter selected"))
          )
        }
      }
    })
  })
  
  
  # reinitialize current filters
  observe({
    cpt <- input$reinitializeFilter
    
    isolate({
      if (cpt > 0) {
        data <- data_to_filter()
        # reinitialize current filters only
        reinit_filters <- intersect(input$chosenfilters, colnames(data))
        
        for (filter in reinit_filters) {
          selectedtype <- input[[paste0("typefilter", filter)]]
          
          isolate({
            ctrlclass <- class(data[, get(filter)])
            
            if (any(ctrlclass %in% c("factor", "character", "logical"))) {
              if (selectedtype %in% c("single select", "multiple select")) {
                values <- unique(as.character(data[, get(filter)]))
                
              } else if (selectedtype %in% c("single slider", "range slider")) {
                values <- range(as.numeric(as.character(data[, get(filter)])),
                                na.rm = TRUE)
              }
              
            } else if (any(ctrlclass %in% c("integer", "numeric", "POSIXct", "POSIXlt"))) {
              if (selectedtype %in% c("single select", "multiple select")) {
                values <- unique(as.character(data[, get(filter)]))
                
              } else if (selectedtype %in% c("single slider", "range slider")) {
                values <- range(data[, get(filter)], na.rm = TRUE)
                
              } else if (selectedtype %in% c("less than", "less than or equal to")) {
                values <- max(data[, get(filter)], na.rm = TRUE)
                
              } else if (selectedtype %in% c("greater than", "greater than or equal to")) {
                values <- min(data[, get(filter)], na.rm = TRUE)
              }
              
            } else if (any(ctrlclass %in% c("IDate", "Date"))) {
              values <- range(data[, get(filter)], na.rm = TRUE)
            }
            
            if (selectedtype == "multiple select") {
              updateSelectizeInput(session = session, paste0("filter", filter),
                                   choices = values, selected = values[1:min(length(values), get_default_multisel_n())])
              
            } else if (selectedtype == "single select") {
              updateSelectInput(session = session, paste0("filter", filter),
                                choices = values, selected = values[1])
              
            } else if (selectedtype == "range slider") {
              updateSliderInput(session = session, paste0("filter", filter),
                                min = values[1], max = values[2], value = values)
              
            } else if (selectedtype == "single slider") {
              updateSliderInput(session = session, paste0("filter", filter),
                                min = values[1], max = values[2], value = values[1])
              
            } else if (selectedtype == "single date") {
              updateDateInput(session = session, paste0("filter", filter),
                              min = values[1], max = values[2], value = values[1])
              
            } else if (selectedtype == "range date") {
              updateDateRangeInput(session = session, paste0("filter", filter),
                                   start = values[1], end = values[2],
                                   min = values[1], max = values[2])
            } else if (selectedtype %in% c("less than", "less than or equal to", "greater than", "greater than or equal to")) {
              updateNumericInput(session = session, paste0("filter", filter),
                                 value = values[1])
            }
          })
        }
      }
    })
  })
  
  
  # creation des filtres
  # a optimiser
  observe({
    columns_to_filter <- get_columns_to_filter()
    if(!"all" %in% columns_to_filter){
      columns_to_filter <- intersect(columns_to_filter, colnames(data_to_filter()))
      if(length(columns_to_filter) == 0) columns_to_filter <- NULL
    }
    
    if (is.null(columns_to_filter) || "all" %in% columns_to_filter) {
      data <- data_to_filter()
    } else {
      data <- data_to_filter()[, .SD, .SDcols = columns_to_filter]
    }
    
    filters <- filternames$filter
    ctrl <- lapply(colnames(data), function(colname) {
      
      # if (paste0("typefilter", colname) %in% names(input)) {
      if (colname %in% filters) {
        output[[paste0("uifilter", colname)]] <- renderUI({
          
          selectedtype <- input[[paste0("typefilter", colname)]]
          
          isolate({
            # colname <- colnames(data)[colnames(data) == var]
            ctrlclass <- class(data[, get(colname)])
            
            if (any(ctrlclass %in% c("factor", "character", "logical"))) {
              if (selectedtype %in% c("single select", "multiple select")) {
                values <- unique(as.character(data[, get(colname)]))
                
              } else if (selectedtype %in% c("single slider", "range slider")) {
                values <- range(as.numeric(as.character(data[, get(colname)])), 
                                na.rm = TRUE)
              }
              
            } else if (any(ctrlclass %in% c("integer", "numeric", "POSIXct", "POSIXlt"))) {
              if (selectedtype %in% c("single select", "multiple select")) {
                values <- unique(as.character(data[, get(colname)]))
                
              } else if (selectedtype %in% c("single slider", "range slider")) {
                values <- range(data[, get(colname)], na.rm = TRUE)
                
              } else if (selectedtype %in% c("less than", "less than or equal to")) {
                values <- max(data[, get(colname)], na.rm = TRUE)
                
              } else if (selectedtype %in% c("greater than", "greater than or equal to")) {
                values <- min(data[, get(colname)], na.rm = TRUE)
              }
              
            } else if (any(ctrlclass %in% c("IDate", "Date"))) {
              values <- range(data[, get(colname)], na.rm = TRUE)
            }
            
            if (selectedtype == "multiple select") {
              selectizeInput(ns(paste0("filter", colname)), label = NULL, 
                             choices = values, selected = values[1:min(length(values), get_default_multisel_n())], 
                             multiple = TRUE, width="100%")
              
            } else if (selectedtype == "single select") {
              selectInput(ns(paste0("filter", colname)), label = NULL, 
                          choices = values, 
                          multiple = FALSE, width="100%")
              
            } else if (selectedtype == "range slider") {
              sliderInput(ns(paste0("filter", colname)), label = NULL, 
                          min = values[1], max = values[2],
                          round = TRUE, value = values, width="100%")
              
            } else if (selectedtype == "single slider") {
              sliderInput(ns(paste0("filter", colname)), label = NULL,
                          min = values[1], max = values[2],
                          round = TRUE, value = values[1], width="100%")
              
            } else if (selectedtype == "single date") {
              dateInput(ns(paste0("filter", colname)), label = NULL, 
                        value = values[1], min = values[1], 
                        max = values[2],format = "yyyy-mm-dd", width="100%")
              
            } else if (selectedtype == "range date") {
              dateRangeInput(ns(paste0("filter", colname)), label = NULL, 
                             start = values[1], end = values[2],
                             min = values[1], max = values[2], 
                             format = "yyyy-mm-dd", width="100%")
            } else if (selectedtype %in% c("less than", "less than or equal to", "greater than", "greater than or equal to")) {
              numericInput(inputId = ns(paste0("filter", colname)), label = NULL, 
                           value = values[1], min = NA, max = NA, step = NA,
                           width = "100%")
            }
          })
        })
      }
    })
  })
  
  
  listfilters <- shiny::reactive({
    if(!is.null(input$validateFilter) && input$validateFilter > 0) {
      columns_to_filter <- get_columns_to_filter()
      data <- data_to_filter()
      var <- 1:ncol(data)
      varname <- intersect(input$chosenfilters, colnames(data))
      
      if(!"all" %in% columns_to_filter){
        columns_to_filter <- intersect(columns_to_filter, varname)
        if(length(columns_to_filter) == 0) columns_to_filter <- NULL
      }
      
      if (is.null(columns_to_filter) || "all" %in% columns_to_filter) {
        varname <- intersect(input$chosenfilters, colnames(data))
      } else {
        varname <- columns_to_filter
      }
      
      ctrl <- NULL
      filters <- filternames$filter
      
      ctrl <- rbindlist(lapply(varname, function(x){
        if (x %in% filters) {
          
          name <- x
          selectedtype <- input[[paste0("typefilter", x)]]
          filter <-  input[[paste0("filter", x)]]
          
          if (selectedtype %in% c("range slider", "range date")) {
            colname <- c(name, name)
            fun <- c(">=", "<=")
            values <- c(list(filter[1]), list(filter[2]))
            
          } else if(selectedtype %in% c("single slider", "single date")) {
            colname <- c(name)
            fun <- c("==")
            values <- c(list(filter[1]))
            
          } else if(selectedtype %in% c("multiple select", "single select")) {
            colname <- c(name)
            fun <- c("%in%")
            values <- c(list(filter))
            
          } else if(selectedtype %in% c("less than")) {
            colname <- c(name)
            fun <- c("<")
            values <- c(list(gsub(",", ".", filter, fixed = TRUE)))
            
          } else if(selectedtype %in% c("less than or equal to")) {
            colname <- c(name)
            fun <- c("<=")
            values <- c(list(gsub(",", ".", filter, fixed = TRUE)))
            
          } else if(selectedtype %in% c("greater than")) {
            colname <- c(name)
            fun <- c(">")
            values <- c(list(gsub(",", ".", filter, fixed = TRUE)))
            
          } else if(selectedtype %in% c("greater than or equal to")) {
            colname <- c(name)
            fun <- c(">=")
            values <- c(list(gsub(",", ".", filter, fixed = TRUE)))
            
          }
          
          dtres <- data.table(column = colname, "fun" = fun, values = values)
          
          dtres
        }
      }))
      
      if (nrow(ctrl) > 0 & nrow(data_to_filter()) > 0) {
        
        res <- lapply(1:nrow(ctrl), function(x) {
          list(column = ctrl[x, column], fun = ctrl[x, "fun"], values = ctrl[x, unlist(values)])
        })
        
      } else {
        res <- NULL
      }
      res
      
    } else {
      NULL
    }
  })
  
  filterdata <- reactiveValues(data = NULL)
  observeEvent(c(data_to_filter(), input$validateFilter), {
    
    data <- data_to_filter()
    if (is.null(listfilters())) {
      filterdata$data <- data
    } else {
      filterdata$data <- .filterDataTable(data, listfilters())
    }
  })
  
  return(filterdata)
}