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
#' @param file_filtering \code{logical/reactive} Enable importing .csv file for apply custom filtering ?
#' 
#' @param labels \code{list/reactive} Title / subtitle / message
#' \itemize{
#'  \item{"title"}{ : Module title.}
#'  \item{"no_data "}{ : Printed message if no data.}
#'  \item{"filter"}{ : Selection filters.}
#'  \item{"reinitialize"}{ : Reinit button.}
#'  \item{"validate"}{ :  Validate filter button.}
#'  \item{"complete_data"}{ :  Complete data button.}
#'  \item{"file"}{ :  File filtering label.}
#'}
#'
#' @return \code{reactiveValues} with filtered data
#' @export
#' @import shiny
#'
#' @examples 
#' \dontrun{
#' 
#' ui = shiny::fluidPage(filter_data_UI(id = "id", titles = TRUE))
#' 
#' server = function(input, output, session) {
#'   data <- reactiveValues(data = iris)
#'   shiny::callModule(module = filter_data, id = "id", 
#'     data = reactive(data$data), 
#'     columns_to_filter = c("Sepal.Length", "Sepal.Width")
#'   )
#' }
#' 
#' shiny::shinyApp(ui = ui, server = server)
#' 
#' 
#' ui = shiny::fluidPage(filter_data_UI(id = "id"))
#' 
#' server = function(input, output, session) {
#'   filtered_data <- shiny::callModule(module = filter_data, id = "id", 
#'   data = iris,
#'   columns_to_filter = c("Width" = "Petal.Width", "Length" = "Petal.Length"),
#'   labels = list(
#'       title = "Filtres",
#'       no_data = "Pas de données disponibles", 
#'       filter = "Filtrer sur les colonnes :", 
#'       reinitialize = "Réinitialisation des filtres", 
#'       validate = "Filtrer les données",
#'       complete_data = "Jeu de données total"
#'    ))
#'    
#'    shiny::observe({
#'        print(filtered_data$data)
#'    })
#' }
#' 
#' shiny::shinyApp(ui = ui, server = server)
#' 
#' ## Example apps
#' run_example_app_summary_data()
#' run_example_app_filter_and_summary_data()
#' } 
#'
#' @rdname filter_data_module
filter_data_UI <- function(id, titles = TRUE) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      column(12,
             if(titles) uiOutput(ns("ui_title"))
      )
    ),
    
    shiny::conditionalPanel(
      condition = paste0("output['", ns("have_data_filter"), "'] === false"),
      uiOutput(ns("ui_no_data"))
    ),
    
    shiny::conditionalPanel(
      condition = paste0("output['", ns("have_data_filter"), "'] === true"),
      
      shiny::conditionalPanel(
        condition = paste0("output['", ns("is_data_file_filter"), "'] === true"),
        shiny::fluidRow(
          column(9, shiny::div(br(), uiOutput(ns("fileinput_ui")), align = "left")),
          column(3, shiny::div(br(), shiny::actionButton(
            ns("remove_file"), "", icon = icon("trash"), style = "margin-top:25px"), align = "center"))
        )
      ),
      
      shiny::fluidRow(
        column(2, div(br(), uiOutput(ns("ui_filter")), align = "left")),
        column(7, shiny::uiOutput(ns("choicefilter"))),
        column(3, shiny::div(br(), shiny::actionButton(
          ns("reinitializeFilter"), "Reinitialize filters"), align = "center"))
      ),
      
      # display only if there is at least one selected filter
      shiny::conditionalPanel(condition = paste0("output['", ns("have_selected_filter"), "'] === true"),
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
      ),
      # display only if there is no filter
      shiny::conditionalPanel(condition = paste0("output['", ns("have_selected_filter"), "'] !== true"),
                              shiny::fluidRow(
                                column(1),
                                column(10, hr())
                              ),
                              shiny::div(shiny::actionButton(
                                ns("getAlldata"), "Get complete dataset"), 
                                align = "center")
      )
    )
  )
}

#' @export
#' @import shiny data.table magrittr bit64
#'
#' @rdname filter_data_module
filter_data <- function(input, output, session, data = NULL,
                        columns_to_filter = "all", 
                        max_char_values = 1000,
                        default_multisel_n = 10, 
                        file_filtering = FALSE,
                        labels = list(title = "Filters",
                                      no_data = "No data available", 
                                      filter = "Filter on :", 
                                      reinitialize = "Reinitialize filters", 
                                      validate = "Apply filtering on data", 
                                      complete_data = "Get complete dataset", 
                                      file = "Import file with filters :")) {
  
  ns <- session$ns
  
  filterdata <- reactiveValues(data = NULL)
  init <- reactiveVal(TRUE)
  
  # reactive controls
  if (! shiny::is.reactive(data)) {
    get_data <- shiny::reactive(data)
  } else {
    get_data <- data
  }
  
  observe({
    req(get_data())
    init(TRUE)
  })
  
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
  
  if (! shiny::is.reactive(file_filtering)) {
    get_file_filtering <- shiny::reactive(file_filtering)
  } else {
    get_file_filtering <- file_filtering
  }
  
  if (! shiny::is.reactive(labels)) {
    get_labels <- shiny::reactive(labels)
  } else {
    get_labels <- labels
  }
  
  output$ui_title <- renderUI({
    shiny::div(h2(ifelse(length(get_labels()$title) == 0, "Filters", get_labels()$title)))
  })
  output$ui_no_data <- renderUI({
    shiny::div(h2(ifelse(length(get_labels()$no_data) == 0, "No data available", get_labels()$no_data)))
  })
  output$ui_filter <- renderUI({
    shiny::h5(ifelse(length(get_labels()$filter) == 0, "Filter on :", get_labels()$filter), style = "font-weight: bold;")
  })

  output$fileinput_ui <- renderUI({
    input$remove_file
    button_file_label <- get_labels()$file
    if(length(button_file_label) == 0) button_file_label <- "Import file with filters :"
    shiny::fileInput(ns("file_filters"), button_file_label, accept = ".csv", width  = "100%")
  })
  
  filter_file_data <- reactiveVal(NULL)
  
  observeEvent(input$file_filters, {
    
    data_filter <-  try(fread(input$file_filters$datapath), silent = TRUE)
    if("try-error" %in% class(data_filter)){
      showModal(
        modalDialog(
          title = "Error importing data",
          data_filter[[1]],
          easyClose = TRUE,
          footer = modalButton("OK"),
        )
      )
      
      filter_file_data(NULL)
    
    } else {
      filter_file_data(data_filter)
    }
  }, ignoreNULL = TRUE)
  

  observeEvent(input$remove_file, {
    filter_file_data(NULL)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
  observe({
    button_validate_label <- get_labels()$validate
    if(!is.null(button_validate_label)){
      updateActionButton(session, "validateFilter", label = button_validate_label)
    }
  })
  
  observe({
    button_complete_label <- get_labels()$complete_data
    if(!is.null(button_complete_label)){
      updateActionButton(session, "getAlldata", label = button_complete_label)
    }
  })
  
  observe({
    button_renit_label <- get_labels()$reinitialize
    if(!is.null(button_renit_label)){
      updateActionButton(session, "reinitializeFilter", label = button_renit_label)
    }
  })
  
  data_to_filter <- shiny::reactive({ 
    data <- get_data()
    if(!"data.frame" %in% class(data)){
      data <- NULL
    } else {
      if(!"data.table" %in% class(data)){
        data <- data.table::as.data.table(data)
      }
      # setcolorder(data, colnames(data)[order(colnames(data))])
    }
    data
  })
  
  output$is_data_file_filter <- shiny::reactive({
    get_file_filtering()
  })
  shiny::outputOptions(output, "is_data_file_filter", suspendWhenHidden = FALSE)
  
  output$have_data_filter <- shiny::reactive({
    !is.null(data_to_filter()) && "data.frame" %in% class(data_to_filter()) && nrow(data_to_filter()) > 0
  })
  shiny::outputOptions(output, "have_data_filter", suspendWhenHidden = FALSE)
  
  output$have_selected_filter <- shiny::reactive({
    (!is.null(input$chosenfilters) && length(input$chosenfilters) > 0) | (length(filter_file_data()) > 0)
  })
  
  shiny::outputOptions(output, "have_selected_filter", suspendWhenHidden = FALSE)
  
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
      # setcolorder(data, colnames(data)[order(colnames(data))])
      values <- colnames(data)
      
      init_columns_to_filter <- get_columns_to_filter()
      if(length(init_columns_to_filter) > 0 && !"all" %in% init_columns_to_filter && !is.null(names(init_columns_to_filter))){
        names(values) <- names(init_columns_to_filter)[match(values, unname(init_columns_to_filter))]
      }
      
      # selectInput(ns("chosenfilters"), "", 
      #             choices = values, selected = NULL, multiple = TRUE, width = "100%")
      
      shinyWidgets::pickerInput(
        inputId = ns("chosenfilters"), label = "",
        choices = values,
        selected = NULL, 
        multiple = T, width = "100%",
        options = list(`actions-box` = TRUE, `live-search` = TRUE)
      )
      
    }
  })
  
  # Initialize all filters (one per specified column)
  output$uifilter <- renderUI({
    data <- data_to_filter()
    columns_to_filter <- get_columns_to_filter()
    init_columns_to_filter <- get_columns_to_filter()
    
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
              
            } else if (any(ctrlclass %in% c("numeric", "integer", "integer64"))) {
              selectedtype <- "range slider"
              
              nb_unique_values <- length(unique(data[[colname]]))
              
              if (nb_unique_values > isolate(get_max_char_values())) {
                choices <- c("single slider", "range slider", "less than", "less than or equal to", "greater than", "greater than or equal to", "equal to", "not equal to") 
              } else {
                choices <- c("single slider", "range slider", "less than", "less than or equal to", "greater than", "greater than or equal to", "equal to", "not equal to", "single select", "multiple select") 
              }
              
            } else if (any(ctrlclass %in% c("POSIXct", "POSIXlt"))) {
              selectedtype <-  "range date"
              choices <- c("single date", "range date", "single slider", "range slider")
              
            } else if (any(ctrlclass %in% c("IDate", "Date"))) {
              selectedtype <- "range date"
              choices <- c("single date", "range date")
              
            } else {
              choices <- c("single slider", "range slider", 
                           "single select", "multiple select")
              selectedtype <- "single select"
              
            }
            
            label_colname <- colname
            if(length(init_columns_to_filter) > 0 && !"all" %in% init_columns_to_filter && !is.null(names(init_columns_to_filter))){
              label_colname <- names(init_columns_to_filter)[match(label_colname, unname(init_columns_to_filter))]
            }
            
            # check that the column is selected to display it (a little artificial, using its position in the input vector)
            conditionalPanel(condition = paste0('input["', ns("chosenfilters"), '"] !== null && input["', ns("chosenfilters"), '"] !== undefined && input["', ns("chosenfilters"), '"].includes("', colname, '")'),
                             # condition = paste0('output[["', ns(paste0("have_uifilter", colname)), '"]]'), 
                             fluidRow(
                               column(width = 2, offset = 1,  
                                      h5(label_colname, style = "font-weight: bold;")
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
      if (!is.null(cpt) && cpt > 0) {
        data <- data_to_filter()
        # reinitialize current filters only
        reinit_filters <- intersect(input$chosenfilters, colnames(data))
        
        for (filter in reinit_filters) {
          selectedtype <- input[[paste0("typefilter", filter)]]
          
          isolate({
            ctrlclass <- class(data[, get(filter)])
            
            if (any(ctrlclass %in% c("factor", "character", "logical"))) {
              if (selectedtype %in% c("single select", "multiple select")) {
                values <- sort(unique(as.character(data[, get(filter)])))
                
              } else if (selectedtype %in% c("single slider", "range slider")) {
                values <- range(as.numeric(as.character(data[, get(filter)])),
                                na.rm = TRUE)
              }
              
            } else if (any(ctrlclass %in% c("integer", "numeric", "integer64", "POSIXct", "POSIXlt"))) {
              if (selectedtype %in% c("single select", "multiple select")) {
                values <- sort(unique(as.character(data[, get(filter)])))
                
              } else if (selectedtype %in% c("single slider", "range slider")) {
                values <- range(data[, get(filter)], na.rm = TRUE)
                
              } else if (selectedtype %in% c("not equal to", "equal to", "less than", "less than or equal to")) {
                values <- max(data[, get(filter)], na.rm = TRUE)
                
              } else if (selectedtype %in% c("greater than", "greater than or equal to")) {
                values <- min(data[, get(filter)], na.rm = TRUE)
                
              } else if (selectedtype %in% c("single date", "range date")) {
                values <- c(
                  as.Date(min(data[, get(filter)], na.rm = TRUE)), 
                  as.Date(max(data[, get(filter)], na.rm = TRUE))
                )
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
            } else if (selectedtype %in% c("less than", "less than or equal to", "greater than", "greater than or equal to", "equal to", "not equal to")) {
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
        # output[[paste0("have_uifilter", colname)]] <- shiny::reactive({
        #   !is.null(input$chosenfilters) && colname %in% input$chosenfilters
        # })
        # shiny::outputOptions(output, paste0("have_uifilter", colname), suspendWhenHidden = FALSE)
        
        output[[paste0("uifilter", colname)]] <- renderUI({
          
          selectedtype <- input[[paste0("typefilter", colname)]]
          
          isolate({
            # colname <- colnames(data)[colnames(data) == var]
            ctrlclass <- class(data[, get(colname)])
            
            if (any(ctrlclass %in% c("factor", "character", "logical"))) {
              if (selectedtype %in% c("single select", "multiple select")) {
                values <- sort(unique(as.character(data[, get(colname)])))
                
              } else if (selectedtype %in% c("single slider", "range slider")) {
                values <- range(as.numeric(as.character(data[, get(colname)])), 
                                na.rm = TRUE)
              }
              
            } else if (any(ctrlclass %in% c("integer", "numeric", "integer64", "POSIXct", "POSIXlt"))) {
              if (selectedtype %in% c("single select", "multiple select")) {
                values <- sort(unique(as.character(data[, get(colname)])))
                
              } else if (selectedtype %in% c("single slider", "range slider")) {
                values <- range(data[, get(colname)], na.rm = TRUE)
                
              } else if (selectedtype %in% c("not equal to", "equal to", "less than", "less than or equal to")) {
                values <- max(data[, get(colname)], na.rm = TRUE)
                
              } else if (selectedtype %in% c("greater than", "greater than or equal to")) {
                values <- min(data[, get(colname)], na.rm = TRUE)
              } else if (selectedtype %in% c("single date", "range date")) {
                values <- c(
                  as.Date(min(data[, get(colname)], na.rm = TRUE)), 
                  as.Date(max(data[, get(colname)], na.rm = TRUE))
                )
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
            } else if (selectedtype %in% c("less than", "less than or equal to", "greater than", "greater than or equal to", "equal to", "not equal to")) {
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
          if("Date" %in% class(filter)){
            filter <- as.character(filter)
          }
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
            values <- c(list(as.numeric(gsub(",", ".", filter, fixed = TRUE))))
            
          } else if(selectedtype %in% c("less than or equal to")) {
            colname <- c(name)
            fun <- c("<=")
            values <- c(list(as.numeric(gsub(",", ".", filter, fixed = TRUE))))
            
          } else if(selectedtype %in% c("greater than")) {
            colname <- c(name)
            fun <- c(">")
            values <- c(list(as.numeric(gsub(",", ".", filter, fixed = TRUE))))
            
          } else if(selectedtype %in% c("equal to")) {
            colname <- c(name)
            fun <- c("==")
            values <- c(list(as.numeric(gsub(",", ".", filter, fixed = TRUE))))
            
          } else if(selectedtype %in% c("not equal to")) {
            colname <- c(name)
            fun <- c("!=")
            values <- c(list(as.numeric(gsub(",", ".", filter, fixed = TRUE))))
            
          } else if(selectedtype %in% c("greater than or equal to")) {
            colname <- c(name)
            fun <- c(">=")
            values <- c(list(as.numeric(gsub(",", ".", filter, fixed = TRUE))))
            
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
  
  observeEvent(c(data_to_filter(), input$validateFilter, input$getAlldata), {
    data <- data_to_filter()
    data_filter <- filter_file_data()
    if(init()){
      filterdata$data <- data
      init(FALSE)
    } else {
      if (is.null(listfilters())) {
        if(is.null(data_filter)){
          filterdata$data <- data
        } else {
          filterdata$data <- .filterDataTableFile(data, data_filter)
        }
      } else {
        if(is.null(data_filter)){
          filterdata$data <- .filterDataTable(data, listfilters())
        } else {
          tmp <- .filterDataTable(data, listfilters())
          filterdata$data <- .filterDataTableFile(tmp, data_filter)
        }
       
      }
    }
  })
  
  return(filterdata)
}


