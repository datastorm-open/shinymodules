#' @title UI part of the module show_data
#' 
#' @description Shiny module to show descripive statistics on data
#' 
#' @param id \code{character} An id that will be used to create a namespace
#' It returns three tables, one with statistics on numeric data, one with 
#' statistics on factor data and one with statistics on dates data
#' @param titles \code{logical} Add titles on UI ? Default to TRUE
#' @param subtitles \code{logical} Add subtitles on UI ? Default to TRUE
#' @param input Not a real parameter, should not be set manually. 
#' Done by callModule automatically.
#' @param output Not a real parameter, should not be set manually. 
#' Done by callModule automatically.
#' @param session Not a real parameter, should not be set manually. 
#' Done by callModule automatically.
#' @param data \code{data.frame/reactive} reactive data.table
#' @param optional_stats \code{character/reactive} optional statistics computed on numeric
#' data, default is "all". "pct_zero", "pct_NA", "mean", "median", "sd" are
#' always computed, possible values are "min", "max", "nb_valid", "var", "interquartile_range",
#' "mode_max", "kurtosis", "skewness", "boxplot", "density".
#' @param nb_modal2show \code{integer/reactive} number of modalities to show 
#' for factor variables. 
#' @param columns_to_show  \code{character/reactive} vector of column names you want to be
#' shown in the tables
#' @param show_warnings \code{logical/reactive} Show warnings ? (example compute Min. on all NAs)
#' @param labels \code{list/reactive} Message of the progress computing indicators.
#' \itemize{
#'  \item{"title"}{ : Module title.}
#'  \item{"num_var "}{ : Subtitle for numeric variables.}
#'  \item{"date_var"}{ : Subtitle for date variables.}
#'  \item{"factor_var"}{ : Subtitle for factor variables.}
#'  \item{"message"}{ :  Message of the progress computing indicators}
#'}
#' @return shiny module
#' 
#' @export
#' 
#' @import shiny shinycssloaders
#' 
#' @seealso \code{\link{filter_data}}
#' @examples 
#' 
#' \dontrun{
#' 
#' ui = shiny::fluidPage(show_dataUI(id = "id", titles = TRUE))
#' server = function(input, output, session) {
#'   data <- reactiveValues(data = iris)
#'   shiny::callModule(module = show_data, id = "id", data = reactive(data$data),
#'     optional_stats = "all")
#' }
#' 
#' shiny::shinyApp(ui = ui, server = server)
#'      
#' ## filter on stats 
#' ui = shiny::fluidPage(show_dataUI(id = "id", titles = TRUE))
#' server = function(input, output, session) {
#'   shiny::callModule(module = show_data, id = "id", data = iris,
#'     optional_stats = c("kurtosis", "density"),
#'     columns_to_show = c("Species", "Petal.Width", "Sepal.Width"))
#' }
#' 
#' shiny::shinyApp(ui = ui, server = server)   
#'           
#' ## labels
#' ui = shiny::fluidPage(show_dataUI(id = "id", titles = FALSE))
#' server = function(input, output, session) {
#'   shiny::callModule(module = show_data, id = "id", data = iris,
#'     labels = list(title = "Descriptive statistics", 
#'                   num_var = "Variables numériques",
#'                   date_var = "Variables dates",
#'                   factor_var = "Variables Facteurs",
#'                   message = "Calcul en cours..."
#'     ))
#' }
#' 
#' shiny::shinyApp(ui = ui, server = server)   
#'                  
#' ## Examples apps
#' run_example_app_show_data()
#' run_example_app_filter_and_show_data()
#' }
#' 
#' @rdname show_data_module
#' 
show_dataUI <- function(id, titles = TRUE, subtitles = TRUE) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    ## Stats descriptives on numerical variables in a first tab
    ## then on factor variables in a second tab
    shiny::fluidRow(
      column(12,
             if(titles) uiOutput(ns("title"))
      )
    ),
    shiny::conditionalPanel(
      condition = paste0("output['", ns("have_data"), "'] === false"),
      shiny::div(h2("No data available"))
    ),
    
    shiny::conditionalPanel(
      condition = paste0("output['", ns("have_data"), "'] === true"),
      shiny::fluidRow(
        column(12,
               shiny::conditionalPanel(
                 condition = paste0("output['", ns("have_dt_num"), "'] === true"),
                 if(subtitles) uiOutput(ns("subtitle_num_var")),
                 show_DT_UI(ns("dt_num_ui"))
               )
        )
      ),
      shiny::fluidRow(
        column(12,
               shiny::conditionalPanel(
                 condition = paste0("output['", ns("have_dt_dates"), "'] === true"),
                 shiny::hr(),
                 if(subtitles) uiOutput(ns("subtitle_date_var")),
                 show_DT_UI(ns("dt_dates_ui"))
               )
        )
      ),
      shiny::fluidRow(
        column(12,
               shiny::conditionalPanel(
                 condition = paste0("output['", ns("have_dt_fact"), "'] === true"),
                 shiny::hr(),
                 if(subtitles) uiOutput(ns("subtitle_factor_var")),
                 show_DT_UI(ns("dt_fact_ui"))
               )
        )
      )
    )
  )
}


#' @export
#' @import shiny data.table magrittr sparkline PerformanceAnalytics htmlwidgets
#'
#' @rdname show_data_module
show_data <- function(input, output, session, data = NULL, optional_stats = "all", 
                      nb_modal2show = 3, columns_to_show = "all", show_warnings = FALSE, 
                      labels = list(title = "Descriptive statistics", 
                                    num_var = "Numeric variables",
                                    date_var = "Date variables",
                                    factor_var = "Factor variables",
                                    message = "Calculation in progress..."
                      )
) {
  ns <- session$ns
  
  # reactive controls
  if (! shiny::is.reactive(data)) {
    get_data <- shiny::reactive(data)
  } else {
    get_data <- data
  }
  
  if (! shiny::is.reactive(optional_stats)) {
    get_optional_stats <- shiny::reactive(optional_stats)
  } else {
    get_optional_stats <- optional_stats
  }
  
  if (! shiny::is.reactive(nb_modal2show)) {
    get_nb_modal2show <- shiny::reactive(nb_modal2show)
  } else {
    get_nb_modal2show <- nb_modal2show
  }
  
  if (! shiny::is.reactive(columns_to_show)) {
    get_columns_to_show <- shiny::reactive(columns_to_show)
  } else {
    get_columns_to_show <- columns_to_show
  }
  
  if (! shiny::is.reactive(show_warnings)) {
    get_show_warnings <- shiny::reactive(show_warnings)
  } else {
    get_show_warnings <- show_warnings
  }
  
  if (! shiny::is.reactive(labels)) {
    get_labels <- shiny::reactive(labels)
  } else {
    get_labels <- labels
  }
  
  output$title <- renderUI({
    shiny::div(h2(get_labels()$title))
  })
  output$subtitle_num_var <- renderUI({
    shiny::div(h4(get_labels()$num_var))
  })
  output$subtitle_factor_var <- renderUI({
    shiny::div(h4(get_labels()$factor_var))
  })
  output$subtitle_date_var <- renderUI({
    shiny::div(h4(get_labels()$date_var))
  })
  
  output$have_data <- shiny::reactive({
    !is.null(get_data()) && "data.frame" %in% class(get_data()) && nrow(get_data()) > 0
  })
  shiny::outputOptions(output, "have_data", suspendWhenHidden = FALSE)
  
  data_num_fact <- shiny::reactive({
    data <- get_data()
    if(!"data.frame" %in% class(data)){
      data <- NULL
    } else {
      if(!"data.table" %in% class(data)){
        data <- data.table::as.data.table(data)
      }
    }
    
    if(!is.null(data) && nrow(data) > 0){
      columns_to_show <- get_columns_to_show()
      if(!"all" %in% columns_to_show){
        columns_to_show <- intersect(columns_to_show, colnames(get_data()))
        if(length(columns_to_show) == 0) columns_to_show <- NULL
      }
      if (is.null(columns_to_show) | "all" %in% columns_to_show) {
        data <- data
      } else {
        
        data <- data[, .SD, .SDcols = columns_to_show]
      }
      setcolorder(data, colnames(data)[order(colnames(data))])
      get_dt_num_dt_fac(data, optional_stats = get_optional_stats(), nb_modal2show = get_nb_modal2show(), 
                        show_warnings = get_show_warnings(), message = get_labels()$message, session = session)
    } else {
      NULL
    }
  })
  
  observe({
    res_num <- data_num_fact()$dt_num
    if(!is.null(res_num)){
      callModule(show_DT, "dt_num_ui", reactive(res_num$df), reactive(res_num$dt), 
                 paste0("Stats_num_", format(Sys.time(), format = "%d%m%Y_%H%M%S")))
    }
  })
  
  observe({
    res_dates <- data_num_fact()$dt_dates
    if(!is.null(res_dates)){
      callModule(show_DT, "dt_dates_ui", reactive(res_dates$df), reactive(res_dates$dt), 
                 paste0("Stats_dates_", format(Sys.time(), format = "%d%m%Y_%H%M%S")))
    }
  })
  
  observe({
    res_fact <- data_num_fact()$dt_fact
    if(!is.null(res_fact)){
      callModule(show_DT, "dt_fact_ui", reactive(res_fact$df), reactive(res_fact$dt), 
                 paste0("Stats_factor_", format(Sys.time(), format = "%d%m%Y_%H%M%S")))
    }
  })
  
  output$have_dt_num <- shiny::reactive({
    !is.null(data_num_fact()$dt_num)
  })
  shiny::outputOptions(output, "have_dt_num", suspendWhenHidden = FALSE)
  
  output$have_dt_dates <- shiny::reactive({
    !is.null(data_num_fact()$dt_dates)
  })
  shiny::outputOptions(output, "have_dt_dates", suspendWhenHidden = FALSE)
  
  output$have_dt_fact <- shiny::reactive({
    !is.null(data_num_fact()$dt_fact)
  })
  shiny::outputOptions(output, "have_dt_fact", suspendWhenHidden = FALSE)
}
