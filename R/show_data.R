#' @title UI part of the module show_data
#' 
#' @description Shiny module to show descripive statistics on data
#' 
#' @param id \code{character} An id that will be used to create a namespace
#' It returns two table, one with statistics on numeric data and one with 
#' statistics on factor data
#' @param titles \code{logical} Add titles on UI ? Default to TRUE
#' @param input Not a real parameter, should not be set manually. 
#' Done by callModule automatically.
#' @param output Not a real parameter, should not be set manually. 
#' Done by callModule automatically.
#' @param session Not a real parameter, should not be set manually. 
#' Done by callModule automatically.
#' @param data \code{reactivevalues} reactive data.table
#' @param optional_stats \code{character} optional statistics computed on numeric
#' data, default is "all". "pct_zero", "pct_NA", "mean", "median", "sd" are
#' always computed, possible values are "min", "max", "var", "ecart_interquartile",
#' "mode_max", "kurtosis", "skewness", "boxplot", "density".
#' @param nb_modal2show \code{integer} number of modalities to show 
#' for factor variables. 
#' @return shiny module
#' 
#' @export
#' 
#' @import shiny DT
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
#'   data <- reactiveValues(data = iris)
#'   shiny::callModule(module = show_data, id = "id", data = reactive(data$data),
#'     optional_stats = c("kurtosis", "density"))
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
show_dataUI <- function(id, titles = TRUE) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    ## Stats descriptives on numerical variables in a first tab
    ## then on factor variables in a second tab
    shiny::fluidRow(
      column(12,
             if(titles) shiny::div(h2("Descriptive statistics"))
      )
    ),
    shiny::fluidRow(
      column(12,
             shiny::conditionalPanel(
               condition = paste0("output['", ns("have_dt_num"), "'] === true"),
               if(titles) shiny::div(h4("Numeric variables")),
               DT::DTOutput(ns("dt_num")))
      )
    ),
    shiny::fluidRow(
      column(12,
             shiny::conditionalPanel(
               condition = paste0("output['", ns("have_dt_dates"), "'] === true"),
               shiny::hr(),
               if(titles) shiny::div(h4("Date variables")),
               DT::DTOutput(ns("dt_dates")))
      )
    ),
    shiny::fluidRow(
      column(12,
             shiny::conditionalPanel(
               condition = paste0("output['", ns("have_dt_fact"), "'] === true"),
               shiny::hr(),
               if(titles) shiny::div(h4("Factor variables")),
               DT::DTOutput(ns("dt_fact")))
      )
    )
  )
}


#' @export
#' @import shiny DT data.table magrittr sparkline PerformanceAnalytics htmlwidgets
#'
#' @rdname show_data_module
show_data <- function(input, output, session, data = NULL,
                      optional_stats = "all", nb_modal2show = 3) {
  ns <- session$ns
  data_num_fact <- shiny::reactive({ 
    get_dt_num_dt_fac(data(), optional_stats = optional_stats, nb_modal2show)
  })
  
  output$dt_num <- DT::renderDT({
    dt <- data_num_fact()$dt_num
    if(!is.null(dt)){
      dt 
    } else NULL
  })
  
  output$dt_dates <- DT::renderDT({
    dt <- data_num_fact()$dt_dates
    if(!is.null(dt)){
      dt 
    } else NULL
    
  })
  
  output$dt_fact <- DT::renderDT({
    dt <- data_num_fact()$dt_fact
    if(!is.null(dt)){
      dt 
    } else NULL
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
