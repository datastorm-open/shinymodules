#' @title UI part of the module show_data
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
#' show_dataUI(id = "id")
#' # In Server
#' callModule(module = show_data, id = "id", variable = reactive(data))
#' 
#' ## For a complete example, you can run the function \link{run_app_show_data}
#' optional_stats <- "all"
#' run_app_show_data(optional_stats)
#' 
#' }
#'
#' @export
show_dataUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    ## Stats descriptives on numerical variables in a first tab
    ## then on factor variables in a second tab
    shiny::fluidRow(
      column(12,
             shiny::div(h2("Descriptive statistics"))
      )
    ),
    shiny::fluidRow(
      column(12,
             shiny::conditionalPanel(
               condition = paste0("output['", ns("have_dt_num"), "'] === true"),
               DT::DTOutput(ns("dt_num")))
      )
    ),
    shiny::hr(),
    shiny::fluidRow(
      column(12,
             shiny::conditionalPanel(
               condition = paste0("output['", ns("have_dt_fact"), "'] === true"),
               DT::DTOutput(ns("dt_fact")))
      )
    )
  )
}

#' @title server part of the module show_data
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
#' @param optional_stats \code{character} optional statistics computed on numeric
#' data, default is "all". "pct_zero", "pct_NA", "mean", "median", "sd" are
#' always computed, possible values are "min", "max", "var", "ecart_interquartile",
#' "mode_max", "kurtosis", "skewness", "boxplot", "density".
#' 
#' @return Server logic
#' @export
#' @import shiny DT data.table magrittr sparkline PerformanceAnalytics htmlwidgets
#'
#' @examples 
#' \dontrun{
#' # In UI :
#' show_dataUI(id = "id")
#' # In Server
#' callModule(module = show_data, id = "id", variable = reactive(data))
#' 
#' ## For a complete example, you can run the function \link{run_app_show_data}
#'
#' optional_stats <- "all"
#' run_app_show_data(optional_stats)
#' 
#' }
#'
#' @export
show_data <- function(input, output, session, data = NULL,
                      optional_stats = "all") {
  ns <- session$ns
  data_num_fact <- shiny::reactive({ 
    get_dt_num_dt_fac(data(), optional_stats = optional_stats)
  })
  
  output$dt_num <- DT::renderDT({
    dt <- data_num_fact()$dt_num
    if(!is.null(dt)){
      dt %>%  DT::formatStyle(
        'pct_NA',
        color = DT::styleInterval(0, c("green", 'red'))
      ) %>% DT::formatPercentage(c('pct_NA', "pct_zero"), 2)
      
    } else NULL
    
  })
  
  output$dt_fact <- DT::renderDT({
    dt <- data_num_fact()$dt_fact
    if(!is.null(dt)){
      dt %>%  DT::formatStyle(
        'pct_NA',
        color = DT::styleInterval(0, c("green", 'red'))
      ) %>% DT::formatPercentage(c(
        'pct_NA'), 2) %>%
        DT::formatStyle(
          'nb_modalities',
          color = DT::styleInterval(1, c('red', "black")))
    } else NULL
    
  })
  
  output$have_dt_num <- shiny::reactive({
    !is.null(data_num_fact()$dt_num)
  })
  shiny::outputOptions(output, "have_dt_num", suspendWhenHidden = FALSE)
  
  output$have_dt_fact <- shiny::reactive({
    !is.null(data_num_fact()$dt_fact)
  })
  shiny::outputOptions(output, "have_dt_fact", suspendWhenHidden = FALSE)
  
  
}
