#' @title Run a shiny example application to visualise statistics
#' 
#' @description
#' Run a shiny application calling the module show_data, displaying two tables 
#' with descriptive statistics, one for numeric variables and one for factor variables.
#' 
#' @param optional_stats \code{character} vector of the optional statistics you
#' want to be computed on your numeric data, default is "all". "pct_zero", 
#' "pct_NA", "mean", "median", "sd" are always computed, possible values are 
#' "min", "max", "var", "ecart_interquartile",
#' "mode_max", "kurtosis", "skewness", "boxplot", "density".
#' @examples
#'
#' \dontrun{
#' 
#' optional_stats <- "all"
#' run_app_show_data(optional_stats)
#' 
#' optional_stats <- c("min", "max", "var", "boxplot", "density")
#' run_app_show_data(optional_stats)
#' }
#'
#' @import shiny
#'
#' @export
run_app_show_data <- function(optional_stats){
  
  G <- .GlobalEnv
  assign("optional_stats", optional_stats, envir = G)
  shiny::runApp(system.file("test_modules/app_test_show_data.R", package = "shinymodules"),
                launch.browser = TRUE)
}