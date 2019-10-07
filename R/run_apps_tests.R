#' @title Run a shiny example application to visualise statistics
#' 
#' @description
#' Run a shiny application calling the module show_data, displaying three tables 
#' with descriptive statistics, one for numeric variables, one for dates and one for factor variables.
#' 
#' @param optional_stats \code{character} vector of the optional statistics you
#' want to be computed on your numeric data, default is "all". "pct_zero", 
#' "pct_NA", "mean", "median", "sd" are always computed, possible values are 
#' "min", "max", "var", "interquartile range",
#' "mode_max", "kurtosis", "skewness", "boxplot", "density".
#' 
#' @param nb_modal2show \code{integer} number of modalities to show for factor variables.
#' 
#' @examples
#'
#' \dontrun{
#' 
#' optional_stats <- "all"
#' nb_modal2show <- 5
#' run_example_app_show_data(optional_stats, nb_modal2show)
#' 
#' optional_stats <- c("min", "max", "var", "boxplot", "density")
#' run_example_app_show_data(optional_stats, nb_modal2show)
#' }
#'
#' @import shiny
#' @export
run_example_app_show_data <- function(optional_stats = "all", nb_modal2show = 5){
  
  G <- .GlobalEnv
  assign("optional_stats", optional_stats, envir = G)
  assign("nb_modal2show", nb_modal2show, envir = G)
  
  shiny::runApp(system.file("test_modules/app_test_show_data.R", package = "shinymodules"),
                launch.browser = TRUE)
}


#' @title Run a shiny example application to filter data and download the filtered
#' data
#' 
#' @description
#' Run a shiny application calling the module filter_data, allowing the user
#' to use filters on the different variables of his data
#' 
#' @examples
#'
#' \dontrun{
#' 
#' run_example_app_filter_data()
#' }
#'
#' @import shiny
#'
#' @export
run_example_app_filter_data <- function(){
  
  shiny::runApp(system.file("test_modules/app_test_filter_data.R", 
                            package = "shinymodules"), launch.browser = TRUE)
}


#' @title Run a shiny example application to filter on data and
#'  visualise statistics on the filtered data
#'  
#' @description
#' Run a shiny application calling the modules show_data and filter_data, 
#' allowing the user to use filters on the different variables of his data 
#' and then displaying two tables with descriptive statistics, 
#' one for numeric variables and one for factor variables.
#' 
#' @param optional_stats \code{character} vector of the optional statistics you
#' want to be computed on your numeric data, default is "all". "pct_zero", 
#' "pct_NA", "mean", "median", "sd" are always computed, possible values are 
#' "min", "max", "var", "interquartile range",
#' "mode_max", "kurtosis", "skewness", "boxplot", "density".
#' @param nb_modal2show \code{integer} number of modalities to show 
#' for factor variables.
#' 
#' @examples 
#' \dontrun{
#' 
#' optional_stats <- c("min", "max", "var", "boxplot", "density")
#' nb_modal2show <- 5
#' run_example_app_filter_and_show_data(optional_stats)
#' }
#'
#' @import shiny
#'
#' @export
run_example_app_filter_and_show_data <- function(
  optional_stats = "all", nb_modal2show = 5){
  
  G <- .GlobalEnv
  assign("optional_stats", optional_stats, envir = G)
  assign("nb_modal2show", nb_modal2show, envir = G)
  shiny::runApp(system.file("test_modules/app_test_filter_and_show_data.R", 
                            package = "shinymodules"), launch.browser = TRUE)
  
}


#' @title Run a shiny example application to visualize graphics on data
#'  
#' @description
#' Run a shiny application calling the module visualize_data, 
#' allowing the user to visualize his data with diffrent type of graphics 
#' 
#' @examples 
#' \dontrun{
#' 
#' run_example_app_visualize_data()
#' }
#'
#' @import shiny
#'
#' @export
run_example_app_visualize_data <- function() {
  
  G <- .GlobalEnv
  shiny::runApp(system.file("test_modules/app_test_visualize_data.R", 
                            package = "shinymodules"), launch.browser = TRUE)
  
}