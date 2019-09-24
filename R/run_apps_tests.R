#' @title Run a shiny example application to visualise statistics
#' 
#' @description
#' Run a shiny application calling the module show_data, displaying three tables 
#' with descriptive statistics, one for numeric variables, one for dates and one for factor variables.
#' 
#' @param optional_stats \code{character} vector of the optional statistics you
#' want to be computed on your numeric data, default is "all". "pct_zero", 
#' "pct_NA", "mean", "median", "sd" are always computed, possible values are 
#' "min", "max", "var", "ecart_interquartile",
#' "mode_max", "kurtosis", "skewness", "boxplot", "density".
#' 
#' @param nb_modal2show \code{integer} number of modalities to show for factor variables.
#' 
#' @examples
#'
#' \dontrun{
#' 
#' optional_stats <- "all"
#' run_app_show_data <- 5
#' run_app_show_data(optional_stats, run_app_show_data)
#' 
#' optional_stats <- c("min", "max", "var", "boxplot", "density")
#' run_app_show_data(optional_stats, run_app_show_data)
#' }
#'
#' @import shiny
#' @export
run_example_app <- function(optional_stats = "all", nb_modal2show = 5){
  
  G <- .GlobalEnv
  assign("optional_stats", optional_stats, envir = G)
  assign("nb_modal2show", nb_modal2show, envir = G)
  
  shiny::runApp(system.file("test_modules/app_test_show_data.R", package = "shinymodules"),
                launch.browser = TRUE)
}





#' @title Run an R Markdown example application to visualise statistics
#' 
#' @description
#' Run an R Markdown application calling the function "get_dt_num_dt_fac", displaying three tables 
#' with descriptive statistics, one for numeric variables, one for dates and one for factor variables.
#' 
#' @param optional_stats \code{character} vector of the optional statistics you
#' want to be computed on your numeric data, default is "all". "pct_zero", 
#' "pct_NA", "mean", "median", "sd" are always computed, possible values are 
#' "min", "max", "var", "ecart_interquartile",
#' "mode_max", "kurtosis", "skewness", "boxplot", "density".
#' 
#' @param nb_modal2show \code{integer} number of modalities to show for factor variables.
#' @examples
#'
#' \dontrun{
#' 
#' optional_stats <- "all"
#' nb_modal2show <- 5
#' run_md_show_data(optional_stats, nb_modal2show)
#' 
#' optional_stats <- c("min", "max", "var", "boxplot", "density")
#' run_md_show_data(optional_stats, nb_modal2show)
#' }
#'
#' @import shiny nycflights13
#' @export
run_example_Rmd <- function(optional_stats = "all", nb_modal2show = 5){
  
  G <- .GlobalEnv
  assign("optional_stats", optional_stats, envir = G)
  assign("nb_modal2show", nb_modal2show, envir = G)
  
  rmarkdown::render(system.file("test_modules/md_test_show_data.Rmd", package = "shinymodules"),
                    output_dir = getwd(),
                    output_file = paste0("Rmd_example.html"),
                    quiet = T,
                    output_format = "html_document",
                    encoding = "UTF-8", run_pandoc = T)
}
