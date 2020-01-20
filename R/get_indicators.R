#' @title  preprocessing on data 
#' @description preprocessing of input data, return two DT, one with
#' statistics on numeric variables and another one with statistics on factor variables
#' 
#' @param data \code{data.frame}, \code{data.table} input data which will be preprocessed
#' @param optional_stats \code{character} optional statistics computed on data,
#' you can look at \link{show_data} for more information.
#' @param nb_modal2show \code{integer} number of modalities to show for factor variables.
#' @param show_warnings \code{logical} Show warnings ? (example compute Min. on all NAs)
#' @param keep_dataframe \code{logical} data.frame in output ?
#' @param keep_datatable \code{logical} datatable (DT) in output ?
#' 
#' @return list with two DT, one with statistics on numeric data and one with
#' statistics on factor data
#' @import data.table
#' @export
#' @examples 
#' \dontrun{
#' get_dt_num_dt_fac(mtcars, optional_stats = c("min", "max", "boxplot", "density"))
#' 
#' get_dt_num_dt_fac(airquality, optional_stats = "all")
#' 
#' }
#' 
get_dt_num_dt_fac <- function(data, optional_stats, nb_modal2show, 
                              show_warnings = FALSE, keep_dataframe = TRUE, keep_datatable = TRUE) {
  
  if(!show_warnings){
    user_opt_warn <- getOption("warn")
    options(warn = -1)
  }
  
  if(!"data.table" %in% class(data)){
    data <- data.table::as.data.table(data)
  }
  
  # get variables with 2 to 10 levels then pass them to factors
  # fact_vars <- names(which(unlist(data[, lapply(
  #   .SD, function(var) length(unique(na.omit(var))) %in% 2:10)])))
  fact_vars <- names(
    which(
      sapply(data, function(var) is.factor(var) | is.character(var) | is.logical(var))
    )
  )
  # if(length(fact_vars) > 0) {
  #   data[, c(fact_vars) := lapply(.SD, as.factor), .SDcols = c(fact_vars)]
  # }
  
  # get other numeric variables
  num_vars <- names(
    which(
      sapply(data, function(var) is.numeric(var) | is.integer(var))
    )
  )
  # add Dates on numeric variables ?
  dates_vars <- names(
    which(
      sapply(data, function(var) any(class(var) %in% c("Date", "POSIXct", "POSIXlt")))
    )
  )
  
  # generate table with stats on numeric
  if (length(num_vars) > 0) {
    dt_num <- .get_stat_indicators(data, num_vars, optional_stats = optional_stats, 
                                   keep_dataframe = keep_dataframe, keep_datatable = keep_datatable)
  } else {
    dt_num <- NULL
  }
  
  # generate table with stats on dates
  if (length(dates_vars) > 0) {
    dt_dates <- .get_dates_indicators(data, dates_vars, optional_stats = optional_stats, 
                                      keep_dataframe = keep_dataframe, keep_datatable = keep_datatable)
  } else {
    dt_dates <- NULL
  }
  
  
  # same for factors
  if (length(fact_vars) > 0) {
    dt_fact <- .get_factor_indicators(data, fact_vars, nb_modal2show, optional_stats = optional_stats, 
                                      keep_dataframe = keep_dataframe, keep_datatable = keep_datatable)
  } else {
    dt_fact <- NULL
  }
  
  if(!show_warnings){
    options(warn = user_opt_warn)
  }
  
  list(dt_num = dt_num, dt_dates = dt_dates, dt_fact = dt_fact)
}

#' @title  get stats indicator on numeric column
#' @description return statistics from numeric data, is called by 
#' \link{.get_stat_indicators}
#' @param data \code{data.frame}, \code{data.table}
#' @param var \code{character}
#' @param absolute \code{logical}
#' @param optional_stats \code{character} optional statistics computed on data,
#' you can look at \link{show_data} for more information.
#' @return data.table with statistics on numeric data
#' @import sparkline PerformanceAnalytics
#' 
.get_indicators <- function(data, var, absolute = FALSE, optional_stats = "all"){
  
  tmp_compute_ind <- NULL
  if(!"data.table" %in% class(data)){
    data <- data.table::as.data.table(data)
  }
  
  if(absolute){
    data[, tmp_compute_ind := abs(get(var))]
  } else {
    data[, tmp_compute_ind := get(var)]
  }
  
  range_ind <- range(data$tmp_compute_ind, na.rm = TRUE)
  
  getmode <- function(x, ...) {
    #dstx <- density(x, ...)
    dstx <- tryCatch(density(x, ...), error = function(e) NULL)
    mode <- ifelse(is.null(dstx), NA, dstx$x[which(dstx$y == max(dstx$y))])
  }
  
  getboxplotValues <- function(x){
    if(!all(is.na(x)) && length(x) >4){
      x_box <- grDevices::boxplot.stats(x)
      x_out_range <- ifelse(length(x_box$out)>=2, range(x_box$out),NA)
      return(sort(c(x_box$stats, x_out_range)))
    } else{
      return(NA)
    }
  }
  
  ref_ind <- data.frame(
    indicator= c( "pct_zero", "pct_NA", "nb_valid", "min", "mean", 
                  "median", "max", "sd", "var",
                  "interquartile_range",
                  "mode_max",
                  "kurtosis",
                  "skewness",
                  "boxplot", 
                  "density"),
    
    code = c("length(which(tmp_compute_ind == 0))/.N",
             "length(which(is.na(tmp_compute_ind)))/.N",
             "length(which(!is.na(tmp_compute_ind)))",
             "round(min(tmp_compute_ind, na.rm = T), 2)",
             "round(mean(tmp_compute_ind, na.rm = T), 2)",
             "round(stats::median(tmp_compute_ind, na.rm = T), 2)",
             "round(max(tmp_compute_ind, na.rm = T), 2)",
             "round(stats::sd(tmp_compute_ind, na.rm = TRUE), 2)",
             "round(stats::var(tmp_compute_ind, na.rm = TRUE), 2)",
             "round(stats::IQR(tmp_compute_ind, na.rm = TRUE), 2)",
             "round(getmode(tmp_compute_ind, na.rm = TRUE), 2)",
             "round(PerformanceAnalytics::kurtosis(tmp_compute_ind, na.rm = TRUE), 2)",
             "round(PerformanceAnalytics::skewness(tmp_compute_ind, na.rm = TRUE), 2)",
             # "spk_chr(getboxplotValues(tmp_compute_ind), type ='box', raw=TRUE , chartRangeMin = range_ind[1], chartRangeMax = range_ind[2], boxLineColor = '#0000ff', boxFillColor = '#aaffff', whiskerColor = '#0000ff', outlierLineColor = '#0000ff', medianColor = '#0000ff', outlierLineColor = '#0000ff', outlierFillColor = '#aad4ff')",
             "spk_chr(getboxplotValues(tmp_compute_ind), type ='box', raw=TRUE , boxLineColor = '#0000ff', boxFillColor = '#aaffff', whiskerColor = '#0000ff', outlierLineColor = '#0000ff', medianColor = '#0000ff', outlierLineColor = '#0000ff', outlierFillColor = '#aad4ff')",
             "spk_chr(tryCatch(density(tmp_compute_ind, n= 100, na.rm = T)$y, error = function(e) NULL))"),
    
    stringsAsFactors = F)
  rownames(ref_ind) <- ref_ind$indicator
  
  expr_calcul <- paste0("list(", paste(paste(
    ref_ind[, "indicator"], ref_ind[, "code"], sep = " = "), collapse = ","), ")")
  
  stats_desc_global <- data[, eval(parse(text = expr_calcul))]
  if (!("all" %in% optional_stats)) {
    expected_cols <- c(
      "pct_NA", "pct_zero", "mean", "median", "sd", optional_stats
    )
    valid_cols <- expected_cols[expected_cols %in% colnames(stats_desc_global)]
    stats_desc_global <- stats_desc_global[, .SD, .SDcols = valid_cols]
  }
  if ("nb_valid" %in% colnames(stats_desc_global)) {
    setcolorder(stats_desc_global, c("pct_NA", "nb_valid", "pct_zero"))
  }
  return(stats_desc_global)
}

#' @title  get stats indicator on numeric data
#' @description return statistics from numeric data, is called by 
#' \link{get_dt_num_dt_fac}
#' @param data \code{data.frame}, \code{data.table}
#' @param vars \code{character}
#' @param optional_stats \code{character} optional statistics computed on data,
#' you can look at \link{show_data} for more information.
#' @param keep_dataframe \code{logical} data.frame in output ?
#' @param keep_datatable \code{logical} datatable (DT) in output ?
#' 
#' @return DT with statistics on numeric data
#' @import sparkline PerformanceAnalytics
#' @importFrom DT renderDT datatable formatStyle styleInterval formatPercentage formatCurrency
.get_stat_indicators <- function(data, vars, optional_stats, keep_dataframe = TRUE, keep_datatable = TRUE){
  
  if(!"data.frame" %in% class(data)){
    stop("'data' must be a data.frame / data.table.")
  }
  
  if(!"data.table" %in% class(data)){
    data_copy <- data.table::as.data.table(data)
  } else {
    data_copy <- data.table::copy(data)
  }
  
  res <- sapply(vars, function(var) .get_indicators(
    data_copy, var, optional_stats = optional_stats), simplify = FALSE)
  stats_table <- data.table::rbindlist(res, use.names = TRUE, idcol = "variable")
  
  if (!all(optional_stats %in% c(colnames(stats_table), "all"))) {
    fake_stats <- optional_stats[!(optional_stats %in% c(colnames(stats_table), "all"))]
    warning(paste("One or more of optional statistics selected are not in the
            available ones and will be ignored: ", paste(fake_stats, collapse = ", ")))
  }
  
  if(keep_datatable){
    container <-  set_datatable_var_info(colnames(stats_table), vars_infos = .stats_info)
    dt <- DT::datatable(stats_table, rownames = FALSE, filter = "bottom", 
                        container = container,
                        escape = FALSE, selection = "none", width = "100%",
                        options = list(
                          pageLength = 20, lengthMenu = c(5, 10, 20, 50), 
                          dom = 'Blfrtip', scrollX = TRUE,
                          drawCallback =  htmlwidgets::JS(
                            'function(){debugger;HTMLWidgets.staticRender();}'),
                          columnDefs = list(
                            list(className = 'dt-center', 
                                 targets = c(ncol(stats_table)-1:ncol(stats_table))))
                        )) %>%  DT::formatStyle(
                          'pct_NA',
                          color = DT::styleInterval(0, c("green", 'red'))
                        ) %>% DT::formatPercentage(
                          c('pct_NA', "pct_zero"), 2
                        ) 
    names_num <- attr(dt$x, "colnames")[which(
      !(attr(dt$x, "colnames") %in% c(
        "pct_NA", "pct_zero", "boxplot", "density")))]
    
    dt <- dt %>% DT::formatCurrency(
      names_num, currency = "", interval = 3, mark = " ")
    
    if ("nb_valid" %in% optional_stats | "all" %in% optional_stats) {
      dt <- dt %>% DT::formatCurrency(
        "nb_valid", currency = "", interval = 3, mark = " ", digits = 0)
    }
    dt$dependencies <- append(dt$dependencies, htmlwidgets::getDependency("sparkline"))   
  }
 
  stats_table$density <- NULL
  stats_table$boxplot <- NULL
  
  if(keep_datatable & !keep_dataframe){
    return(dt)
  } else if(!keep_datatable & keep_dataframe){
    return(stats_table)
  } else {
    return(list(df = stats_table, dt = dt))
  }
  
}

#' @title  get stats indicator on dates data
#' @description return statistics from dates data, is called by 
#' \link{get_dt_num_dt_fac}
#' @param data \code{data.frame}, \code{data.table}
#' @param dates_vars \code{character}
#' @param optional_stats \code{character} optional statistics computed on data,
#' you can look at \link{show_data} for more information.
#' @param keep_dataframe \code{logical} data.frame in output ?
#' @param keep_datatable \code{logical} datatable (DT) in output ?
#' 
#' @return DT with statistics on dates data
#' 
.get_dates_indicators <- function(data, dates_vars, optional_stats = "all", 
                                  keep_dataframe = TRUE, keep_datatable = TRUE){
  
  if(!"data.frame" %in% class(data)){
    stop("'data' must be a data.frame / data.table.")
  }
  
  if(!"data.table" %in% class(data)){
    data <- data.table::as.data.table(data)
  }
  
  stats_dates <- data.table::rbindlist(lapply(
    dates_vars, 
    FUN = function(var){
      density <- spk_chr(tryCatch(density(
        data[, as.numeric(get(var))], n= 100, na.rm = T)$y, 
        error = function(e) NULL))
      pct_na <- sum(is.na(data[[var]]))/nrow(data)
      nb_valid <- length(which(!is.na(data[[var]])))
      
      data.table::data.table(variable = var,
                             pct_NA = pct_na,
                             nb_valid = nb_valid,
                             min = min(data[[var]], na.rm = T),
                             mean = mean(data[[var]], na.rm = T),
                             median = stats::median(data[[var]], na.rm = T),
                             max = max(data[[var]], na.rm = T),
                             density = density
      )}))
  if (!("nb_valid" %in% optional_stats | "all" %in% optional_stats)) {
    stats_dates[, nb_valid := NULL]
  }
  
  if(keep_datatable){
  container <-  set_datatable_var_info(colnames(stats_dates), vars_infos = .stats_info)
  dt_dates <- DT::datatable(stats_dates, container = container,
                            rownames = FALSE, filter = "bottom", escape = FALSE, 
                            selection = "none", width = "100%",
                            options = list(scrollX = TRUE, columnDefs = list(
                              list(className = 'dt-center', targets = 0:5,
                                   drawCallback =  htmlwidgets::JS(
                                     'function(){debugger;HTMLWidgets.staticRender();}')
                              )))) %>%  DT::formatStyle(
                                'pct_NA',
                                color = DT::styleInterval(0, c("green", 'red'))
                              ) %>% DT::formatPercentage(c(
                                'pct_NA'), 2
                              )
  
  if ("nb_valid" %in% optional_stats | "all" %in% optional_stats) {
    dt_dates <- dt_dates %>% DT::formatCurrency(
      "nb_valid", currency = "", interval = 3, mark = " ", 
      digits = 0)
  }
  dt_dates$dependencies <- append(dt_dates$dependencies, 
                                  htmlwidgets::getDependency("sparkline"))
  } 
  
  stats_dates$density <- NULL
  
  if(keep_datatable & !keep_dataframe){
    return(dt_dates)
  } else if(!keep_datatable & keep_dataframe){
    return(stats_dates)
  } else {
    return(list(df = stats_dates, dt = dt_dates))
  }
  
}

#' @title  get stats indicator on factor data
#' 
#' @description return statistics from factor data, is called by 
#' 
#' \link{get_dt_num_dt_fac}
#' 
#' @param data \code{data.frame}, \code{data.table}
#' @param fact_vars \code{character}
#' @param nb_modal2show \code{integer}
#' @param optional_stats \code{character} optional statistics computed on data,
#' you can look at \link{show_data} for more information.
#' @param keep_dataframe \code{logical} data.frame in output ?
#' @param keep_datatable \code{logical} datatable (DT) in output ?
#' 
#' @return DT with statistics on factor data
#' @import sparkline PerformanceAnalytics
#' 
.get_factor_indicators <- function(data, fact_vars, nb_modal2show, optional_stats = "all", 
                                   keep_dataframe = TRUE, keep_datatable = TRUE) {
  
  if(!"data.frame" %in% class(data)){
    stop("'data' must be a data.frame / data.table.")
  }
  
  if(!"data.table" %in% class(data)){
    data <- data.table::as.data.table(data)
  }
  
  N <- NULL
  
  stats_fact <- data.table::rbindlist(lapply(
    fact_vars, 
    FUN = function(var){
      
      # get details of factor
      data_det <- data[!is.na(get(var)), .N, var][order(-N)]
      modalities <- paste(data_det[, get(var)], ":", 
                          round(100*data_det[, N]/nrow(data), 2), "%")
      
      # control modalities
      if(length(modalities) < nb_modal2show) {
        lendiff <- nb_modal2show - length(modalities)
        modalities <- c(modalities, rep("", lendiff))
      } else if(length(modalities) > nb_modal2show) {
        modalities <- modalities[1:nb_modal2show]
      }
      
      half1 <- gsub(" [0-9]{1,}\\.[0-9]{1,} %| [0-9]{1,} %", "", modalities)
      half2 <- gsub("^.*:", "", modalities)
      modalities <- paste("<i>", half1, "</i> <b>", half2, "</b>")
      
      other <- paste(round(100*(
        1-sum(data_det[1:min(c(nb_modal2show, nrow(data_det))), 
                       N])/nrow(data)), 2), "%")
      
      # get pct na
      pct_na <- sum(is.na(data[[var]]))/nrow(data)
      nb_valid <- length(which(!is.na(data[[var]])))
      data_fac <- data.table::data.table(variable = var,
                                         nb_modalities = nrow(data_det),
                                         pct_NA = pct_na,
                                         nb_valid = nb_valid)
      
      # give modalities
      data_fac[, paste(sapply(1:nb_modal2show, function(i){
        paste0("modality", i)})) := as.list(modalities)]
      data_fac[, other := other]
      
      if (!("nb_valid" %in% optional_stats | "all" %in% optional_stats)) {
        data_fac[, nb_valid := NULL]
      }
      data_fac
    }))
  
  if(keep_datatable){
    container <-  set_datatable_var_info(colnames(stats_fact), nb_modal2show = nb_modal2show, 
                                         vars_infos = .stats_info)
    
    dt_fact <- DT::datatable(stats_fact, container = container,
                             rownames = FALSE, filter = "bottom", escape = FALSE, 
                             selection = "none", width = "100%",
                             options = list(scrollX = TRUE, columnDefs = list(
                               list(className = 'dt-center', targets = 0:5
                               )))) %>%  DT::formatStyle(
                                 'pct_NA',
                                 color = DT::styleInterval(0, c("green", 'red'))
                               ) %>% DT::formatPercentage(c(
                                 'pct_NA'), 2
                               )
    
    if ("nb_valid" %in% optional_stats | "all" %in% optional_stats) {
      dt_fact <- dt_fact %>% DT::formatCurrency(
        "nb_valid", currency = "", interval = 3, mark = " ", 
        digits = 0)
    }    
  }
  
  mod_colum <- colnames(stats_fact)[grep("^modality", colnames(stats_fact))]
  if(length(mod_colum) > 0){
    stats_fact[, c(mod_colum) := lapply(.SD, function(x){
      gsub("(<i>)|(</i>)|([[:space:]]{1}<b>[[:space:]]{1})|(</b>)","", x)
    }), .SDcols = mod_colum]
  }
  
  if(keep_datatable & !keep_dataframe){
    return(dt_fact)
  } else if(!keep_datatable & keep_dataframe){
    return(stats_fact)
  } else {
    return(list(df = stats_fact, dt = dt_fact))
  }

}



#' Add information on variables in DT table 
#'
#' @param cols \code{character}. Columns names the table to be displayed.
#' @param vars_infos  \code{data.frame}, \code{data.table} Table with cols 'name' and 'info'.
#' @param nb_modal2show \code{integer}. todo.
#'
#' @return DT container arg.
#' @export
#' 
#' @import data.table
#' @importFrom htmltools withTags
#'
#' @examples
#' \dontrun{\donttest{
#' 
#' library(shiny)
#' 
#' # create table of variables informations
#' vars_infos <- data.frame(
#'   # names of all possible variables 
#'   name = c("Sepal.Length", "Species", "Petal.Length", "Sepal.Width", "Petal.Width"), 
#'   # corresponding infos
#'   info = c("Infos about Sepal.Length", "Infos about Species", "Infos about Petal.Length", 
#'            "Infos about Sepal.Width", "Infos about Petal.Width")
#' )
#' 
#' # shiny app
#' ui <- fluidPage(
#'   fluidRow(
#'     column(12,
#'            DT::DTOutput("my_DT_table")        
#'     )
#'   )
#' )
#' 
#' server <- function(input, output, session) {
#'   output$my_DT_table <- DT::renderDT({
#'     DT::datatable(
#'       data.table(iris), 
#'       
#'       rownames = F, filter = 'bottom', 
#'       
#'       container = set_datatable_var_info(
#'         cols = names(iris),
#'         vars_infos = vars_infos
#'       )
#'     )
#'   })
#' }
#' 
#' shinyApp(ui, server)
#' 
#' }}
set_datatable_var_info <- function(cols, 
                            vars_infos, 
                            nb_modal2show = NULL) {
  
  if(!"data.frame" %in% class(vars_infos)){
    stop("'vars_infos' must be a data.frame / data.table.")
  }
  
  if(!"data.table" %in% class(vars_infos)){
    vars_infos <- data.table::as.data.table(vars_infos)
  }
  
  stopifnot(all.equal(colnames(vars_infos), c("name", "info")))
  
  # ?
  if (!is.null(nb_modal2show)) {
    nb_modal2show <- max(4, nb_modal2show)
    dt_modal <- data.table(name = c(paste0("modality", 1:nb_modal2show), "other"),
                           info = c("1st occuring modality", 
                                    "2nd occuring modality", 
                                    "3rd occuring modality",
                                    paste0(4:nb_modal2show, "th occuring modality"),
                                    "percentage of observations with other values than the displayed modalities"))
    vars_infos <- rbindlist(list(vars_infos, dt_modal))
  }
  
  # retrieve infos
  htmlbodyth <- as.list(cols)
  
  htmlbodytitle <- c(unname(sapply(cols, function(col) {
    if (col %in% vars_infos$name) {
      vars_infos[name == col, as.character(info)] 
    } else {
      col
    }
  })))
  
  # compute DT container
  container = htmltools::withTags(
    table(
      class = 'display',
      thead(
        tr(lapply(1:length(htmlbodyth), function(val) {
          th(htmlbodyth[val], title = htmlbodytitle[val])
        }))
      )
    )
  )
  
  return(container)
}