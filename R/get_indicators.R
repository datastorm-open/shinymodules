#' @title  preprocessing on data 
#' @description preprocessing of input data, return two DT, one with
#' statistics onnumeric variables and another one with statistics on factor variables
#' 
#' @param data \code{data.table} input data which will be preprocessed
#' @param optional_stats \code{character} optional statistics computed on data,
#' you can look at \link{show_data} for more information.
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
get_dt_num_dt_fac <- function(data, optional_stats) {
  # check if there is constant variables, is yes ignore them
  data.table::setDT(data)
  cst_vars <- names(which(unlist(data[, lapply(
    .SD, function(var) length(unique(var)))]) == 1))
  data[, c(cst_vars) := lapply(.SD, as.character), .SDcols = c(cst_vars)]
  
  # get variables with 2 to 10 levels then pass them to factors
  fact_vars <- names(which(unlist(data[, lapply(
    .SD, function(var) length(unique(na.omit(var))) %in% 2:10)])))
  data[, c(fact_vars) := lapply(.SD, as.factor), .SDcols = c(fact_vars)]
  
  # get other numeric variables
  num_vars <- names(which(
    sapply(data, function(var) is.numeric(var) | is.integer(var))))
  
  # generate table with stats on numeric
  if (length(num_vars) > 0) {
    dt_num <- get_stat_indicators(data, num_vars, optional_stats = optional_stats)
  } else {
    dt_num <- NULL
  }

  # same for factors
  if (length(fact_vars) > 0) {
    dt_fact <- get_factor_indicators(data, fact_vars)
  } else {
    dt_fact <- NULL
  }
  list(dt_num = dt_num, dt_fact = dt_fact)
}

#' @title  get stats indicator on numeric column
#' @description return statistics from numeric data, is called by 
#' \link{get_stat_indicators}
#' @param data \code{data.table}
#' @param var \code{character}
#' @param absolute \code{logical}
#' @param optional_stats \code{character}
#' @return data.table with statistics on numeric data
#' @import sparkline PerformanceAnalytics
#' 
get_indicators <- function(data, var, absolute = FALSE, optional_stats){
  

  data.table::setDT(data)
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
    indicator= c( "pct_zero", "pct_NA", "min", "mean", "median", "max", "sd", "var",
                  "ecart_interquartile",
                  "mode_max",
                  "kurtosis",
                  "skewness",
                  "boxplot", 
                  "density"),
    
    code = c("length(which(tmp_compute_ind == 0))/.N",
             "length(which(is.na(tmp_compute_ind)))/.N",
             "round(min(tmp_compute_ind, na.rm = T), 2)",
             "round(mean(tmp_compute_ind, na.rm = T), 2)",
             "round(median(tmp_compute_ind, na.rm = T), 2)",
             "round(max(tmp_compute_ind, na.rm = T), 2)",
             "round(sd(tmp_compute_ind, na.rm = TRUE), 2)",
             "round(var(tmp_compute_ind, na.rm = TRUE), 2)",
             "round(IQR(tmp_compute_ind, na.rm = TRUE), 2)",
             "round(getmode(tmp_compute_ind, na.rm = TRUE), 2)",
             "round(kurtosis(tmp_compute_ind, na.rm = TRUE), 2)",
             "round(skewness(tmp_compute_ind, na.rm = TRUE), 2)",
             # "spk_chr(getboxplotValues(tmp_compute_ind), type ='box', raw=TRUE , chartRangeMin = range_ind[1], chartRangeMax = range_ind[2], boxLineColor = '#0000ff', boxFillColor = '#aaffff', whiskerColor = '#0000ff', outlierLineColor = '#0000ff', medianColor = '#0000ff', outlierLineColor = '#0000ff', outlierFillColor = '#aad4ff')",
             "spk_chr(getboxplotValues(tmp_compute_ind), type ='box', raw=TRUE , boxLineColor = '#0000ff', boxFillColor = '#aaffff', whiskerColor = '#0000ff', outlierLineColor = '#0000ff', medianColor = '#0000ff', outlierLineColor = '#0000ff', outlierFillColor = '#aad4ff')",
             "spk_chr(tryCatch(density(tmp_compute_ind, n= 100, na.rm = T)$y, error = function(e) NULL))"),
    
    stringsAsFactors = F)
  rownames(ref_ind) <- ref_ind$indicator
  
  expr_calcul <- paste0("list(", paste(paste(
    ref_ind[, "indicator"], ref_ind[, "code"], sep = " = "), collapse = ","), ")")
  
  stats_desc_global <- data[, eval(parse(text = expr_calcul))]
  if (!("all" %in% optional_stats)) {
    stats_desc_global <- stats_desc_global[, .SD, .SDcols = c(
      "pct_zero", "pct_NA", "mean", "median", "sd", optional_stats
    )]
  }


  return(stats_desc_global)
}

#' @title  get stats indicator on numeric data
#' @description return statistics from numeric data, is called by 
#' \link{get_dt_num_dt_fac}
#' @param data \code{data.table}
#' @param vars \code{character}
#' @param optional_stats \code{character}
#' @return DT with statistics on numeric data
#' @import sparkline PerformanceAnalytics
#' 
get_stat_indicators <- function(data, vars, optional_stats){
  data_copy <- data.table::copy(data)
  res <- sapply(vars, function(var) get_indicators(
    data_copy, var, optional_stats = optional_stats), simplify = FALSE)
  stats_table <- data.table::rbindlist(res, use.names=TRUE, idcol = "variable")
  
  dt <- DT::datatable(stats_table, rownames = FALSE, filter = "bottom", 
                      escape = FALSE, selection = "none", width = "100%",
                      options = list(
                        pageLength = 20, lengthMenu = c(5, 10, 20, 50), 
                        dom = 'Blfrtip', scrollX = TRUE,
                        drawCallback =  htmlwidgets::JS(
                          'function(){debugger;HTMLWidgets.staticRender();}'),
                        columnDefs = list(
                          list(className = 'dt-center', 
                               targets = c(ncol(stats_table)-1:ncol(stats_table))))
                      )) 
  
  dt$dependencies <- append(dt$dependencies, htmlwidgets::getDependency("sparkline"))
  return(dt)
}

#' @title  get stats indicator on factor data
#' @description return statistics from factor data, is called by 
#' \link{get_dt_num_dt_fac}
#' @param data \code{data.table}
#' @param fact_vars \code{character}
#' @return DT with statistics on factor data
#' @import sparkline PerformanceAnalytics
#' 
get_factor_indicators <- function(data, fact_vars) {
  dt_fact <- DT::datatable(
    data.table::rbindlist(lapply(
      fact_vars, 
      FUN = function(var){
        # get details of factor
        data_det <- data[!is.na(get(var)), .N, var][order(-N)]
        modalities <- paste(data_det[, get(var)], ":", 
                            round(100*data_det[, N]/nrow(data), 2), "%")
        # get pct na
        pct_na <- round(sum(is.na(data[[var]]))/nrow(data), 2)

        data.table::data.table(variable = var,
                   nb_modalities = nrow(data_det),
                   pct_NA = pct_na,
                   modality1 = modalities[1],
                   modality2 = modalities[2],
                   modality3 = modalities[3]
        )})),
    rownames = FALSE, filter = "bottom", escape = FALSE, 
    selection = "none", width = "100%",
    options = list(columnDefs = list(
      list(className = 'dt-center', targets = 0:5
      )))) 
}