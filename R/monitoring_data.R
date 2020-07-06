#' Indicators funs
#'
#' @param real \code{numeric}. Observed values.
#' @param fit \code{numeric}. Fitted values.
#' @param na.rm \code{boolean}. Whether to ignore NA values.
#'
#' @return a  numeric. The value of the called indicator.
#'
#' @examples
#' \dontrun{\donttest{
#' 
#' real <- round(runif(100, 1, 10))
#' fit <- rnorm(100, 1, 5)
#' 
#' mape <- .mape(real, fit)
#' mape_e <- .mape_e(real, fit)
#' mae <- .mae(real, fit)
#' rmse <- .rmse(real, fit)
#' 
#' }}
.rmse <- function(real, fit, na.rm = TRUE) {
  if (length(real) != length(fit)) {
    stop("'real' & 'fit' must have same number of element")
  }
  sqrt(mean((real - fit) ^ 2, na.rm=na.rm))
}
.mae <- function(real, fit, na.rm = TRUE) {
  if (length(real) != length(fit)) {
    stop("'real' & 'fit' must have same number of element")
  }
  mean(abs(real - fit), na.rm=na.rm)
}
.mape <- function(real, fit, na.rm = TRUE) {
  if (length(real) != length(fit)) {
    stop("'real' & 'fit' must have same number of element")
  }
  
  if (any(real[!is.na(real)] == 0)) {
    ind <- which(real == 0)
    real <- real[-ind]
    fit <- fit[-ind]
  }
  mean(abs((real - fit) / real), na.rm=na.rm)
}
.mape_e <- function(real, fit, na.rm = TRUE) {
  if (length(real) != length(fit)) {
    stop("'real' & 'fit' must have same number of element")
  }
  
  if (any(real[!is.na(real)] == 0)) {
    ind <- which(real == 0)
    real <- real[-ind]
    fit <- fit[-ind]
  }
  mean(abs((real - fit))/mean(real), na.rm=na.rm)
}



#' Compute indicators for given model and data
#'
#' @param object \code{model object} (NULL). Can be a R model. So we compute prediction on \code{data} using 
#' \code{fit = predict(object, newdata = data, ...)}. Else, \code{col_fit} must be in \code{data}
#' @param data \code{data.frame} / \code{data.table}. Data on which to measure the quality of the model.
#' @param col_obs \code{character}. Column name of observed values.
#' @param col_fit \code{character} (NULL). Column name of fitted values. 
#' @param by \code{character} (NULL). Column name of aggregation variable.
#' @param dec \code{integer} (2). Number of decimals to be kept.
#' @param label_no_choice \code{character} ("None"). Label for no selection.
#' @param ... in case of \code{model object}, optionnals arguments of \code{predict}
#' 
#' @return a data.frame whose columns are the required indicators and aggregation column.
#' @import data.table
#' 
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#' 
#' library(data.table)
#' 
#' obs <- round(runif(100, 1, 10))
#' fit <- rnorm(100, 1, 5)
#' 
#' compute_model_idc(data = data.table(obs = obs, fit = fit), 
#'     col_obs = "obs", col_fit = "fit", by = "None"
#' )
#' 
#' data_idc <- compute_model_idc(
#'    data = data.table(obs = obs, fit = fit, by_var = sample(rep(1:10, 10))), 
#'    col_obs = "obs", col_fit = "fit", by = "by_var"
#' )
#' plot_idc_table(data_idc = data_idc)
#' 
#' }}
compute_model_idc <- function(object = NULL, 
                        data, 
                        col_obs, 
                        col_fit = NULL,
                        by = NULL, 
                        dec = 2,
                        label_no_choice = "None", 
                        ...) {
  
  # check inputs
  if (! "data.table" %in% class(data)) {
    if ("data.frame" %in% class(data)) {
      data <- data.table::as.data.table(data) 
    } else {
      stop("data must be of class data.frame or data.table.")
    }
  }
  if (! col_obs %in% colnames(data)) {
    stop("Cant find '", col_obs, "' in data.")
  }
  if (! is.null(col_fit) && ! col_fit %in% colnames(data)) {
    stop("Cant find '", col_fit, "' in data.")
  }
  if (! is.null(by) && ! by %in% c(colnames(data), label_no_choice)) {
    stop("Cant find '", by, "' in data.")
  }
  if (is.null(col_fit)) {
    if (is.null(object)) {
      stop("'object' and 'col_fit' cannot be missing at the same time.")
    }
    fit <- try(predict(object, newdata = data, ...), silent = TRUE)
    if (class(fit) == "try-error") {
      stop("An error occurred during prediction.")
    }
  } else {
    fit <- data[[col_fit]]
  }
  
  # compute indicators
  real <- data[[col_obs]]
  
  if (is.null(by) || by == label_no_choice) {
    mape.score <- round(.mape(real, fit), dec)
    rmse.score <- round(.rmse(real, fit), dec)
    mae.score  <- round(.mae(real, fit), dec)
    mape_e.score <- round(.mape_e(real, fit), dec)
    
    res <- data.frame(mape = mape.score, rmse = rmse.score, mae = mae.score, mape_e = mape_e.score)
    
  } else {
    data.split <- data[, list("real" = real, "fit" = fit, get(by))]
    setnames(data.split, "V3", by)
    
    res <- data.frame(data.split[order(get(by))][, list(mape = round(.mape(real, fit), dec), 
                                                        rmse = round(.rmse(real, fit), dec),
                                                        mae = round(.mae(real, fit), dec),
                                                        mape_e = round(.mape_e(real, fit), dec)),
                                                 by = by])
  }
  
  class(res) <- c("idc_table", "data.frame")
  res
}



#' Add temporal (hour, day, ...) using a date column
#'
#' @param data \code{data.table}. Table containing a date colummn.
#' @param col_date \code{character}. Name of the date column.
#' @param tz \code{character}. Timezone of new columns. Default to "UTC"
#' @param temp_vars \code{character} (c("min", "heure", "jour_semaine", "semaine_annee", "mois_annee", "annee")). Variables to be added to the table.
#'
#' @return a data.table with new columns.
#' @export
#'
#' @import data.table
#'
#' @examples
#' \dontrun{\donttest{
#' 
#' data <- data.table(date = seq(Sys.time(), Sys.time() + 24*60*60, length.out = 100))
#' 
#' data <- add_temp_var(data = data,
#'                      col_date = "date",
#'                      tz = "CET",
#'                      temp_vars = c("heure", "semaine_annee", "annee"))
#'                  
#' data_utc <- add_temp_var(data = data,
#'                      col_date = "date",
#'                      tz = "UTC",
#'                      temp_vars = c("heure", "semaine_annee", "annee"))
#'                      
#' }}
add_temp_var <- function(data,
                         col_date,
                         tz = "UTC",
                         temp_vars = c("min", "heure", "jour_semaine", "semaine_annee", "mois_annee", "annee")) {
  data <- copy(data)
  
  if (! col_date %in% names(data)) {
    stop(paste0("Column '", col_date, "' must be present in 'data'."))
  } 
  if (is.null(temp_vars) || ! all(temp_vars %in% c("min", "heure", "jour_semaine", "semaine_annee", "mois_annee", "annee"))) {
    stop("'temp_vars' must be one of : ('min', 'heure', 'jour_semaine', 'semaine_annee', 'mois_annee', 'annee').")
  }
  
  if ("min" %in% temp_vars) {
    data[, "Minute" := format(get(col_date), tz = tz, format = "%M")]
  }
  if ("heure" %in% temp_vars) {
    data[, "Heure" := format(get(col_date), tz = tz, format = "%H")]
  }
  if ("jour_semaine" %in% temp_vars) {
    data[, "Jour" := factor(format(get(col_date), tz = tz, format = "%A"), 
                            levels = format(seq(as.Date("2020-01-06"), as.Date("2020-01-12"), length.out = 7), tz = tz, format = "%A"))]
  }
  if ("semaine_annee" %in% temp_vars) {
    data[, "Semaine" := format(get(col_date), tz = tz, format = "%V")]
  }
  if ("mois_annee" %in% temp_vars) {
    data[, "Mois" := factor(format(get(col_date), tz = tz, format = "%B"), 
                            levels = format(seq(as.Date("2020-01-15"), as.Date("2020-12-15"), length.out = 12), tz = "UTC", format = "%B"))]
  }
  if ("annee" %in% temp_vars) {
    data[, "Annee" := format(get(col_date), tz = tz, format = "%Y")]
  }
  
  return(data)
}



#' Format by column before plotting
#'
#' @param data \code{data.frame} / \code{data.table}. Table containing by or date column.
#' @param by_col \code{character}. Either the name of the column to use for aggregation or 
#' one of ("None", "day", "week", "month", "year").
#' @param col_date \code{character}. Column name for date values.
#' @param nb_quantiles \code{integer} (NULL). Number of intervals for discretization when by_col is numeric. 
#' @param label_no_choice \code{character} ("None"). Label for no selection.
#'
#' @return a data.table. If 'by_col' is one of: ("day", "week", "month", "year"), a new column 
#' is created. If 'by_col' is numeric, it is discretized. Else it is unchanged.
#' 
#' @import data.table stats
#'
#' @examples
#' \dontrun{\donttest{
#' 
#' library(data.table)
#' 
#' obs <- round(runif(100, 1, 10))
#' fit <- rnorm(100, 1, 5)
#' 
#' data <- data.table(obs = obs, fit = fit, by_var = sample(rep(1:10, 10)))
#' data <- shinymodules:::add_by(data = data, by_col = "by_var", nb_quantiles = 15)
#' 
#' }}
add_by <- function(data, 
                   by_col, 
                   col_date,
                   nb_quantiles = NULL,
                   label_no_choice = "None") {
  
  if (! by_col %in% c(colnames(data), c(label_no_choice, "year_day", "year_week", "year_month", "year"))) {
    stop("'by_col' must be a column of 'data' or one of: ('year_day', 'year_week', 'year_month', 'year')")
  }
  if (by_col %in% c("year_day", "year_week", "year_month", "year") && 
      (is.null(col_date) || ! col_date %in% colnames(data))) {
    stop(paste0("'", col_date, "' is not in 'data'."))
  }
  if (! is.null(nb_quantiles) && ! by_col %in% c(label_no_choice, "year_day", "year_week", "year_month", "year") && 
      is.numeric(data[[by_col]]) && ! is.numeric(nb_quantiles)) {
    stop("'nb_quantiles' must be an integer.")
  } 
  
  data <- copy(data)
  
  if (is.numeric(data[[by_col]]) && ! is.null(nb_quantiles)) {
    nb_quantiles <- round(nb_quantiles)
    
    data[, (by_col) := cut(get(by_col), 
                           breaks = unique(
                             stats::quantile(get(by_col), 
                                      probs = seq(0, 1, length.out = as.numeric(nb_quantiles) + 1), 
                                      type = 1)), 
                           dig.lab = 3,
                           include.lowest = T)]
  }
  
  data
}



#' Plot method for idc_table objects
#'
#' @param data_idc \code{data.frame} / \code{data.table}. Table of indicators.
#' @param main \code{character}. Title of the graphic.
#' @param idc \code{character}. Indicators to be plotted.
#' @param col \code{character}. Colors of the curve for each indicator.
#' @param bullet \code{character}. Type of point for each indicators.
#' @param dashLength \code{character}. Dash length for each indicators (0 = solid line).
#' @param js \code{boolean} (TRUE). ??
#' 
#' @return a rAmCharts grpahic: amSerialChart of indicators.
#' @import data.table rAmCharts pipeR
#' 
#' @export
#' 
#' @examples
#' \dontrun{\donttest{
#' 
#' library(data.table)
#' 
#' obs <- round(runif(100, 1, 10))
#' fit <- rnorm(100, 1, 5)
#' 
#' data <- data.table(obs = obs, fit = fit, by_var = sample(rep(1:10, 10)))
#' data <- shinymodules:::add_by(data = data, by_col = "by_var", nb_quantiles = 15)
#' data <- compute_model_idc(data = data, col_obs = "obs", col_fit = "fit", by = "by_var")
#' 
#' plot_idc_table(data_idc = data)
#' 
#' }}
plot_idc_table <- function(data_idc, 
                           main = "Model's indicators", 
                           idc = c("mape", "rmse", "mae", "mape_e"),
                           col = c("black", "darkblue", "purple", "grey"), 
                           bullet = c("round", "square", "triangleUp", "diamond"),
                           dashLength = c(0, 4, 8, 12),
                           js = TRUE) {
  
  # check inputs.
  if (is.data.frame(data_idc)) {
    if ("data.table" %in% class(data_idc)) {
      data_idc <- as.data.frame(data_idc) 
    } 
  } else {
    stop("'data_idc' must be a data.frame or data.table.")
  }
  if (! all(idc %in% c("mape", "rmse", "mae", "mape_e"))) {
    stop("'idc' must be one of: ('mape', 'rmse', 'mae', 'mape_e').")
  }
  if (nrow(data_idc) == 1) {
    data_idc <- data.frame(Global = "", data_idc)
  }
  
  by <- colnames(data_idc)[1]
  
  # create graph if ??
  if (js) {
    if ("mape" %in% idc) {
      data_idc$mape <- round(data_idc$mape * 100, 1)
    }
    if ("mae" %in% idc) {
      data_idc$mae <- round(data_idc$mae, 0)
    }
    if ("rmse" %in% idc) {
      data_idc$rmse <- round(data_idc$rmse, 0)
    }
    if ("mape_e" %in% idc) {
      data_idc$mape_e <- round(data_idc$mape_e * 100, 1)
    }
    
    labelRotation <- ifelse(length(unique(data_idc[[by]])) > 5, 45, 0)
    
    # initialize graph
    plt <- rAmCharts::amSerialChart(dataProvider = data_idc,
                                    categoryField = by, 
                                    creditsPosition = "top-left", 
                                    marginLeft = 15, 
                                    marginRight = 15) %>>% 
      rAmCharts::addTitle(text = main) %>>%
      rAmCharts::setCategoryAxis(title = by, labelRotation = labelRotation) %>>%
      rAmCharts::setExport(position = "top-right") %>>% 
      rAmCharts::setChartCursor() %>>% 
      rAmCharts::setLegend(align = "center")
    
    # add curves and axes.
    for (i in 1:length(idc)) {
      val <- idc[i]
      ind.col <- ifelse(i <= length(col), i, i %% length(col) + 1)
      # add axes
      plt <- rAmCharts::addValueAxes(plt, id = paste0(val, "axis"), 
                                     title = toupper(val), titleBold = TRUE, titleFontSize = 10, titleColor = col[ind.col],
                                     axisThickness = 2, axisColor = col[ind.col], autoGridCount = FALSE, gridCount = 6,
                                     unit = ifelse(val %in% c("mape", "mape_e"), "%", ""),
                                     position = ifelse(i <= 2, "left", "right"), 
                                     offset = ifelse((i %% 2) == 0, 80, 0))
      # add curves
      plt <- rAmCharts::addGraph(plt, title = toupper(val), valueField = val, valueAxis = paste0(val, "axis"), 
                                 type = "line", dashLength = dashLength[i], lineColor = col[ind.col],
                                 bullet = bullet[i], bulletSize = 4, 
                                 balloonText = paste0("[[category]]<br> ",
                                                      toupper(val), " : <b>[[value]]",
                                                      ifelse(val %in% c("mape", "mape_e"), "%", ""), "</b>"))
    }
    plt
    
    # create graph if ??
  } # else {
  #   lcol <- c()
  #   lname <- c()
  #   llty <- c()
  #   
  #   plotype <- ifelse(nrow(data_idc) == 1, "p", "l")
  #   par(mar = c(5, 4, 4, 1 + 2 * length(idc)) + .1)
  #   plot(x = 1:nrow(data_idc), y = data_idc[, idc[1]],
  #        ylab = toupper(idc[1]), xlab = by, main = main,
  #        type = plotype, lty = 2, lwd = 2, col = col[1], xaxt = 'n')
  #   
  #   axis(side = 1, at = 1:nrow(data_idc), labels = data_idc[, by])
  #   
  #   lcol <- c(lcol, col[1])
  #   lname <- c(lname, idc[1])
  #   llty <- c(llty, 2)
  #   if (length(idc) > 1) {
  #     for (i in 2:length(idc)) {
  #       ind.col <- ifelse(i <= length(col), i, i %% length(col) + 1)
  #       par(new = TRUE)
  #       plot(x = 1:nrow(data_idc), y = data_idc[, idc[i]],
  #            type = plotype, xaxt = "n", yaxt = "n", xlab = "", ylab = "",
  #            col = col[ind.col], lty = i+1, lwd = 2)
  #       if (i == 2) {
  #         axis(4, col = col[ind.col])
  #         mtext(toupper(idc[i]), side = 4, line = 2)
  #       } else if (i == 3) {
  #         axis(4, ylim = c(0, max(data_idc[, idc[i]])),
  #              line = 3.5, col = col[ind.col])
  #         mtext(toupper(idc[i]), side = 4, line = 5.5)
  #       } else if (i == 4) {
  #         axis(4, ylim = c(0, max(data_idc[, idc[i]])),
  #              line = 3.5, col = col[ind.col])
  #         mtext(toupper(idc[i]), side = 4, line = 5.5)
  #       }
  #       
  #       lcol <- c(lcol, col[ind.col])
  #       lname <- c(lname, idc[i])
  #       llty <- c(llty, i + 1)
  #     }
  #   }
  #   graphics::legend("topleft", col = lcol, lty = llty, legend = lname)
  #   invisible(NULL)
  # }
}



#' Shiny module to display monitoring indicators on a data.table.
#'
#' @param id \code{character}. shiny id to allow multiple instanciation.
#' @param input shiny input
#' @param output shiny input
#' @param session shiny input
#' @param data \code{data.frame} / \code{data.table}. Table on which indicators will be computed.
#' @param col_id \code{character} (NULL). Column name for id values.
#' @param keep_id \code{character} (NULL). id values to keep in col_id.
#' @param col_obs \code{character}. Column name for observed values.
#' @param col_fit \code{character}. Column name for fitted values.
#' @param col_date \code{character} (NULL). Column name for date values.
#' @param tz \code{character}. Timezone of new columns. Default to "UTC"
#' @param indicators \code{characters}. Indicators to be computed, amongst: ("rmse", "mae", "mape", "mape_star").
#' @param labels \code{character}. Labels to modify displayed texts. See default in examples.
#'
#' @return shiny module.
#' 
#' @import shiny data.table rAmCharts visNetwork rpart stats
#' 
#' @importFrom DT datatable 
#' 
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#' 
#' library(data.table)
#' 
#' data <- data.table(obs = runif(100, 1, 10))
#' data[, fit := obs + rnorm(100, 0, 10)]
#' data[, date := seq(as.POSIXct("2019-10-07 00:00:00 UTC", tz = "UTC"),
#'                    as.POSIXct("2019-10-11 03:00:00 UTC", tz = "UTC"), by = 60*60)]
#' data[, by_quali := factor(sample(rep(1:10, 10)))]
#' data[, by_quanti := runif(100, 1, 20)]
#' 
#' col_obs <- "obs"
#' col_fit <- "fit"
#' col_date = "date"
#' indicators <- c("rmse", "mae", "mape", "mape_e")
#' 
#' ui <- monitoring_data_UI(id = "my_id")
#' 
#' server <- function(input, output, session) {
#'   callModule(module = monitoring_data, id = "my_id", 
#'              data = reactive(data),
#'              col_obs = col_obs,
#'              col_fit = col_fit,
#'              col_date = col_date,
#'              indicators = indicators
#'   )
#' }
#' shiny::shinyApp(ui = ui, server = server)
#' 
#' }}
#' 
#' @rdname monitoring_data_module
#' 
monitoring_data <- function(input, output, session, 
                           data,
                           col_id = NULL,
                           keep_id = NULL,
                           col_obs, 
                           col_fit, 
                           col_date = NULL,
                           tz = "UTC",
                           indicators = c("rmse", "mae", "mape", "mape_e"),
                           labels = list(
                             "progress_data" = "Processing data",
                             "no_data" = "No data",
                             "no_choice" = "None",
                             "idc_title" = "Distribution of indicators",
                             "idc_aggr" = "Aggregation column",
                             "idv_opt_discretiser" = "Discretise ?",
                             "idv_choice_quantiles" = "Quantiles choice",
                             "idc_button" = "Display graphic",
                             "idc_progress" = "Processing indicators",
                             "idc_plot_title" = "Model's indicators",
                             "err_title" = "Distribution of errors", 
                             "err_aggr" = "Aggregation column",
                             "err_type" = "Error type choice",
                             "err_button" = "Display graphic",
                             "err_progress" = "Processing boxplots",
                             "err_ylab" = "Values",
                             "error_xlab" = "error : ",
                             "tree_title" = "Decision tree",
                             "tree_y" = "Y",
                             "tree_x" = "X",
                             "tree_minsplit" = "Minsplit",
                             "tree_cp" = "Complexity (cp)",
                             "tree_maj_cp" = "Update cp",
                             "tree_maj_params" = "Update parameters",
                             "tree_run" = "Update tree",
                             "tree_cp_modal_titre" = "Update cp",
                             "tree_cp_modal_min" = "Slider min",
                             "tree_cp_modal_max" = "Slider max",
                             "tree_cp_modal_step" = "Slider step",
                             "tree_cp_modal_bouton" = "Validate update",
                             "warning_var" = "Select at least one explanatory variable.")
) {
  ns <- session$ns # needed in renderUI
  
  # reactive controls
  if (! shiny::is.reactive(col_id)) {
    get_col_id <- shiny::reactive(col_id)
  } else {
    get_col_id <- col_id
  }
  if (! shiny::is.reactive(keep_id)) {
    get_keep_id <- shiny::reactive(keep_id)
  } else {
    get_keep_id <- keep_id
  }
  if (! shiny::is.reactive(col_obs)) {
    get_col_obs <- shiny::reactive(col_obs)
  } else {
    get_col_obs <- col_obs
  }
  if (! shiny::is.reactive(col_fit)) {
    get_col_fit <- shiny::reactive(col_fit)
  } else {
    get_col_fit <- col_fit
  }
  if (! shiny::is.reactive(col_date)) {
    get_col_date <- shiny::reactive(col_date)
  } else {
    get_col_date <- col_date
  }
  if (! shiny::is.reactive(tz)) {
    get_tz <- shiny::reactive(tz)
  } else {
    get_tz <- tz
  }
  if (! shiny::is.reactive(indicators)) {
    get_indicators <- shiny::reactive(indicators)
  } else {
    get_indicators <- indicators
  }
  if (! shiny::is.reactive(labels)) {
    get_labels <- shiny::reactive(labels)
  } else {
    get_labels <- labels
  }
  
  # labels 
  output$no_data <- renderText({get_labels()$no_data})
  output$idc_title <- renderText({get_labels()$idc_title})
  output$err_title <- renderText({get_labels()$err_title})
  output$tree_title <- renderText({get_labels()$tree_title})
  output$init_idc_button <- renderUI({
    actionButton(ns("idc_go"), get_labels()$idc_button, width = "100%")
  })
  output$init_err_type <- renderUI({
    selectInput(ns("boxplot_error"), get_labels()$err_type,
                c("relative", "absolute", "quadratic"), 
                selected = "relative")
  })
  output$init_err_button <- renderUI({
    actionButton(ns("boxplot_go"), get_labels()$err_button, width = "100%")
  })
  # output$init_tree_y <- renderUI({
  #   selectInput(ns("tree_y_var"), get_labels()$tree_y, choices = NULL, selected = NULL)
  # })
  output$init_tree_minsplit <- renderUI({
    numericInput(ns("tree_minsplit"), label = get_labels()$tree_minsplit, min = 2, max = Inf, value = 20)
  })
  output$init_tree_cp <- renderUI({
    sliderInput(ns("tree_cp"), label = get_labels()$tree_cp, min = 0, max = 1, value = 0.005, step = 0.005)
  })
  output$init_tree_maj_cp <- renderUI({
    actionButton(ns("tree_set_cp"), label = get_labels()$tree_maj_cp)
  })
  output$init_tree_maj_params <- renderUI({
    actionButton(ns("tree_go"), label = get_labels()$tree_maj_params, width = "20%")
  })
  
  # check data
  output$is_data <- reactive({
    ! is.null(data())
  })
  outputOptions(output, "is_data", suspendWhenHidden = FALSE)
  
  # create data
  get_data <- reactive({
    data <- data()
    
    isolate({
      if (! is.null(data)) {
        withProgress(message = get_labels()$progress_data, value = 0.9, {
          
          # filter ouvrages
          if (! (is.null(get_keep_id()) || get_keep_id() == "Tous") && get_col_id() %in% names(data)) {
            data <- data[get(get_col_id()) %in% get_keep_id(), ]
          }
          
          # add cols min, hour, week, month, year + keep only target vars indicators
          if (! is.null(get_col_date())) {
            data <- add_temp_var(data = data, tz = get_tz(), 
                                 col_date = get_col_date())
          }
        })
        data
        
      } else {
        NULL
      }
    })
  })
  
  # display / hide quantile slider depending on whether aggr col is numeric
  output$idc_quantiles <- renderUI({
    idc_by <- input$idc_by
    discretize <- input$idc_use_quantiles
    
    isolate({
      if (! is.null(get_data()) && is.numeric(get_data()[[idc_by]]) && (is.null(discretize) || discretize == T)) {
        tagList(
          column(1,
                 div(style = "margin-top: 30px;",
                     checkboxInput(
                       ns("idc_use_quantiles"),
                       label = get_labels()$idv_opt_discretiser,
                       value = T))
          ),
          column(2, 
                 div(sliderInput(ns("idc_nb_quantiles"), get_labels()$idv_choice_quantiles, min = 2, max = 100, value = 5), align = "left")
          )
        )
      } else if (! is.null(get_data()) && is.numeric(get_data()[[idc_by]]) && ! is.null(discretize) && discretize == F) {
        column(1,
               div(style = "margin-top: 30px;",
                   checkboxInput(
                     ns("idc_use_quantiles"),
                     label = get_labels()$idv_opt_discretiser,
                     value = F))
        )
      } else {
        NULL
      }
    })
  })
  output$boxplot_quantiles <- renderUI({
    boxplot_by <- input$boxplot_by
    discretize <- input$boxplot_use_quantiles
    
    isolate({
      if (! is.null(get_data()) && is.numeric(get_data()[[boxplot_by]]) && (is.null(discretize) || discretize == T)) {
        tagList(
          column(1,
                 div(style = "margin-top: 30px;",
                     checkboxInput(
                       ns("boxplot_use_quantiles"),
                       label = get_labels()$idv_opt_discretiser,
                       value = T))
          ),
          column(2, 
                 div(sliderInput(ns("boxplot_nb_quantiles"), get_labels()$idv_choice_quantiles, min = 2, max = 100, value = 5), align = "left")
          )
        )
      } else if (! is.null(get_data()) && is.numeric(get_data()[[boxplot_by]]) && discretize == F) {
        column(1,
               div(style = "margin-top: 30px;",
                   checkboxInput(
                     ns("boxplot_use_quantiles"),
                     label = get_labels()$idv_opt_discretiser,
                     value = F))
        )
      } else {
        NULL
      }
    })
  })
  
  # display indicator plot
  output$plot_idc <- renderAmCharts({
    cpt <- input$idc_go
    
    isolate({
      data <- copy(get_data())
      
      if (! is.null(cpt) && cpt > 0 && ! is.null(data) ) {
        withProgress(message = get_labels()$idc_progress, value = 0.5,{
          by <- input$idc_by
          
          nb_quantiles = if (! is.null(input$idc_use_quantiles) && input$idc_use_quantiles) {input$idc_nb_quantiles} else {NULL}
          data_idc <- add_by(data = data, 
                             by_col = by, 
                             col_date = get_col_date(),
                             nb_quantiles = nb_quantiles,
                             label_no_choice = get_labels()$no_choice)
          
          data_idc <- compute_model_idc(data = data_idc, 
                                  col_obs = get_col_obs(), 
                                  col_fit = get_col_fit(),
                                  by = by,
                                  label_no_choice = get_labels()$no_choice)
          
          
          plot_idc_table(data_idc, idc = get_indicators(), main = get_labels()$idc_plot_title)
        })
      }
    })
  })
  # display indicators table
  output$table_idc <- DT::renderDT({
    cpt <- input$idc_go
    
    isolate({
      data <- copy(get_data())
      
      if (! is.null(cpt) && cpt > 0 && ! is.null(data)) {
        by <- input$idc_by
        
        # add aggregation column
        nb_quantiles <- if (! is.null(input$idc_use_quantiles) && input$idc_use_quantiles) {input$idc_nb_quantiles} else {NULL}
        data_idc <- add_by(data = data, 
                           by_col = by, 
                           col_date = get_col_date(),
                           nb_quantiles = nb_quantiles,
                           label_no_choice = get_labels()$no_choice)
        
        # compute indicators
        data_idc <- compute_model_idc(data = data_idc, 
                                col_obs = get_col_obs(), 
                                col_fit = get_col_fit(),
                                by = by,
                                dec = 2,
                                label_no_choice = get_labels()$no_choice)
        
        DT::datatable(data_idc, caption = "data_idc", rownames = NULL)
      }
    })
  })
  
  # update aggregation column choices
  observe({
    data <- get_data()
    
    if (! is.null(data) && input$boxplot_by == "") {
      updateSelectInput(session = session, "boxplot_by", label = get_labels()$err_aggr, 
                        choices = c(get_labels()$no_choice, setdiff(names(get_data()), 
                                                     c("date", get_col_obs(), get_col_fit()))))
    }
    if (! is.null(data) && input$idc_by == "") {
      updateSelectInput(session = session, "idc_by", get_labels()$idc_aggr,
                        choices = c(get_labels()$no_choice, setdiff(names(get_data()), 
                                                     c("date", get_col_obs(), get_col_fit()))))
    }
  })
  
  # display errors boxplot
  output$boxplot_plot <- rAmCharts::renderAmCharts({
    cpt <- input$boxplot_go
    
    isolate({
      data <- copy(get_data())
      
      if (! is.null(cpt) && cpt > 0 && ! is.null(data)) {
        
        withProgress(message = get_labels()$err_progress, value = 0.5, {
          boxplot_error <- input$boxplot_error
          by <- input$boxplot_by
          
          # compute prediction errors
          if (boxplot_error == "relative") {
            data[, "error" := get(get_col_obs()) - get(get_col_fit())]
          } else if (boxplot_error == "absolute") {
            data[, "error" := abs(get(get_col_obs()) - get(get_col_fit()))]
          } else if (boxplot_error == "quadratic") {
            data[, "error" := c(get(get_col_obs()) - get(get_col_fit()))**2]
          } 
          
          if (by == get_labels()$no_choice) {
            plt <- rAmCharts::amBoxplot(data[["error"]],
                                        names = "", xlab = paste0(get_labels()$error_xlab, boxplot_error),
                                        ylab = get_labels()$err_ylab)
          } else {
            # add aggregation column
            nb_quantiles <- if (! is.null(input$boxplot_use_quantiles) && input$boxplot_use_quantiles) {input$boxplot_nb_quantiles} else {NULL}
            data <- add_by(data = data, 
                           by_col = by, 
                           col_date = get_col_date(),
                           nb_quantiles = nb_quantiles,
                           label_no_choice = get_labels()$no_choice)
            
            # problem when using name 'id'
            if (by == "id") {
              by = "id2"
              data[, "id2" := get_keep_id()]
            }
            
            plt <- eval(parse(text = paste0("amBoxplot(error ~ '", by, "', data = data,
                                            names = '', xlab = paste0(toupper(substr(boxplot_error, 1, 1)), 
                                            substr(boxplot_error, 2, nchar(boxplot_error))),
                                            ylab = 'Values')")))
            plt <- plt %>>% setCategoryAxis(labelRotation = ifelse(length(unique(data[[by]])) > 5, 45, 0))
          }
        })
        plt %>%
          amOptions(export = T, zoom = T)
        
        } else {
          NULL
        }
        })
      })
  
  # update x/y vars
  observe({
    data <- get_data()
    
    isolate({
      if (! is.null(data)) {
        y_var <- input$tree_y_var
        if (is.null(y_var)) {
          allowed_y_vars <- c(get_col_fit(), get_col_obs())
          
          updateSelectInput(session = session, "tree_x_var", 
                            label = get_labels()$tree_x,
                            choices = setdiff(names(data), c(allowed_y_vars, "date")), 
                            selected = setdiff(names(data), c(allowed_y_vars, "date")))
          updateSelectInput(session = session, "tree_y_var", 
                            label = get_labels()$tree_y,
                            choices = allowed_y_vars, 
                            selected = allowed_y_vars[1])
        } else {
          allowed_y_vars <- c(get_col_fit(), get_col_obs())
          
          updateSelectInput(session = session, "tree_y_var",
                            label = get_labels()$tree_y,
                            choices = allowed_y_vars, 
                            selected = ifelse(input$tree_y_var %in% allowed_y_vars, input$tree_y_var, allowed_y_vars[1]))
          updateSelectInput(session = session, "tree_x_var", 
                            label = get_labels()$tree_x,
                            choices = setdiff(names(data), c(allowed_y_vars, "date")), 
                            selected = setdiff(names(data), c(allowed_y_vars, "date"))) 
        } 
      }
    })
  })
  observe({
    y_var <- input$tree_y_var
    
    isolate({
      if (! is.null(get_data())) {
        choices = setdiff(names(data), c(y_var, "date"))
        if(is.null(input$tree_x_var)){
          selected <- choices
        } else {
          selected <- intersect(input$tree_x_var, choices)
        }
        updateSelectInput(session = session, "tree_x_var", 
                          label = get_labels()$tree_x,
                          choices = choices, 
                          selected = selected) 
      }
    })
  })
  
  # update cp slider
  cp_parameters <- reactiveValues(min = 0, max = 1, step = 0.005)
  observe({
    cpt <- input$tree_set_cp
    
    isolate({
      if (! is.null(cpt) && cpt > 0) {
        showModal(modalDialog(
          title = get_labels()$tree_cp_modal_titre,
          numericInput("tree_cp_min", get_labels()$tree_cp_modal_min, cp_parameters$min),
          numericInput("tree_cp_max", get_labels()$tree_cp_modal_max, cp_parameters$max),
          numericInput("tree_cp_step", get_labels()$tree_cp_modal_step, cp_parameters$step),
          style = "margin-top: 25px;", actionButton("tree_update_cp", get_labels()$tree_cp_modal_bouton),
          easyClose = TRUE, footer = NULL))
      }
    })
  })
  observeEvent(input$tree_update_cp, {
    isolate({
      cp_parameters$min <- input$tree_cp_min
      cp_parameters$max <- input$tree_cp_max
      cp_parameters$step <- input$tree_cp_step
      updateSliderInput(session, "tree_cp", min = cp_parameters$min, max = cp_parameters$max, step = cp_parameters$step)
    })
  })
  
  # display error tree
  observe({
    cpt <- input$tree_go
    
    isolate({
      if (! is.null(cpt) && cpt > 0) {
        if (! is.null(input$tree_x_var) && !is.null(input$tree_y_var)) {
          withProgress(message = get_labels()$tree_title, value = 0.5, {
            data <- copy(get_data())
            
            formule <- paste(input$tree_y_var, "~", paste0(input$tree_x_var, collapse = "+")) %>% 
              stats::as.formula()
            rpart_tree <- rpart::rpart(formule, 
                                       data = data, 
                                       control = rpart::rpart.control(cp = input$tree_cp, 
                                                                      minsplit = input$tree_minsplit))
            
            is_const <- names(which(sapply(data, function(x) length(unique(x)) == 1)))
          })
          
          callModule(visNetwork::visTreeModuleServer, "vis_tree",
                     data = reactive(rpart_tree),
                     tooltip_data = data[, setdiff(c(input$tree_y_var, input$tree_x_var), is_const), with = F],
                     height = 800) 
          
        } else {
          showModal(modalDialog(
            easyClose = TRUE,
            footer = NULL,
            get_labels()$warning_var
          ))
          
          NULL 
        }
      } else {
        NULL
      }
    })
  })
  }



#' @import shiny rAmCharts visNetwork
#' @importFrom DT DTOutput renderDT
#' 
#' @export
#'
#' @rdname monitoring_data_module
#' 
monitoring_data_UI <- function(id) {
  ns <- shiny::NS(id)
  
  fluidPage(
    conditionalPanel(condition = paste0("output['", ns("is_data"), "']"),
                     
                     # indicators
                     fluidRow(
                       # title indicatorss
                       div(h3(textOutput(ns("idc_title"))), align = "center", style = "color: #3c8dbc"),
                       # parameters
                       column(2, 
                              selectInput(ns("idc_by"), "Colonne d'agregation", choices = "")
                       ),
                       uiOutput(ns("idc_quantiles")),
                       column(2, 
                              div(style = "margin-top: 25px;",
                                  uiOutput(ns("init_idc_button")), align = "center")
                       ) 
                     ),
                     conditionalPanel(condition = paste0("output['", ns("is_data"), "'] && input['", ns('idc_go'), "'] > 0"),
                                      tabsetPanel(
                                        tabPanel("plot",
                                                 rAmCharts::amChartsOutput(ns("plot_idc"), type = "serial", height = "550px")
                                        ),
                                        tabPanel("table",
                                                 fluidRow(
                                                   column(12,
                                                          br(),
                                                          DT::DTOutput(ns("table_idc"))
                                                   )
                                                 )
                                        )
                                      )
                     ),
                     conditionalPanel(condition = paste0("! output['", ns('is_data'), "'] || input['", ns('idc_go'), "'] == 0"), 
                                      fluidRow(
                                        # div(h4(textOutput(ns("no_data")), style = "color: darkblue;"), align = "center")
                                      )
                     ),
                     
                     # boxplots
                     div(fluidRow(
                       # title boxplots
                       div(h3(textOutput(ns("err_title"))), align = "center", style = "color: #3c8dbc"),
                       # parameters
                       column(2, 
                              selectInput(ns("boxplot_by"), "Agregation", choices = "")
                       ),
                       uiOutput(ns("boxplot_quantiles")),
                       column(2, 
                              uiOutput(ns("init_err_type"))
                       ),
                       column(2, 
                              div(style = "margin-top: 25px;",
                                  uiOutput(ns("init_err_button")), align = "center")
                       ) 
                     ), 
                     # result
                     conditionalPanel(condition = paste0("output['", ns('is_data'), "'] && input['", ns('boxplot_go'), "'] > 0"),
                                      rAmCharts::amChartsOutput(ns("boxplot_plot"), height = "400px")
                     ),
                     conditionalPanel(condition = paste0("! output['", ns('is_data'), "'] || input['", ns('boxplot_go'), "'] == 0"), 
                                      fluidRow(
                                        # div(h4(textOutput(ns("no_data")), style = "color: darkblue;"), align = "center")
                                      )
                     ), style = "border-top: 5px solid #3c8dbc; margin-top: 15px; padding-top: 15px"),
                     
                     # visNetwork
                     div(fluidRow(
                       # title visNetwork
                       div(h3(textOutput(ns("tree_title"))), align = "center", style = "color: #3c8dbc"), 
                       # parameters
                       column(2,
                              selectInput(ns("tree_y_var"), "Y : ", choices = NULL, selected = NULL)
                       ),
                       column(5,
                              selectInput(ns("tree_x_var"), "X :", choices = NULL,  multiple = TRUE, selected = NULL, width = "100%")
                       ),
                       column(2,
                              uiOutput(ns("init_tree_minsplit"))
                       ),
                       column(2,
                              uiOutput(ns("init_tree_cp"))
                       ),
                       column(1,
                              div(uiOutput(ns("init_tree_maj_cp")), style = "margin-top: 25px;")
                       ),
                       column(12,
                              div(uiOutput(ns("init_tree_maj_params")), align = "center")
                       )
                     ), style = "border-top: 5px solid #3c8dbc; margin-top: 15px; padding-top: 15px"),
                     # result
                     conditionalPanel(condition = paste0("output['", ns('is_data'), "'] > 0 && input['", ns('tree_go'), "'] > 0"),
                                      visNetwork::visTreeModuleUI(ns("vis_tree"), rpartParams = F)
                     ),
                     conditionalPanel(condition = paste0("output['", ns('is_data'), "'] == 0 || input['", ns('tree_go'), "'] == 0"),
                                      fluidRow(
                                        div(h4(textOutput(ns("no_data")), style = "color: darkblue;"), align = "center")
                                      )
                     )
    )
  )
}
