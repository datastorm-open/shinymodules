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
#' @param object \code{modelInstant} (NULL). A modelInstant object. IE = ???
#' @param data \code{data.frame} / \code{data.table}. Data on which to measure the quality of the model.
#' @param col_obs \code{character}. Column name of observed values.
#' @param col_fit \code{character} (NULL). Column name of fitted values. 
#' @param by \code{character} (NULL). Column name of aggregation variable.
#' @param nb.cores \code{integer} (1). Number of cores, when oject is used to make prediction.
#'
#' @return a data.frame whose columns are the required indicators and aggregation column.
#' @import data.table
#'
#' @examples
#' \dontrun{\donttest{
#' 
#' library(data.table)
#' 
#' obs <- round(runif(100, 1, 10))
#' fit <- rnorm(100, 1, 5)
#' 
#' .compute_idc(data = data.table(obs = obs, fit = fit), 
#' col_obs = "obs", col_fit = "fit", by = "none")
#' .compute_idc(data = data.table(obs = obs, fit = fit, by_var = sample(rep(1:10, 10))), 
#' col_obs = "obs", col_fit = "fit", by = "by_var")
#' 
#' }}
.compute_idc <- function(object = NULL, 
                         data, 
                         col_obs, 
                         col_fit = NULL,
                         by = NULL, 
                         nb.cores = 1) {
  
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
  if (! is.null(by) && ! by %in% c(colnames(data), "none")) {
    stop("Cant find '", by, "' in data.")
  }
  if (is.null(col_fit)) {
    if (is.null(object)) {
      stop("'object' and 'col_fit' cannot be missing at the same time.")
    }
    fit <- try(predict(object, newdata = data, nb.cores = nb.cores),
               silent = TRUE)
    if (class(fit) == "try-error") {
      stop("An error occurred during prediction.")
    }
  } else {
    fit <- data[[col_fit]]
  }
  
  # compute indicators
  real <- data[[col_obs]]
  
  if (is.null(by) || by == "none") {
    mape.score <- .mape(real, fit)
    rmse.score <- .rmse(real, fit)
    mae.score  <- .mae(real, fit)
    mape_e.score <- .mape_e(real, fit)
    
    res <- data.frame(mape = mape.score, rmse = rmse.score, mae = mae.score, mape_e = mape_e.score)
    
  } else {
    data.split <- data[, list("real" = real, "fit" = fit, get(by))]
    setnames(data.split, "V3", by)
    
    res <- data.frame(data.split[order(get(by))][, list(mape = .mape(real, fit), rmse = .rmse(real, fit),
                                     mae = .mae(real, fit), mape_e = .mape_e(real, fit)), by = by])
  }
  
  class(res) <- c("idc_table", "data.frame")
  res
}


#' Format by column before plotting
#'
#' @param data \code{data.frame} / \code{data.table}. Table containing by or date column.
#' @param by_col \code{character}. Either the name of the column to use for aggregation or one of ("none", "day", "week", "month", "year").
#' @param col_date \code{character} (NULL). Column name for date values.
#' @param nb_quantiles \code{integer} (NULL). Number of intervals for discretization when by_col is numeric. 
#'
#' @return a data.table. If 'by_col' is one of: ("day", "week", "month", "year"), a new column 
#' is created. If 'by_col' is numeric, it is discretized. Else it is unchanged.
#' @import data.table
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
#' data <- shinymodules:::.add_by(data = data, by_col = "by_var", nb_quantiles = 15)
#' 
#' }}
.add_by <- function(data, 
                    by_col, 
                    col_date,
                    nb_quantiles = NULL) {
  
  
  if (! by_col %in% c(colnames(data), c("none", "day", "week", "month", "year"))) {
    stop("'by_col' must be a column of 'data' or one of: ('day', 'week', 'month', 'year')")
  }
  if (by_col %in% c("day", "week", "month", "year") && (is.null(col_date) || ! col_date %in% colnames(data))) {
    stop(paste0("'", col_date, "' is not in 'data'."))
  }
  if (! data[[by_col]] %in% c("none", "day", "week", "month", "year") && is.numeric(data[[by_col]]) && ! is.numeric(nb_quantiles)) {
    stop("'nb_quantiles' must be an integer.")
  } 
  
  data <- copy(data)
  
  if (by_col == "year") {
    data[, "year" := year(get(col_date))]
  } else if (by_col == "month") {
    data[, "month" := paste(year(get(col_date)), month(get(col_date)), sep = "/")]
  } else if (by_col == "week") {
    data[, "week" := paste(year(get(col_date)), week(get(col_date)), sep = " week ")]
  } else if (by_col == "day") {
    data[, "day" := substr(get(col_date), 1, 10)]
    
  } else if (is.numeric(data[[by_col]])) {
    nb_quantiles <- round(nb_quantiles)
    
    data[, (by_col) := cut(get(by_col), 
                           breaks = unique(
                             quantile(get(by_col), 
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
#' @param instantToHour \code{boolean} (FALSE). ??
#' @param main \code{character}. Title of the graphic.
#' @param idc \code{character}. Indicators to be plotted.
#' @param col \code{character}. Colors of the curve for each indicator.
#' @param bullet \code{character}. Type of point for each indicators.
#' @param dashLength \code{character}. Dash length for each indicators (0 = solid line).
#' @param js \code{boolean} (TRUE). ??
#' 
#' @return a rAmCharts grpahic: amSerialChart of indicators.
#' @import data.table rAmCharts
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
#' data <- shinymodules:::.add_by(data = data, by_col = "by_var", nb_quantiles = 15)
#' data <- shinymodules:::.compute_idc(data = data, col_obs = "obs", col_fit = "fit", by = "by_var")
#' 
#' shinymodules:::plot.idc_table(data_idc = data)
#' 
#' }}
plot.idc_table <- function(data_idc, 
                                    instantToHour = FALSE,
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
    
    # if (instantToHour) {
    #   data_idc[, by] <- substring(
    #              seq.POSIXt(as.POSIXct("2010-01-01 00:00:00", tz = "UTC"),
    #                  as.POSIXct("2010-01-02 00:00:00", tz = "UTC"),
    #                  length.out = nrow(data_idc)), 12, 19)
    # }
    
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


#' Shiny module server-like fun to display indicators on a data.table.
#'
#' @param input shiny input
#' @param output shiny input
#' @param session shiny input
#' @param data \code{data.frame} / \code{data.table}. Table on which indicators will be computed.
#' @param col_obs \code{character}. Column name for observed values.
#' @param col_fit \code{character}. Column name for fitted values.
#' @param col_date \code{character} (NULL). Column name for date values.
#' @param indicators \code{characters}. Indicators to be computed, amongst: ("rmse", "mae", "mape", "mape_star").
#'
#' @return shiny module.
#' 
#' @import shiny data.table rAmCharts
#' 
#' @importFrom DT datatable renderDT DTOutput
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
#'     as.POSIXct("2019-10-11 03:00:00 UTC", tz = "UTC"), by = 60*60)]
#' data[, by_quali := factor(sample(rep(1:10, 10)))]
#' data[, by_quanti := runif(100, 1, 20)]
#' 
#' col_obs <- "obs"
#' col_fit <- "fit"
#' indicators <- c("rmse", "mae", "mape", "mape_e")
#' 
#' ui <- shiny::fluidPage(vis_indicators_UI("my_id", data, col_obs, col_fit))
#' server <- function(input, output, session) {
#'   callModule(vis_indicators, "my_id", data, col_obs, col_fit, indicators)
#' }
#' shiny::shinyApp(ui = ui, server = server)
#' 
#' }}
vis_indicators <- function(input, output, session, 
                           data, 
                           col_obs, col_fit, col_date = NULL,
                           indicators) {
  ns <- session$ns # needed in renderUI
  
  # display / hide quantile slider depending on whether aggr col is numeric
  output$update_by_idc <- renderUI({
    by <- input$by_idc
    
    isolate({
      if (! is.null(data) && is.numeric(data[[by]])) {
        tagList(
          column(3, 
                 sliderInput(ns("nb_quantiles_idc"), "Quantile number (quantitative vars)", min = 2, max = 100, value = 5)
          )
        )
      } else {
        NULL
      }
    })
  })
  output$update_by_boxplot <- renderUI({
    by <- input$by_boxplot
    
    isolate({
      if (! is.null(data) && is.numeric(data[[by]])) {
        tagList(
          column(3, 
                 sliderInput(ns("nb_quantiles_boxplot"), "Quantile number (quantitative vars)", min = 2, max = 100, value = 5)
          )
        )
      } else {
        NULL
      }
    })
  })
  
  # compute indicators
  get_data_idc <- reactive({
    if(input$go_idc > 0){
      isolate({
        data_idc <- data.table(data)
        by <- input$by_idc
        
        data_idc <- .add_by(data = data_idc, 
                            by_col = by, 
                            col_date = col_date,
                            nb_quantiles = input$nb_quantiles_idc)

        .compute_idc(data = data_idc, 
                     col_obs = col_obs, 
                     col_fit = col_fit,
                     by = by)
      })
    } else{
      NULL
    }
  })
  
  # display indicators plot
  output$is_idc <- reactive({
    ! is.null(get_data_idc())
  })
  outputOptions(output, "is_idc", suspendWhenHidden = FALSE)
  output$plot_idc <- renderAmCharts({
    data_idc <- get_data_idc()
    isolate({
      if(! is.null(data_idc)){
        withProgress(message = 'Graphic...', value = 0.5,{
          plot(data_idc, instantToHour = FALSE, idc = indicators)
        })
      }
    })
  })
  # display indicators table
  output$table_idc <- DT::renderDT({
    data_idc <- get_data_idc()
    if(! is.null(data_idc)){
      data_idc$mape <- round(data_idc$mape, 2)
      data_idc$rmse <- round(data_idc$rmse, 2)
      data_idc$mae <- round(data_idc$mae, 2)
      data_idc$mape_e <- round(data_idc$mape_e, 2)
      
      DT::datatable(data_idc, caption = "data_idc", rownames = NULL)
    }
  })
  
  # display errors boxplot
  output$is_data <- reactive({
    ! is.null(data)
  })
  outputOptions(output, "is_data", suspendWhenHidden = FALSE)
  output$plot_boxplot <- renderAmCharts({
    input$go_boxplot
    
    isolate({
      data <- copy(data)
      
      if (input$go_boxplot > 0 && ! is.null(data)) {
        
        type_boxplot <- input$type_boxplot
        
        if (type_boxplot == "relative") {
          data[, "error" := get(col_obs) - get(col_fit)]
        } else if (type_boxplot == "absolute") {
          data[, "error" := abs(get(col_obs) - get(col_fit))]
        } else if (type_boxplot == "quadratic") {
          data[, "error" := c(get(col_obs) - get(col_fit))**2]
        } 

        by <- input$by_boxplot
        if (by == "none") {
          plt <- amBoxplot(data[["error"]], export = TRUE)
          plt
        } else {
          data <- .add_by(data = data, 
                          by_col = input$by_boxplot, 
                          col_date = col_date,
                          nb_quantiles = input$nb_quantiles_boxplot)

          labelRotation <- ifelse(length(unique(data[[by]])) > 5, 45, 0)
          
          plt <- eval(parse(text = paste0("amBoxplot(error ~ ", by, ", data = data)")))
          plt %>>% setCategoryAxis(labelRotation = labelRotation)
        }
        
      } else {
        NULL
      }
    })
  })
}

#' Shiny module ui-like fun to display indicators on a data.table.
#'
#' @param id \code{character}. shiny id to allow multiple instanciation.
#' @param data \code{data.frame} / \code{data.table}. Table on which indicators will be computed.
#' @param col_obs \code{character}. Column name for observed values.
#' @param col_fit \code{character}. Column name for fitted values.
#' @param col_date \code{character} (NULL). Column name for date values.
#'
#' @return shiny module.
#' @import shiny rAmCharts
#' @importFrom DT DTOutput renderDT
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
#'     as.POSIXct("2019-10-11 03:00:00 UTC", tz = "UTC"), by = 60*60)]
#' data[, by_quali := factor(sample(rep(1:10, 10)))]
#' data[, by_quanti := runif(100, 1, 20)]
#' 
#' col_obs <- "obs"
#' col_fit <- "fit"
#' indicators <- c("rmse", "mae", "mape", "mape_e")
#' 
#' ui <- shiny::fluidPage(vis_indicators_UI("my_id", data, col_obs, col_fit))
#' server <- function(input, output, session) {
#'   callModule(vis_indicators, "my_id", data, col_obs, col_fit, indicators)
#' }
#' shiny::shinyApp(ui = ui, server = server)
#' 
#' }}
vis_indicators_UI <- function(id, 
                              data,
                              col_obs,
                              col_fit,
                              col_date = NULL) {
  ns <- shiny::NS(id)
  
  fluidPage(
    fluidRow(
      column(3, 
             selectInput(ns("by_idc"), "Aggregation column", c("none", setdiff(names(data), c(col_obs, col_fit, col_date)), "year", "month", "week", "day"))
      ),
      uiOutput(ns("update_by_idc")),
      column(3, 
             div(br(), actionButton(ns("go_idc"), "Display indicators"))
      ) 
    ),
    conditionalPanel(condition = paste0("output['", ns("is_idc"), "'] > 0"),
                     tabsetPanel(
                       tabPanel("plot",
                                amChartsOutput(ns("plot_idc"), type = "serial", height = "550px")
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
    conditionalPanel(condition = paste0("output['", ns("is_idc"), "'] == 0"),
                     "No data available."
    ),
    div(fluidRow(
      column(3, 
             selectInput(ns("by_boxplot"), "Aggregation column", c("none", setdiff(names(data), c(col_obs, col_fit, col_date)), "year", "month", "week", "day"))
      ),
      uiOutput(ns("update_by_boxplot")),
      column(3, 
             selectInput(ns("type_boxplot"), "Type of boxplot", c("relative", "absolute", "quadratic"), selected = 5)
      ),
      column(3, 
             div(br(), actionButton(ns("go_boxplot"), "Display errors distribution"))
      ) 
    ), style = "border-top: 0.5px solid blue; margin-top: 15px; padding-top: 15px"), 
    conditionalPanel(condition = paste0("output['", ns("is_data"), "'] > 0"),
                     amChartsOutput(ns("plot_boxplot"), height = "400px")
    ),
    conditionalPanel(condition = paste0("output['", ns("is_data"), "'] == 0"), 
                     "No data available."
    )
    
  )
}