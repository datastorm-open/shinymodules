#' @title plot histogram and scatterplot
#' @description This function takes a data.table with one or two numeric
#' variables in input and return a graph of the type chosen by the user,
#' it can do interactive or fix graph.
#' 
#' @param data \code{data.table} data.table with one or two numeric columns
#' @param type \code{character} type of the graph, default is line, possible 
#' values are "line", "point" and "hist"
#' @param js \code{logical} if TRUE, the graph is interactive (default TRUE)
#' @param id \code{character} if you use an id column, default NULL
#'
#' @import ggplot2 rAmCharts
#' @export
#' 
#' @examples 
#' \dontrun{
#' mtcars <- data.table::copy(data.table(datasets::mtcars))
#' 
#' plotScatterplot(mtcars[, list(mpg, cyl)], type = "line", js = T)
#' plotScatterplot(mtcars[, list(mpg, drat)], type = "point", js = T)
#' plotScatterplot(mtcars[, list(drat)], type = "hist", js = T)
#' plotScatterplot(mtcars[, list(drat, wt)], type = "point", js = F)
#' }
#'
plotScatterplot <- function(data, type = "line", js = TRUE, id = NULL,
                            palette_ggplot){
  
  tmpdata <- data.table::data.table(data)
  ctrl <- sapply(colnames(tmpdata), function(quanti.var){
    tmpdata[, c(quanti.var) := round(get(quanti.var), 2)]
  })
  if (ncol(tmpdata) == 1) {
    if (type %in% c("line", "point")) {
      
      if (js) {
        
        if(is.null(id)){
          tmpdata[, id := 1:.N]
        }else{
          if (!(id %in% colnames(tmpdata))) {
            stop("Can't find '", id, "' column in data")
          }
          data.table::setnames(tmpdata, id, "id")
        }
        lineAlpha <- ifelse (type == "line", 1, 0)
        bullet <- NULL
        if (type != "line") {
          bullet <- "circle"
        }
        graph <- rAmCharts::amSerialChart(
          theme = "light", creditsPosition = "top-left", dataProvider = tmpdata) %>>% 
          rAmCharts::addTitle(text = colnames(tmpdata)[1]) %>>%
          rAmCharts::setCategoryAxis(labelsEnabled = FALSE) %>>%
          rAmCharts::setChartCursor(categoryBalloonEnabled = FALSE) %>>%
          rAmCharts::addGraph(balloonText = paste0(
            "Observation num. <b>[[id]]</b><br>", 
            colnames(data)[1], " : <b>[[value]]</b>"), 
            bullet = bullet,
            valueField = colnames(data)[1],lineAlpha = lineAlpha) %>>%
          rAmCharts::addValueAxes(title = colnames(data)[1]) %>>% 
          rAmCharts::setExport(position = "top-right") %>>% 
          rAmCharts::plot()
        
      } else {
        tmpdata$observation <- 1:nrow(tmpdata)
        if(type != "line") {
          graph <- ggplot2::ggplot(tmpdata, environment = environment()) + 
            # ggplot2::geom_point(ggplot2::aes(x =  observation, 
            #                                  y = get(colnames(tmpdata)[1]))) +
            geom_hex(ggplot2::aes(x =  observation, 
                                  y = get(colnames(tmpdata)[1]))) + 
            scale_fill_gradientn(colours = brewer.pal(8, palette_ggplot)) +
            ggplot2::ylab(colnames(tmpdata)[1]) + 
            ggplot2::ggtitle(colnames(tmpdata)[1]) + 
            ggplot2::theme_bw() + 
            ggplot2::theme(plot.title = element_text(hjust = 0.5))
          
        } else {
          graph <- ggplot2::ggplot(tmpdata, environment = environment()) + 
            ggplot2::geom_line(ggplot2::aes( x =  observation, 
                                             y = get(colnames(tmpdata)[1]))) +
            ggplot2::ylab(colnames(tmpdata)[1]) + 
            ggplot2::ggtitle(colnames(tmpdata)[1]) + 
            ggplot2::theme_bw() +
            ggplot2::theme(plot.title = element_text(hjust = 0.5))
        }
      }
      
    } else if (type == "hist") {
      if (js) {
        graph <- rAmCharts::amHist(x = tmpdata[, get(colnames(tmpdata)[1])], 
                                   main = colnames(tmpdata)[1], ylab ="", 
                                   xlab = colnames(tmpdata)[1])
      } else {
        graph <- ggplot2::ggplot(tmpdata, environment = environment()) +
          ggplot2::geom_histogram(
            ggplot2::aes(x = get(colnames(tmpdata)[1])), 
            color = "purple", fill = "lightblue") +
          ggplot2::xlab(colnames(tmpdata)[1]) +
          ggplot2::ggtitle(colnames(tmpdata)[1]) +
          ggplot2::theme_bw() +
          ggplot2::theme(plot.title = element_text(hjust = 0.5))
      }
    }
    
  } else {
    if (js) {
      if(is.null(id)){
        tmpdata[, id := 1:.N]
      }else{
        if(!id%in%colnames(tmpdata)){
          stop("Can't find '", id, "' column in data")
        }
        data.table::setnames(tmpdata, id, "id")
      }
      tmpdata <- tmpdata[order(get(colnames(tmpdata)[1]))]
      
      balloonText <- paste0("Observation num. <b>[[id]]</b><br>",
                            colnames(tmpdata)[1], " :<b>[[x]]</b><br>", 
                            colnames(tmpdata)[2] ,":<b>[[y]]</b><br>")
      
      lineAlpha <- ifelse(type == "line", 1, 0)
      bullet <- "circle"
      
      graph <- rAmCharts::amXYChart(theme = "light", dataProvider = tmpdata) %>>% 
        rAmCharts::addTitle(text = paste(colnames(tmpdata)[1], 
                                         colnames(tmpdata)[2], sep = " ~ ")) %>>%
        rAmCharts::addValueAxes(position = "bottom", axisAlpha = 0, 
                                title = colnames(tmpdata)[1]) %>>% 
        rAmCharts::addGraph(balloonText = balloonText,
                            xField = colnames(tmpdata)[1],
                            yField = colnames(tmpdata)[2],
                            bullet = bullet, lineAlpha = lineAlpha, bulletSize = 6) %>>%
        rAmCharts::setChartCursor() %>>%
        rAmCharts::addValueAxes(title = colnames(tmpdata)[2]) %>>% 
        rAmCharts::setExport(position = "top-right") %>>%
        rAmCharts::plot()
      
    } else {
      tmpdata <- tmpdata[order(get(colnames(tmpdata)[1]))]
      
      if (type != "line") {
        graph <- ggplot2::ggplot(tmpdata, environment = environment()) + 
          ggplot2::geom_point(ggplot2::aes( x =  get(colnames(tmpdata)[1]), 
                                            y = get(colnames(tmpdata)[2]))) + 
          geom_hex(ggplot2::aes( x =  get(colnames(tmpdata)[1]), 
                                 y = get(colnames(tmpdata)[2]))) +
          scale_fill_gradientn(colours = brewer.pal(8, palette_ggplot)) +
          ggplot2::ylab(colnames(tmpdata)[2]) + 
          ggplot2::xlab(colnames(tmpdata)[1]) + 
          ggplot2::ggtitle(paste(colnames(tmpdata)[1], 
                                 colnames(tmpdata)[2], sep = " ~ ")) + 
          ggplot2::theme_bw() +
          ggplot2::theme(plot.title = element_text(hjust = 0.5))
        
      } else {
        graph <- ggplot2::ggplot(tmpdata, environment = environment()) + 
          ggplot2::geom_line(aes( x =  get(colnames(tmpdata)[1]), 
                                  y = get(colnames(tmpdata)[2]))) + 
          ggplot2::ylab(colnames(tmpdata)[2]) + xlab(colnames(tmpdata)[1]) + 
          ggplot2::ggtitle(paste(colnames(tmpdata)[1],
                                 colnames(tmpdata)[2], sep = " ~ ")) + 
          ggplot2::theme_bw() +
          ggplot2::theme(plot.title = element_text(hjust = 0.5))
      }
    }
  }
  
  graph
}

plotBoxplot <- function(data, quanti.var, quali.var = NULL, main ="", js, palette_ggplot){
  
  data[, c(quanti.var) := round(get(quanti.var), 2)]
  
  if (is.null(quali.var)) {
    if (js) {
      amBoxplot(data[, get(quanti.var)], 
                ylab = quanti.var, main = quanti.var)
    } else {
      ggplot2::ggplot(data = data, aes(y = get(quanti.var))) +
        ggplot2::geom_boxplot(notch = TRUE, color = "purple", fill = "lightblue") +
        ggplot2::ylab(c(quanti.var)) + 
        ggplot2::ggtitle(c(quanti.var)) + 
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = element_text(hjust = 0.5))
      
    }
  } else {
    formule <- paste(quanti.var, "~", quali.var)
    if (js) {
      amBoxplot(as.formula(formule), data = data, 
                xlab = quali.var, ylab = quanti.var,
                main = paste(quanti.var, quali.var, sep = " ~ "))
    } else {
      ggplot2::ggplot(data = data, 
                      aes(x = get(quali.var), y = get(quanti.var), fill = get(quali.var))) +
        ggplot2::geom_boxplot(notch = TRUE) +
        ggplot2::scale_fill_brewer(palette = palette_ggplot) +
        ggplot2::stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4) +
        ggplot2::xlab(c(quali.var)) + 
        ggplot2::ylab(c(quanti.var)) + 
        ggplot2::ggtitle(paste(quanti.var, quali.var, sep = " ~ ")) + 
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = element_text(hjust = 0.5))
      
    }
  }
}

plotBarplot <- function(data, js){
  
  var <- colnames(data)[1]
  data <- data[, list(count = .N), by = c(var)]
  if (js) {
    amSerialChart(theme = "light", categoryField = colnames(data)[1], 
                  creditsPosition = "top-left") %>>% 
      addTitle(text = var) %>>%
      setDataProvider(data) %>>% 
      setCategoryAxis(title = var) %>>% 
      addValueAxes(title = "count") %>>%
      addGraph(balloonText = "[[category]]: <b>[[value]]</b>", type = "column",
               valueField = "count", fillAlphas = .8, lineAlpha = .2) %>>% 
      setExport(position = "top-right") %>>% setChartCursor() %>>% 
      plot()
  } else {
    ggplot2::ggplot(data = data, aes(x = get(var), y = count, fill = get(var))) +
      ggplot2::geom_bar(stat = "identity", color = "blue") +
      ggplot2::scale_fill_brewer(palette = palette_ggplot) +
      ggplot2::geom_text(aes(label = count), vjust=1.6, color="white", size=3.5) +
      ggplot2::xlab(c(var)) +
      ggplot2::ggtitle(c(var)) + 
      ggplot2::theme_bw() +
      ggplot2::theme(plot.title = element_text(hjust = 0.5))
  }
}


plotHeatmap <- function(data) {
  keep.column <- colnames(data)
  # b = paste0("list(", paste(keep.column, collapse = ","), ")")
  datacount <- data[, list(count = .N), by = c(keep.column)]
  
  res <- dcast(datacount, as.formula(paste0(
    colnames(data)[1], "~", colnames(data)[2])), value.var='count', fill=0)
  print(res)
  rownames <- res[, get(colnames(res)[1])]
  unval <- length(unique(datacount[, count]))
  
  res <- res[, -1]
  rownames(res) <- rownames
  amHeatmap(res, nclasses = ifelse(unval <= 10, 10, 5), rownames = rownames,
            main = paste0(colnames(data)[1], " ~ ", colnames(data)[2]))
}

#' @title plot boxplot, timeseries and barplot
#' @description This function takes a data.table with one numeric
#' variable and one character, factor or date variable in input and return 
#' a graph of the type chosen by the user, it can do interactive or fix graph.
#' @param data \code{data.table} data.table with one or two columns, one numeric and 
#' another one, character, factor or date
#' @param type \code{character} type of the graph, default is line, possible 
#' values are "boxplot", "barplot" and "timeseries"
#' @param aggregation \code{character} how you want to do the aggregation 
#' (if timeseries), possible values are "Average", "Sum", "Low" and "High"
#' @param js \code{logical} TRUE if you want dynamic graph
#'
#' @import ggplot2 rAmCharts
#' @export
#' @examples
#' \dontrun{
#' iris <- data.table::copy(data.table(datasets::iris))
#' flights <- data.table(nycflights13::flights)
#' 
#' plotExploratory(iris[, list(Petal.Width, Species)], type = "barplot")
#' plotExploratory(iris[, list(Sepal.Width)], type = "boxplot")
#' plotExploratory(flights[, list(time_hour, distance)], type = "timeseries",
#' aggregation = "Average", js = F)
#' 
#'
#' }
#'
plotExploratory <- function (data, type, aggregation = NULL, js = TRUE) {
  
  classx <- class(data[, get(colnames(data)[1])])
  if (ncol(data) > 1) {
    classy <- class(data[, get(colnames(data)[2])])
    
    if (any(classy%in%c("character", "factor")) & 
        !any(classx%in%c("character", "factor"))){
      data <- data[ , c(colnames(data)[2:1]), with = FALSE]
    }
  }
  
  if (type == "boxplot") {
    if (ncol(data) == 2) {
      graph <- plotBoxplot(data, colnames(data)[2], colnames(data)[1], js = js)
    } else {
      graph <- plotBoxplot(data, quanti.var = colnames(data)[1], 
                           quali.var = NULL, js = js)
    }
  } else if (type == "barplot") {
    graph <- plotBarplot(data, js = js)
    
  } else if (type == "timeseries") {
    dataseries <- data.table(data)
    
    graph <- plotTimeSeries(dataseries, col.date = colnames(dataseries)[1], 
                            col.series = colnames(dataseries)[2], 
                            aggregation = aggregation, js = js)
  } else {
    graph <- plotScatterplot(data, type = type)
  }
  graph
}

plotStaticExploratory <- function(data, type, aggregation = NULL, js = FALSE,
                                  palette_ggplot) {
  
  classx <- class(data[, get(colnames(data)[1])])
  if (ncol(data) > 1) {
    classy <- class(data[, get(colnames(data)[2])])
    
    if (any(classy %in% c("character", "factor")) & 
        !any(classx %in% c("character", "factor"))){
      data <- data[ , colnames(data)[2:1], with = FALSE]
    }
  }
  
  if (type == "timeseries") {
    dataseries <- data.table(data)
    
    graph <- plotTimeSeries(dataseries, col.date = colnames(dataseries)[1], 
                            col.series = colnames(dataseries)[2], 
                            aggregation = aggregation, point.size = 0, js = js)
    
  } else if (type == "boxplot") {
    if (ncol(data) == 2) {
      graph <- plotBoxplot(data, colnames(data)[2], colnames(data)[1], js = js,
                           palette_ggplot = palette_ggplot)
    } else {
      graph <- plotBoxplot(data, quanti.var = colnames(data)[1], 
                           quali.var = NULL, js = js, 
                           palette_ggplot = palette_ggplot)
    }
    
  } else if (type == "barplot") {
    graph <- plotBarplot(data, js = js, palette_ggplot = palette_ggplot)
  } else {
    graph <- plotScatterplot(data, type = type, js = js, 
                             palette_ggplot = palette_ggplot)
  }
  graph
}

#' @title Plot time series
#' @title This function can plot timeseries in a dynamic or static way
#' @param data : data
#' @param col.date : column name for data values
#' @param col.series : column(s) name for serie(s)
#' @param main : Title
#' @param aggregation : Javascript only. one of 'Average' (Default), 'Sum', 'Low', 'High'
#' @param col : color
#' @param point.size : point size, default to 5
#' @param maxSeries : Javascript only. maximum number of points 
#' before aggregation. Default to 300
#' @param js : boolean. TRUE (Default) = javascript plot, FALSE = base plot
#' @param ylab : label of the numeric variable
#' 
#' @examples
#' \dontrun{
#' data <- data.frame(date = seq(ISOdate(2000,1,1), by = "10 min", length.out = 500), 
#'  conso = rnorm(500), fitted.values = rnorm(500))
#'  
#' plotTimeSeries(data, col.date = "date", col.series = "conso", main = "Consommation")
#' 
#' plotTimeSeries(data, col.date = "date", col.series = "conso", main = "Consommation", js = FALSE)
#' 
#' plotTimeSeries(data, col.date = "date", col.series = "conso",
#'  aggregation = "Sum")
#'  
#' plotTimeSeries(data, col.date = "date", col.series = c("conso", "fitted.values"),
#'  col = c("blue", "purple"), point.size = 2)
#'  
#'plotTimeSeries(data, col.date = "date", col.series = c("conso", "fitted.values"),
#'  col = c("blue", "purple"), point.size = 2, js = FALSE)
#'  }
#'  
#' @export
#' 
plotTimeSeries <- function(data, col.date, col.series, ylab = "values",
                           main = NULL, aggregation = "Average", 
                           col = c("red", "black"),
                           point.size = 5,
                           maxSeries = 300, js = TRUE) {
  if ("data.table" %in% class(data)) {
    data <- as.data.frame(data)
  }
  
  if (any(!col.date %in% colnames(data))) {
    stop("Can't find '", col.date, "' in data")
  }
  
  if (any(!col.series %in% colnames(data))) {
    stop("Can't find '",
         paste(col.series[!col.series %in% colnames(data)], collapse="', '"),
         "' in data")
  }
  
  data <- data[, c(col.date, col.series)]
  
  if (js) {
    if (!aggregation %in% c('Average', 'Low', 'High', 'Sum')) {
      stop("Invalid aggregation value")
    }
    shape <- c("round", "square", "triangleUp", "triangleDown",
               "triangleLeft", "triangleRight", "none")
    linetype <- c(0,5, 10, 15, 20)
    if (!"character" %in% class(data[, 1])) {
      data[, 1] <- as.character(data[, 1])
    }
    
    mycategoryBalloonDateFormat <- (
      list(list(period="YYYY", format="YYYY"), 
           list(period="MM", format="YYYY-MM"), 
           list(period="WW", format="YYYY-MM-DD"), 
           list(period="DD", format="YYYY-MM-DD"), 
           list(period="hh", format="YYYY-MM-DD JJ:NN"), 
           list(period="mm", format="YYYY-MM-DD JJ:NN"), 
           list(period="ss", format="YYYY-MM-DD JJ:NN:ss"), 
           list(period="fff", format="YYYY-MM-DD JJ:NN:ss")))
    
    graph <- amStockChart(dataDateFormat="YYYY-MM-DD JJ:NN:ss") %>>% 
      setProperties(export=list(enabled=TRUE)) %>>% 
      addDataSet(dataSet(title="Data set", categoryField=col.date) %>>%
                   setDataProvider(data, keepNA=FALSE)) 
    
    FieldMapping <- lapply(colnames(data)[-1], function(x) {
      list(fromField=x, toField=x)
    })
    
    graph@dataSets[[1]]$fieldMappings <- FieldMapping
    
    graph <- addPanel(graph, stockPanel(showCategoryAxis=TRUE, title=ylab) %>>%
                        setStockLegend(periodValueTextRegular="[[value.close]]")) 
    stockGraphs <- lapply(2:ncol(data), function(x) {
      column <- colnames(data)[x]
      i <- x - 1
      mybullet <- shape[ifelse(i %% length(shape) == 0, length(shape),
                               i %% length(shape))]
      mycolor <- col[ifelse(i %% length(col) == 0, length(col),
                            i %% length(col))]
      mydashLength <- linetype[ifelse(i %% length(linetype) == 0,
                                      length(linetype), i %% length(linetype))]
      
      list(id=paste0("g", x), title=column, connect=FALSE, valueField=column,
           comparable=TRUE, periodValue=aggregation,
           compareField=column,
           balloonText=paste0(column," : <b>[[value]]</b>"), precision=1,
           bullet=mybullet, lineColor=mycolor, dashLength=mydashLength,
           bulletSize=point.size,
           compareGraphBalloonText="[[title]] =<b>[[value]]</b>",
           useDataSetColors=FALSE)
    })
    
    graph@panels[[1]]$stockGraphs <- stockGraphs
    setChartScrollbarSettings(graph, graph="g1") %>>% 
      setChartCursorSettings(valueBalloonsEnabled=TRUE, fullWidth=TRUE,
                             cursorAlpha=0.1, valueLineBalloonEnabled=TRUE,
                             valueLineEnabled=TRUE, valueLineAlpha=0.5,
                             categoryBalloonDateFormats=mycategoryBalloonDateFormat) %>>%
      setPeriodSelector(periodSelector(position="bottom",
                                       inputFieldsEnabled=FALSE) %>>%
                          addPeriod(period="DD", selected=TRUE, count=1,
                                    label="1 day") %>>%
                          addPeriod(period="WW", count=1, label="1 week") %>>%
                          addPeriod(period="MAX", label="All")) %>>% 
      setCategoryAxesSettings(parseDates=TRUE, minPeriod="10mm",
                              groupToPeriods=c("10mm", "30mm", "hh", "3hh"),
                              maxSeries=maxSeries) %>>% 
      setPanelsSettings(recalculateToPercents="never",
                        creditsPosition="top-left") %>>% 
      plot()
  } else {
    
    if (!"POSIXct" %in% class(data[, 1])) {
      data[, 1] <- as.POSIXct(data[, 1])
    }
    shape <- 16:20
    linetype <- c("solid", "dashed", "dotted", "dotdash",
                  "longdash", "twodash")
    graph <- ggplot(data) + 
      ylab(ylab) + 
      ggtitle(main) +
      xlab("date")
    
    for (x in 2:ncol(data)) {
      i <- x - 1
      mybullet <- shape[ifelse(i %% length(shape) == 0, length(shape),
                               i %% length(shape))]
      mycolor <- col[ifelse(i %% length(col) == 0, length(col),
                            i %% length(col))]
      mydashLength <- linetype[ifelse(i %% length(linetype) == 0,
                                      length(linetype), i %% length(linetype))]
      
      graph <- graph + 
        geom_line(aes(x = get(col.date), y = get(colnames(data)[x])),
                  color=mycolor, linetype=mydashLength) + 
        geom_point(aes(x = get(col.date), y = get(colnames(data)[x])),
                   color=mycolor, size=point.size) + 
        theme_bw()
    }
    graph
  }
}