#' Associeted colors to data.frame
#' @param data : data.frame
#' @param nbclasses : number of classes
#' @param col : 3 col to use in colorRampPalette
#' @param colorby : can be "all","row","col". 
#' 
#' 
#' @return data.frame compound to original data.frame and associated color data.frame
#' 
colorData <- function(data, nbclasses = NULL, col=c("#FF0000","#FFFFFF","#0000FF"),
                      colorby="all")
{
  
  if(colorby=="all")
  {
    framclasses <- matrix(0, nrow = nrow(data), ncol = ncol(data))
    values <- unlist(c(data))
    if(nbclasses < length(unique(values))){
      classes <- stats::quantile(values, seq(from = 0, to = 1, length.out = nbclasses + 1))
      for(i in 1:(length(classes) - 1))
      {
        framclasses = framclasses + ((data >= classes[i]) + 1 - 1)
      }
    }else{
      nbclasses <- length(unique(values)) 
      classes <- sort(unique(values))
      for(i in 1:length(classes))
      {
        framclasses = framclasses + ((data >= classes[i]) + 1 - 1)
      }
    }
  }
  
  if(colorby == "col")
  {
    framclasses <- matrix(0, nrow = nrow(data), ncol = ncol(data))
    for(j in 1:ncol(data))
    {
      classes <- stats::quantile(sort((unlist(c(data[,j])))),
                          seq(from = 0, to = 1,length.out = nbclasses+1))
      
      for(i in 1:(length(classes)-1))
      {
        framclasses[, j] = framclasses[, j] + ((data[, j] >= classes[i]) + 1 - 1)
      }
    }
  }
  
  if(colorby == "row")
  {
    framclasses <- matrix(0, nrow = nrow(data), ncol = ncol(data))
    for(j in 1:nrow(data))
    {
      
      classes <- stats::quantile(sort((unlist(c(data[j,])))), 
                          seq(from = 0, to = 1,length.out = nbclasses+1))
      for(i in 1:(length(classes)-1))
      {
        framclasses[j,]=framclasses[j,]+((data[j,]>=classes[i])+1-1)
      }
    }
  }
  
  color <- grDevices::colorRampPalette(col)(nbclasses)
  for(i in 1:length(color)){
    framclasses[framclasses == as.character(i)] <- color[i]
  }
  framclasses <- data.frame(framclasses)
  names(framclasses) <- paste0(names(data), "col")
  framclasses[] <- lapply(framclasses, as.character)
  list(data = cbind(data,framclasses), 
       classes = list(nclasses = nbclasses, labels = classes))
}

#' Associeted constructor data.frame to initial data.frame
#' @param data : data.frame
#' @param rownames : Rownames of the heatmap graphic
#' @return data.frame compound to original data.frame 
#' and associated constructor data.frame
#' 
constructdata <- function(data, rownames){
  construct <- matrix(1,ncol=ncol(data)/2,nrow=nrow(data))
  construct <- data.frame(construct)
  names(construct) <- paste0(names(data)[1:(ncol(data)/2)], "construct")
  return(cbind(row = rownames, data,construct))
}


#' Make chart
#' @param data : data.frame
#' @param labels : TRUE FALSE, display labels
#' @param cex : size of labels
#' @param xLabelsRotation : rotation of xlabels
#' @param colorby : can be "all","row","col". 
#' @param col : 3 col to use in colorRampPalette
#' @param classes : classes
#' @param main : Title of the heatmap
#' @param rownames : Rownames of the heatmap graphic
#' 
#' 
#' 
#' @return data.frame compound to original data.frame and associated constructor data.frame
#' 
heatmap <- function(data, classes, labels = TRUE, cex = 10, main = "",
                    xLabelsRotation = 45, colorby = "all", rownames,
                    col = c("#FF0000", "#FFFFFF", "#0000FF")){
  # browser()
  ncate <- (ncol(data)-1)/3
  
  namecat <- names(data[,2:(ncate+1)])
  
  values <- paste0("['", paste(namecat, collapse = "','"), "']")
  

  chart <- sapply(namecat, function(x){
    if(labels){
      labelText <- paste0("[[",x,"]]")
    }else{
      labelText <- ""
    }
    amGraph(balloonText=paste0(
      "<b>[[title]]-[[category]]</b><br><b> count : </b>[[",x,"]]"),
      fillAlphas=0.8, labelText = labelText, lineAlpha = 0.3,fontSize = cex,
      title = x, type = "column", fillColorsField = paste0(x, "col"),
      valueField = paste0(x, "construct"))}, USE.NAMES = FALSE
  )
  
  guides = list()
  n <- length(colnames(data[, 2:(ncate+1)]))
  k <- 0
  for(i in 1:n)
  {
    k <- k +1
    guides[[k]] <- guide(id = paste0("guide", i), value = i, toValue = i, 
                         lineAlpha = 1, color = "#000000", lineThickness = 1)
    
  }
  n <- nrow(data)
  for(i in 1:n)
  {

    guides[[k]] = guide(id = paste0("guide", k), category = rownames[i],
                        lineAlpha=1,color="#000000", lineThickness = 1,
                        above=TRUE,expand=TRUE)
    k <- k +1
  }
  
  legendlist <- list()
  
  if(colorby=="all")
  {
    
    nbclasses <- classes$nclasses
    classes <- classes$labels
    color <- grDevices::colorRampPalette(col)(nbclasses)
    
    associated <- NULL
    if(nbclasses < length(classes)){
      for(i in 1:length(classes)-1){
        associated[i] <- paste0("[",classes[i]," , ",classes[i+1], 
                                ifelse(i==length(classes)-1, "]", "["))
      }
    }else{
      associated <- classes
    }
    
    datatemp <- data.frame(title=associated,color=color)
    for(i in 1:nrow(datatemp))
    {
      legendlist[[i]] <- list(title=as.character(datatemp[i,1]),
                              color = as.character(datatemp[i,2]))
    }
  }else{
    legendlist[[1]]<-list(title="Low",color = as.character(col)[1])
    legendlist[[2]]<-list(title="Medium",color = as.character(col)[2])
    legendlist[[3]]<-list(title="Large",color = as.character(col)[3])
  }
  
  
  amSerialChart()%>>%
    setBalloon(borderThickness = 0) %>>%
    setDataProvider(data) %>>%
    setProperties(type = "serial", theme = "light", columnWidth = 1, 
                  categoryField = "row",
                  gridAboveGraphs = TRUE, rotate = TRUE)%>>%
    setGuides(guides)%>>%
    addTitle(text=main)%>>%
    setLegend(data = (legendlist), markerBorderColor = "#000000", align = "center") %>>%
    addValueAxes(stackType = "regular", axisAlpha = 0, 
                 gridThickness = 0, gridAlpha = 1,
                 position = "left", labelRotation = xLabelsRotation, 
                 maximum = ncate,
                 labelFunction = htmlwidgets::JS(paste0(
                   "function(value,valueString,axis){
                                                        Math.trunc = Math.trunc || function(x) {
                                                        return x < 0 ? Math.ceil(x) : Math.floor(x);
                                                        };                                                                                                         
                                                        var val = ", values, ";
                                                        var indice = Math.trunc(value);
                                                        if(indice < val.length && value % 1 != 0){
                                                        return val[indice];
                                                        }else{
                                                        return '';
                                                        }
                                                        ;}"))) %>>%
    setGraphs(chart)%>>%
    setCategoryAxis(gridPosition = "start", axisAlpha = 1, gridThickness = 0,
                    gridAlpha = 1)%>>%
    setExport(enabled = TRUE, 
              menu = list(
                list(
                  class = "export-main",
                  menu = list(
                    list(
                      label = "Download as ...",
                      menu = list("PNG", "JPG", "SVG", "PDF")
                    ),
                    list(
                      label = "Save data as CSV",
                      click = htmlwidgets::JS(paste0('function() {
                                                     
                                                     var cfg = {
                                                     data: this.getChartData(),
                                                     delimiter: ",",
                                                     quotes: true,
                                                     escape: true,
                                                     dateFields: [],
                                                     dateFormat: this.setup.chart.dataDateFormat || "YYYY-MM-DD"
                                                     };
                                                     
                                                     var data = "";
                                                     
                                                     if ( this.setup.chart.categoryAxis && this.setup.chart.categoryAxis.parseDates && this.setup.chart.categoryField ) {
                                                     cfg.dateFields.push( this.setup.chart.categoryField );
                                                     }
                                                     
                                                     //header
                                                     row = 0;
                                                     var buffer = [];
                                                     var cpt = 1;
                                                     for ( col in cfg.data[ row ] ) {
                                                     if(cpt <= ', ((ncol(data)-1)/3)+1, '){
                                                     var value = cfg.data[ row ][col];
                                                     value = col;
                                                     
                                                     if ( typeof value === "string" ) {
                                                     if ( cfg.escape ) {
                                                     value = value.replace( \'"\', \'""\' );
                                                     }
                                                     if ( cfg.quotes ) {
                                                     value = [ \'"\', value, \'"\' ].join( "" );
                                                     }
                                                     }
                                                     
                                                     buffer.push( value );
                                                     cpt = cpt+1;
                                                     }
                                                     }
                                                     data += buffer.join( cfg.delimiter ) + "\\n";
                                                     
                                                     for ( row in cfg.data ) {
                                                     var cpt = 1;
                                                     var buffer = [];
                                                     
                                                     for ( col in cfg.data[ row ] ) {
                                                     if(cpt <= ', ((ncol(data)-1)/3)+1, '){
                                                     var value = cfg.data[ row ][ col ];
                                                     
                                                     if ( typeof value === "string" ) {
                                                     value = value;
                                                     } else if ( cfg.dateFormat && value instanceof Date && cfg.dateFields.indexOf( col ) != -1 ) {
                                                     value = AmCharts.formatDate( value, cfg.dateFormat );
                                                     }
                                                     
                                                     // WRAP IN QUOTES
                                                     if ( typeof value === "string" ) {
                                                     if ( cfg.escape ) {
                                                     value = value.replace( \'"\', \'""\' );
                                                     }
                                                     if ( cfg.quotes ) {
                                                     value = [ \'"\', value, \'"\' ].join( "" );
                                                     }
                                                     }
                                                     
                                                     buffer.push( value );
                                                     cpt = cpt +1;
                                                     }
                                                     }
                                                     data += buffer.join( cfg.delimiter ) + "\\n";
                                                     };
                                                     this.download( data, "text/plain", "heatmap.csv" );}')
                      ))
                  )
                )
              )
    ) %>>%
    plot()
  
}


#' Amchart Heat-Map
#' @param data : data.frame, should be a contingency table
#' @param nbclasses : number of classes
#' @param col : 3 col to use in colorRampPalette
#' @param labels : TRUE FALSE, display labels
#' @param cex : size of labels
#' @param main : title
#' @param xLabelsRotation : rotation of xlabels
#' @param colorby : can be "all","row","col". 
#' @param legend : TRUE or FALSE, display legend
#' @param rownames : Rownames of the heatmap graphic
#' 
#' @examples
#' \dontrun{
#' data(USArrests, "VADeaths")
#' USArrests <- USArrests [1:10,]
#' amHeatmap(USArrests, rownames = rownames(USArrests))
#' amHeatmap(USArrests, nbclasses=5, col = c("#FF0000", "#FFFFFF", "#0000FF"),
#' labels = TRUE, cex = 10, main="My title", xLabelsRotation = 45, colorby = "all",
#' legend = TRUE, rownames = rownames(USArrests))
#' amHeatmap(USArrests, nbclasses = 5, col = c("#FF0000","#FFFFFF","#0000FF"),
#' labels = TRUE, cex = 10, main = "My title",xLabelsRotation = 45,
#' colorby="row",legend = TRUE, rownames = rownames(USArrests))
#' amHeatmap(USArrests, nbclasses = 5, col = c("#FF0000", "#FFFFFF", "#0000FF"),
#' labels = TRUE, cex = 10, main = "My title", xLabelsRotation = 45,
#' colorby="col",legend = TRUE, rownames = rownames(USArrests))
#' amHeatmap(USArrests, nbclasses=10, col=c("#00FF00","#FF00FF","#0000FF"),
#' labels = TRUE, cex = 10, main = "My title", xLabelsRotation = 45, 
#' colorby="all",legend = TRUE, rownames = rownames(USArrests))
#' }
#' @return data.frame compound to original data.frame and associated constructor data.frame
#' @import rAmCharts pipeR

.amHeatmap <- function(data, nbclasses = 5, col = c("#FF0000","#FFFFFF","#0000FF"), 
                      labels = TRUE, cex=10, main="", rownames,
                      xLabelsRotation=45, colorby="all", legend = TRUE) {
  
  
  colordata <- colorData(data, nbclasses, col, colorby)
  data <- constructdata(colordata$data, rownames = rownames)
  heatmap(data, colordata$classes, labels, cex, rownames = rownames,
          main, xLabelsRotation, colorby,col)
}



#' @title Heatmap function with the DT package
#' @description This function takes data with two factor/character variables
#' and return a DT object in a heatmap form
#' @param data \code{data.table} data.table with two character columns
#' @return DT in a heatmap form
#' @import DT data.table

#' @examples 
#' \dontrun{
#' titanic <- data.table(datasets::Titanic)
#' get_Heatmapdt(titanic[, list(Class, Age)])
#' 
#' 
#' }
.get_Heatmapdt <- function(data) {

  dtCast <- data[, .N, by = c(colnames(data)[1], colnames(data)[2])]
  uniq_var1 <-length(unique(dtCast[, get(colnames(dtCast)[1])]))
  uniq_var2 <- length(unique(dtCast[, get(colnames(dtCast)[2])]))

  if (uniq_var2 > uniq_var1) {
    setcolorder(dtCast, c(colnames(dtCast)[2], colnames(dtCast)[1]))
  }
  dtCast <- dcast(dtCast, get(colnames(dtCast)[1])~get(colnames(dtCast)[2]), value.var = "N")
  for(j in seq_along(dtCast)){
    set(dtCast, i = which(is.na(dtCast[[j]])), j = j, value = 0)
  }
  
  rownames(dtCast) <- unlist(dtCast[, 1])
  dtCast[, c(colnames(dtCast)[1]) := NULL]

  rownames(dtCast) <- paste('<span style="color:grey"><b>', rownames(dtCast),
                            '</span></b>')
  colnames(dtCast) <- paste('<span style="color:purple"><b>', colnames(dtCast),
                            '</span></b>')
  
  
  brks <- stats::quantile(dtCast, probs = seq(.05, .95, .05), na.rm = TRUE)
  color <- round(seq(255, 40, length.out = length(brks) + 1), 0)
  color <- paste0("rgb(", color, ", ", color, ", ", "255)")
  
  dtCastdt <- DT::datatable(
    dtCast, rownames = T, escape = F, 
    class = 'cell-border stripe', options = list(
      pageLength = 20, lengthMenu = c(5, 10, 20, 50), 
      dom = 'Blfrtip', scrollX = TRUE,
      drawCallback =  htmlwidgets::JS(
        'function(){debugger;HTMLWidgets.staticRender();}'),
      columnDefs = list(
        list(className = 'dt-center', 
             targets = 1:ncol(dtCast))))) %>%
    formatStyle(names(dtCast), backgroundColor = styleInterval(brks, color))

  return(dtCastdt)
}