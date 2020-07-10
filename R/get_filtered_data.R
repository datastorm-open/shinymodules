.filterDataTable <- function(data, filters){
  
  expr <- paste0(unlist(lapply(filters, function(x){
    values <- x$values
    if(!is.vector(values) && ! is.null(values)){
      if(any(class(values) %in% c("character", "factor", "IDate", "Date"))){
        values = paste0('"', values, '"')
      }else if(class(values) %in% "POSIXct"){
        values = paste0("as.POSIXct('", paste(values, collapse = ","), "')")
      }else if(class(values) %in% "POSIXlt"){
        values = paste0("as.POSIXlt('", paste(values, collapse = ","), "')")
      }
    }else{
      if(any(class(values) %in% c("character", "factor"))){
        values = paste0('c("', paste(values, collapse = '","'), '")')
      }else {
        values = paste0("c(", paste(values, collapse = ","), ")")
      }
    }
    paste0("`", x$column, "`", x$fun, values)
  })), collapse = " & ")
   data[eval(parse(text = expr))]
}
