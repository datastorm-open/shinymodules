.filterDataTable <- function(data, filters){
  
  expr <- paste0(unlist(lapply(filters, function(x){
    values <- x$values
    if(!is.vector(values)){
      if(class(values)%in%c("character", "factor", "IDate", "Date")){
        values = paste0("'", values, "'")
      }
    }else{
      if(class(values)%in%c("character", "factor")){
        values = paste0("c('", paste(values, collapse = "','"), "')")
      }else{
        values = paste0("c(", paste(values, collapse = ","), ")")
      }
    }
    paste0(x$column, x$fun, values)
  })), collapse = " & ")
  
  data[eval(parse(text = expr))]
}