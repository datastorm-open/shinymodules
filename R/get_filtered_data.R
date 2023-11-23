.filterDataTable <- function(data, filters){
  
  expr <- paste0(unlist(lapply(filters, function(x){
    values <- x$values
    if(!is.vector(values) && ! is.null(values)){
      if(any(class(values) %in% c("character", "factor", "IDate", "Date"))){
        values <- paste0('"', values, '"')
        values <- gsub('"NA"', "NA", values, fixed = TRUE)
      }else if(class(values) %in% "POSIXct"){
        values <- paste0("as.POSIXct('", paste(values, collapse = ","), "')")
      }else if(class(values) %in% "POSIXlt"){
        values <- paste0("as.POSIXlt('", paste(values, collapse = ","), "')")
      }
    }else{
      if(any(class(values) %in% c("character", "factor"))){
        values <- paste0('c("', paste(values, collapse = '","'), '")')
        values <- gsub('"NA"', "NA", values, fixed = TRUE)
      }else {
        values <- paste0("c(", paste(values, collapse = ","), ")")
      }
    }

    paste0("`", x$column, "`", x$fun, values)
    
  })), collapse = " & ")
   data[eval(parse(text = expr))]
}


transform_names <- function(old_names){
  if(is.null(old_names)){
    stop('No input provided!')
  }
  
  i <- gsub("^[[:space:]]+", "", old_names)
  i <- gsub("([[:space:]]+)$", "",  i)
  i <- gsub("^(([.])+)", "", i)
  i <- gsub("(([.])+)$", "", i)
  i <- gsub("[[:space:]]+", "_",  i)
  i <- gsub("([.])+", "_",  i)
  i <- gsub("-", "_", i)
  i <- iconv(i, to='ASCII//TRANSLIT', sub = "")
  i <- tolower(i)
  new_names <- gsub("[^[:alnum:]_]+", "", i)
  
  return(new_names)
}

.filterDataTableFile <- function(data, data_filters){
  
  col_names <- colnames(data)
  colnames(data) <- transform_names(colnames(data))
  colnames(data_filters) <- transform_names(colnames(data_filters))
  
  res <- copy(data)
  miss_col <- c()
  
  for(i in colnames(data_filters)){
    values_ <- data_filters[, unique(get(i))]
    values_ <- values_[!is.na(values_)]
  
    if(i %in% colnames(res)){
      if(!isTRUE(all.equal(class(res[, get(i)]), class(values_)))){
        if("integer64" %in% class(res[, get(i)])){
          if("factor" %in% class(values_)) values_ <- as.character(values_)
          values_ <- as.integer64(values_)
        } else if("numeric" %in% class(res[, get(i)])){
          if("factor" %in% class(values_)) values_ <- as.character(values_)
          values_ <- as.numeric(values_)
        } else if("character" %in% class(res[, get(i)])){
          values_ <- as.character(values_)
        } else if("factor" %in% class(res[, get(i)])){
          values_ <- as.character(values_)
        } else if("logical" %in% class(res[, get(i)])){
          if("factor" %in% class(values_)) values_ <- as.character(values_)
          values_ <- as.logical(values_)
        } else if("Date" %in% class(res[, get(i)])){
          if("factor" %in% class(values_)) values_ <- as.character(values_)
          pattern <- "^[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}$"
          if(regexpr(pattern, values_[1])[1] > -1){
            values_ <- as.Date(values_, format = "%Y-%m-%d")
          } 
          
          pattern <- "^[[:digit:]]{4}/[[:digit:]]{2}/[[:digit:]]{2}$"
          if(regexpr(pattern, values_[1])[1] > -1){
            values_ <- as.Date(values_, format = "%Y/%m/%d")
          } 
          
          pattern <- "^[[:digit:]]{2}/[[:digit:]]{2}/[[:digit:]]{4}$"
          if(regexpr(pattern, values_[1])[1] > -1){
            values_ <- as.Date(values_, format = "%d/%m/%Y")
          } 
          
          pattern <- "^[[:digit:]]{2}-[[:digit:]]{2}-[[:digit:]]{4}$"
          if(regexpr(pattern, values_[1])[1] > -1){
            values_ <- as.Date(values_, format = "%d-%m-%Y")
          } 
        }
      }
      res <- res[get(i) %in% values_]
    } else {
      miss_col <- c(miss_col, i)
    }
  }
  
  if(length(miss_col) > 0){
    showModal(
      modalDialog(
        title = "Filtering from file : missing columns",
        paste(miss_col, collapse = ", "),
        easyClose = TRUE,
        footer = modalButton("OK"),
      )
    )
  }
  colnames(res) <- col_names
  res
}