#' @title  preprocessing on data 
#' @description preprocessing of input data, return two DT, one with
#' statistics on numeric variables and another one with statistics on factor variables
#' 
#' @param con \code{SQLiteConnection} SQLite connection
#' 
give_info_data_sql <- function(con){
  colname <- colnames(con)
  class_col <- lapply(con %>% head(100) %>% collect(), class)
  
  ##MinMax for num
  num <- which(unlist(class_col) %in% c("numeric", "integer"))
  col_num <- colname[num]
  chr <- which(unlist(class_col) %in% c("character"))
  col_chr <- colname[chr]
  min_max <- NULL
  min_max <- sapply(col_num, function(X){
    min <- con %>% summarise(min = min( !!as.name(X), na.rm = TRUE)) %>% collect()
    max <- con %>% summarise(max = max( !!as.name(X), na.rm = TRUE)) %>% collect()
    c(min, max)
  }, simplify = F)
  #Unique for chr
  chr_unique <- NULL
  chr_unique <- sapply(col_chr, function(X){
    unique <- con %>% summarise(unique = distinct( !!as.name(X))) %>% collect()
    as.vector(unlist(unique))
  }, simplify = F)
  
  all_stats <- c(min_max, chr_unique)
  all_stats
}
