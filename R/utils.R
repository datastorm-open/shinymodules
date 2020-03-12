.initProgress <- function(session, message = NULL, detail = NULL, 
                          min  = 0, max = 1){
  if(!is.null(session)){
    progress <- Progress$new(session, min = min, max = max)
    progress$set(message = message, detail = detail)
  } else {
    progress <- NULL
  }
  progress
}