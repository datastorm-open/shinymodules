#' @export
show_DT_UI <- function(id, export = c("csv", "excel", "html")) {
  ns <- NS(id)
  
  wanted_export <- intersect(export, c("csv", "excel", "html"))
  
  if(length(wanted_export) == 3){
    c_offset <- 3
    c_width <- 2
  } else if(length(wanted_export) == 2){
    c_offset <- 3
    c_width <- 3
  } else if(length(wanted_export) == 1){
    c_offset <- 4
    c_width <- 4
  }
  
  fluidRow(
    column(12, 
           # affichage de la table
           withSpinner(
             DT::DTOutput(ns("table"))
           ),
           
           if(length(wanted_export) > 0){
             do.call("fluidRow", lapply(1:length(wanted_export), function(i){
               offset = 0
               if(i == 1) offset <- c_offset
               
               id_btn <- paste0("export_", wanted_export[i])
               
               column(width = c_width, offset = offset, 
                      div(downloadButton(ns(id_btn), toupper(wanted_export[i])), align = "center"))
             }))
           }
    )
  )
}

#' Show & export table using DT
#' 
#' @param input Not a real parameter, should not be set manually. 
#' Done by callModule automatically.
#' @param output Not a real parameter, should not be set manually. 
#' Done by callModule automatically.
#' @param session Not a real parameter, should not be set manually. 
#' Done by callModule automatically.
#' @param data : reactive. \code{data.frame} / \code{data.table}
#' @param dt : reactive. dt (\code{datatable}) with options
#' @param file_name : character. Name of output file
#' @param row.names : \code{logical}
#' @param server : \code{logical}
#' @param id : character. id
#' @param export : character. Wanted export type ? Can be multiple
#' 
#' @import openxlsx htmlwidgets shiny
#' @importFrom DT renderDT datatable DTOutput
#' @importFrom utils write.table
#' 
#' @examples 
#' 
#' \dontrun{
#' # ui
#' ui = shiny::fluidPage(show_DT_UI("iris_module", export = c("csv", "html")))
#' 
#' server = function(input, output, session) {
#'   callModule(show_DT, "iris_module", reactive(iris), reactive(DT::datatable(iris)), 
#'    paste0("Iris_export", format(Sys.time(), format = "%d%m%Y_%H%M%S")))
#' }
#' shiny::shinyApp(ui = ui, server = server)
#'   
#' }  
#' 
#' @export
#' 
#' @rdname show_DT_module
#' 
show_DT <- function(input, output, session, data, dt, file_name, row.names = FALSE, server = TRUE) {
  
  # output DT
  output$table <- DT::renderDT({
    dt()
  }, server = server)
  
  # export .csv
  output$export_csv <- downloadHandler(
    filename = function() {
      paste0(file_name, '.csv')
    },
    content = function(con) {
      write.table(data(), con, sep = ";", dec = ",", row.names = row.names, na = "", fileEncoding = "latin1")
    }
  )
  
  # export excel
  output$export_excel <- downloadHandler(
    filename = function() {
      paste0(file_name, '.xlsx')
    },
    content = function(con) {
      openxlsx::write.xlsx(data(), con, col.names=TRUE, row.names=row.names, keepNA = FALSE)
    }
  )
  
  # export html
  output$export_html <- downloadHandler(
    filename = function() {
      paste0(file_name, '.html')
    },
    content = function(con) {
      dt <- dt()
      if(is.null(dt$width)) dt$width <- "100%"
      htmlwidgets::saveWidget(dt, con, selfcontained = TRUE)
    }
  )
  return(TRUE)
}