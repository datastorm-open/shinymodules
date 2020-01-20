# module shiny pour afficher une table avec DT, 
# en mettant trois boutons d'export dessous

#' Module d'affichage DT avec export. Partie UI
#' 
#' @param id : character. id
#' 
#' @import shiny
#' @importFrom DT DTOutput
#' 
show_DT_UI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(12, 
           # affichage de la table
           withSpinner(
             DT::DTOutput(ns("table"))
           ),
           fluidRow(
             # bouton export .csv
             column(width = 2, offset = 3, div(downloadButton(ns("export_csv"), 'CSV'), align = "center")),
             # bouton export .xlsx
             column(2, div(downloadButton(ns("export_excel"), 'EXCEL'), align = "center")),
             # bouton export .xlsx
             column(2, div(downloadButton(ns("export_html"), 'HTML'), align = "center"))
           )
    )
  )
}

#' Module d'affichage DT avec export. Partie SERVER
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
#' 
#' @import openxlsx htmlwidgets shiny
#' @importFrom DT renderDT datatable
#' @importFrom utils write.table
#' 
#' @examples 
#' 
#' \dontrun{
#' # ui
#' ui = shiny::fluidPage(show_DT_UI("iris_module"))
#' server = function(input, output, session) {
#'   callModule(show_DT, "iris_module", reactive(iris), reactive(DT::datatable(iris)), 
#'    paste0("Iris_export", format(Sys.time(), format = "%d%m%Y_%H%M%S")))
#' }
#' shiny::shinyApp(ui = ui, server = server)
#'   
#' }  
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
      htmlwidgets::saveWidget(dt(), con, selfcontained = TRUE)
    }
  )
  return(TRUE)
}