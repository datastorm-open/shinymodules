% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_data.R
\name{filter_data_UI}
\alias{filter_data_UI}
\alias{filter_data}
\title{UI part of the module filter_data}
\usage{
filter_data_UI(id, titles = TRUE)

filter_data(
  input,
  output,
  session,
  data = NULL,
  columns_to_filter = "all",
  max_char_values = 1000,
  default_multisel_n = 10,
  labels = list(title = "Filters", no_data = "No data available", filter =
    "Filter on :", reinitialize = "Reinitialize filters", validate =
    "Apply filtering on data", complete_data = "Get complete dataset")
)
}
\arguments{
\item{id}{\code{character} An id that will be used to create a namespace}

\item{titles}{\code{logical} Add titles on UI ? Default to TRUE}

\item{input}{Not a real parameter, should not be set manually. 
Done by callModule automatically.}

\item{output}{Not a real parameter, should not be set manually. 
Done by callModule automatically.}

\item{session}{Not a real parameter, should not be set manually. 
Done by callModule automatically.}

\item{data}{\code{data.frame/reactive} reactive data.table}

\item{columns_to_filter}{\code{character/reactive} vector o column names you want to 
allow the user to filter (default is all)}

\item{max_char_values}{\code{integer/reactive} Remove character / factor columns with more than \code{max_char_values} unique values}

\item{default_multisel_n}{\code{integer/reactive} Number of choices  selected by default in case of multiple selection. Defaut 10}

\item{labels}{\code{list/reactive} Title / subtitle / message
\itemize{
 \item{"title"}{ : Module title.}
 \item{"no_data "}{ : Printed message if no data.}
 \item{"filter"}{ : Selection filters.}
 \item{"reinitialize"}{ : Reinit button.}
 \item{"validate"}{ :  Validate filter button.}
 \item{"complete_data"}{ :  Complete data button.}
}}
}
\value{
\code{reactiveValues} with filtered data
}
\description{
This function has to be set in the UI part of a shiny 
application. filter_data is a module used to filter a given data.table on 
chosen columns by the user. The server part returns a reactive value containing
the filtered data.table
}
\examples{
\dontrun{

ui = shiny::fluidPage(filter_data_UI(id = "id", titles = TRUE))

server = function(input, output, session) {
  data <- reactiveValues(data = iris)
  shiny::callModule(module = filter_data, id = "id", 
    data = reactive(data$data), 
    columns_to_filter = c("Sepal.Length", "Sepal.Width")
  )
}

shiny::shinyApp(ui = ui, server = server)


ui = shiny::fluidPage(filter_data_UI(id = "id"))

server = function(input, output, session) {
  filtered_data <- shiny::callModule(module = filter_data, id = "id", 
  data = iris,
  columns_to_filter = c("Width" = "Petal.Width", "Length" = "Petal.Length"),
  labels = list(
      title = "Filtres",
      no_data = "Pas de donnÃ©es disponibles", 
      filter = "Filtrer sur les colonnes :", 
      reinitialize = "RÃ©initialisation des filtres", 
      validate = "Filtrer les donnÃ©es",
      complete_data = "Jeu de donnÃ©es total"
   ))
   
   shiny::observe({
       print(filtered_data$data)
   })
}

shiny::shinyApp(ui = ui, server = server)

## Example apps
run_example_app_summary_data()
run_example_app_filter_and_summary_data()
} 

}
