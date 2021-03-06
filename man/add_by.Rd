% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monitoring_data.R
\name{add_by}
\alias{add_by}
\title{Format by column before plotting}
\usage{
add_by(data, by_col, col_date, nb_quantiles = NULL, label_no_choice = "None")
}
\arguments{
\item{data}{\code{data.frame} / \code{data.table}. Table containing by or date column.}

\item{by_col}{\code{character}. Either the name of the column to use for aggregation or 
one of ("None", "day", "week", "month", "year").}

\item{col_date}{\code{character}. Column name for date values.}

\item{nb_quantiles}{\code{integer} (NULL). Number of intervals for discretization when by_col is numeric.}

\item{label_no_choice}{\code{character} ("None"). Label for no selection.}
}
\value{
a data.table. If 'by_col' is one of: ("day", "week", "month", "year"), a new column 
is created. If 'by_col' is numeric, it is discretized. Else it is unchanged.
}
\description{
Format by column before plotting
}
\examples{
\dontrun{\donttest{

library(data.table)

obs <- round(runif(100, 1, 10))
fit <- rnorm(100, 1, 5)

data <- data.table(obs = obs, fit = fit, by_var = sample(rep(1:10, 10)))
data <- shinymodules:::add_by(data = data, by_col = "by_var", nb_quantiles = 15)

}}
}
