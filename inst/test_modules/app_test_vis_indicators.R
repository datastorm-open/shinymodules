library(shiny)
library(data.table)
library(rAmCharts)
library(DT)
library(pipeR)

# create data
data <- data.table(obs = runif(100, 1, 10))
data[, fit := obs + rnorm(100, 0, 10)]
data[, date := seq(as.POSIXct("2019-10-07 00:00:00 UTC", tz = "UTC"), as.POSIXct("2019-10-11 03:00:00 UTC", tz = "UTC"), by = 60*60)]
data[, by_quali := factor(sample(rep(1:10, 10)))]
data[, by_quanti := runif(100, 1, 20)]

# global vars
col_obs <- "obs"
col_fit <- "fit"
col_date = "date"
indicators <- c("rmse", "mae", "mape", "mape_e")

# ui
ui <- fluidPage(vis_indicators_UI("my_id_1", data, col_obs, col_fit, col_date),
                vis_indicators_UI("my_id_2", data, col_obs, col_fit, col_date))
# server
server <- function(input, output, session) {
  callModule(vis_indicators, "my_id_1", data, col_obs, col_fit, col_date, indicators)
  callModule(vis_indicators, "my_id_2", data, col_obs, col_fit, col_date, indicators)
}
# launcher
shinyApp(ui = ui, server = server)