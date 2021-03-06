library(shiny)
library(data.table)
library(DT)


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
ui <- fluidPage(
  monitoring_data_UI("my_id_1")
)
# server
server <- function(input, output, session) {
  callModule(monitoring_data, "my_id_1", 
             data = reactive(data), 
             col_obs = col_obs, 
             col_fit = col_fit, 
             col_date = col_date, 
             indicators = indicators
  )
}
# launcher
shinyApp(ui = ui, server = server)