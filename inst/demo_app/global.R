library(shiny)
library(shinymodules)
library(data.table)
library(nycflights13)
library(esquisse)
library(colourpicker)

data_flights <- data.table::data.table(copy(nycflights13::flights))

# set limits for esquisse graphics module
limit_nrows_esquisse <- 50000


# some fake data for monitoring
data_monit <- data.table(obs = runif(100, 1, 10))
data_monit[, fit := obs + rnorm(100, 0, 10)]
data_monit[, date := seq(as.POSIXct("2019-10-07 00:00:00 UTC", tz = "UTC"),
                         as.POSIXct("2019-10-11 03:00:00 UTC", tz = "UTC"), by = 60*60)]

data_monit[, by_quali := factor(sample(rep(1:10, 10)))]
data_monit[, by_quanti := runif(100, 1, 20)]

