### Code

```R
# example data in global.R
library(data.table)
# some fake data for monitoring
data_monit <- data.table(obs = runif(100, 1, 10))
data_monit[, fit := obs + rnorm(100, 0, 10)]
data_monit[, date := seq(as.POSIXct("2019-10-07 00:00:00 UTC", tz = "UTC"),
                         as.POSIXct("2019-10-11 03:00:00 UTC", tz = "UTC"), by = 60*60)]

data_monit[, by_quali := factor(sample(rep(1:10, 10)))]
data_monit[, by_quanti := runif(100, 1, 20)]

# ui
ui <- fluidPage(
  monitoring_data_UI(id = "monitoring")
)

# server
shinyServer(function(input, output) {
    # monitoring
    callModule(module = monitoring_data, 
               id = "monitoring", 
               data = reactive(data_monit),
               col_obs = "obs",
               col_fit = "fit",
               col_date = "date"
    )
})
```
