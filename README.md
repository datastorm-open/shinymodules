# shinymodules

This package provides some useful shiny modules to build quickly an ergonomic tool to explore a table.

### Installation

You can install:

-   the latest development version from GitHub with :

``` r
devtools::install_github("datastorm-open/shinymodules")
```

### Demo application

*online*

https://datastorm-demo.shinyapps.io/shinymodules/

*local*

``` r
if(!require(nycflights13)) install.packages("nycflights13")
if(!require(data.table)) install.packages("data.table")
if(!require(esquisse)) install.packages("esquisse")
if(!require(colourpicker)) install.packages("colourpicker")

runApp(system.file("demo_app", package = "shinymodules"))
```

## filter_data

``filter_data`` is a module used to filter a given data.table on chosen columns by the user. The server part returns a reactive value containing the filtered data.table.

![img1](inst/demo_app/www/img/filter_data.png)

```r
?shinymodules::filter_data
# ui.R
shinymodules::filter_data_UI(id = "filter_id"))

# server.R
full_data <- reactive({...})
output_filter <- shiny::callModule(module = shinymodules::filter_data, 
				   id = "filter_id",
			     data = full_data,
				   columns_to_filter = "all")

# then, hafe fun with filter data !
your_filtered_table <- output_filter$data
```

## summary_data

Display an automatically generate dashboard that shows descriptive statistics with `shinymodules::summary_data` : for every columns from the table you selected. The statistics chosen depend on the column's type (character, factor, numeric, date).

![img1](inst/demo_app/www/img/summary_data.png)

```r
?shinymodules::summary_data
# ui.R
shinymodules::summary_data_UI(id = "stat_desc_id", titles = FALSE)

# server.R
data <- shiny::reactive({my_data})
shiny::callModule(module = shinymodules::summary_data, 
		  data = shiny::reactive(data))
		  optional_stats = "all",
		  nb_modal2show = 6, 
	          labels = list(title = "Descriptive statistics",
				num_var = "Variables numériques",
				date_var = "Variables dates",
				factor_var = "Variables facteurs",
				message = "Calcul des indicateurs en cours..."))
```

## show_DT

Show & export table using **DT** package

![img1](inst/demo_app/www/img/show_dt.png)

```r
?shinymodules::show_DT
# ui.R
shinymodules::show_DT_UI(
     id = "iris_module", 
     export = c("csv", "html")
)

# server.R
shiny::callModule(module = show_DT, 
    id = "iris_module", 
    data = reactive(iris), 
    dt = reactive(DT::datatable(iris)), 
    file_name = paste0("Iris_export", format(Sys.time(), format = "%d%m%Y_%H%M%S"))
)
```


## monitoring_data

This module provides tools to monitor models. (Only quantitative regression at moment)

![img1](inst/demo_app/www/img/monitoring_data_1.png)

![img2](inst/demo_app/www/img/monitoring_data_2.png)

![img3](inst/demo_app/www/img/monitoring_data_3.png)

```r
## UI
shinymodules::monitoring_data_UI("my_id")

## SERVER
shiny::callModule(shinymodules::monitoring_data, "my_id", data = reactive(data), 
                  col_obs = col_obs,
                  col_fit = col_fit,
                  col_date = col_date,
                  indicators = indicators)
```
