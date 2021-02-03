# shinymodules

This package provides shiny modules to build quickly an ergonomic tool to explore a table.


## `filter_data`

filter_data is a module used to filter a given data.table on chosen columns by the user. The server part returns a reactive value containing the filtered data.table.

![img1](inst/doc_img/filter_data.png)

```r
## UI
shinymodules::filter_data_UI(id = "filter_id",
			    labels = list(title = "Filtres",
					  no_data = "Pas de données disponibles",
					  filter = "Filter sur les colonnes",
					  reinitialize = "Réinitialisation des filtres",
					  validate = "Filtrer les données",
            complete_data = "Jeu de données total"))

## SERVER
input_filter <- reactive({...})
output_filter <- shiny::callModule(module = shinymodules::filter_data, 
				   id = "filter_id",
			           data = input_filter,
				   columns_to_filter = "all")
your_filtered_table <- output_filter$data
```

## `show_DT`

Show & export table using DT

![img1](inst/doc_img/show_dt.png)

```r
## UI
shinymodules::show_DT_UI("iris_module")

## SERVER
shiny::callModule(show_DT, "iris_module", reactive(iris), reactive(DT::datatable(iris)), 
   paste0("Iris_export", format(Sys.time(), format = "...")))
```

## `summary_data`

Display an automatically generate dashboard that shows descriptive statistics with `shinymodules::summary_data` : for every columns from the table you selected. The statistics chosen depend on the column's type (character, factor, numeric, date).

![img1](inst/doc_img/summary_data.png)

```r
## UI
shinymodules::summary_data_UI(id = "stat_desc_id", titles = FALSE)

## SERVER
input_show <- shiny::reactiveValues(data = NULL)
shiny::callModule(module = shinymodules::summary_data, 
		  data = shiny::reactive(input_show$data),
		  optional_stats = "all",
		  nb_modal2show = 6, 
	          labels = list(title = "Descriptive statistics",
				num_var = "Variables numériques",
				date_var = "Variables dates",
				factor_var = "Variables facteurs",
				message = "Calcul des indicateurs en cours..."))
```

## `monitoring_data`

This module provides tools to monitor models. (Only quantitative regression at moment)

![img1](inst/doc_img/monitoring_data_1.png)

![img2](inst/doc_img/monitoring_data_2.png)

![img3](inst/doc_img/monitoring_data_3.png)

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



