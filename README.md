# shinymodules

This package provides shiny modules to build quickly an ergonomic tool to explore a table.

## Features

- Dynamically filter on your table with `shinymodules::filter_data` : `filter_data` displays a `selectInput` that let the user choose one of several table's columns. For each selected columns, a shiny input will be displayed to apply a filter on this column. This shiny input depends on the column type (character, factor, numeric, date).
- Display an automatically generate dashboard that shows descriptive statistics with `shinymodules::show_data` : for every columns from the table you selected. The statistics chosen depend on the column's type (character, factor, numeric, date) 
- (Deprecated) Display a module to build dynamically your chart with `shinymodules::visualize_data`. We recommand you to use the package esquisseR instead. 

## Examples

### `filter_data`

```r
## UI
shinymodules::filter_dataUI(id = "filter_id",
			    labels = list(title = "Filtres",
					  no_data = "Pas de données disponibles",
					  filter = "Filter sur les colonnes",
					  reinitialize = "Réinitialisation des filtres",
					  validate = "Filtrer les données"))

## SERVER
input_filter <- reactive({...})
output_filter <- shiny::callModule(module = shinymodules::filter_data, 
				   id = "filter_id",
			           data = input_filter,
				   columns_to_filter = "all")
your_filtered_table <- output_filter$data
```

### `show_data`

```r
## UI
shinymodules::show_dataUI(id = "stat_desc_id", titles = FALSE)

## SERVER
input_show <- shiny::reactiveValues(data = NULL)
shiny::callModule(module = shinymodules::show_data, 
		  data = shiny::reactive(input_show$data),
		  optional_stats = "all",
		  nb_modal2show = 6, 
	          labels = list(title = "Descriptive statistics",
				num_var = "Variables numériques",
				date_var = "Variables dates",
				factor_var = "Variables facteurs",
				message = "Calcul des indicateurs en cours..."))
```


## Questions 

- Why labels are passed in UI for filter and in server for show ? 
- Why/does we need a structure sth-reactive$data ?
