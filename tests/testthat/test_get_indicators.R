context(".get_indicators")

airquality <- data.table::copy(data.table(datasets::airquality))
iris <- data.table::copy(data.table(iris))

test_that(".get_indicators", {
  indic <- .get_indicators(airquality, var = "Ozone", 
                 optional_stats  =  c("min", "var", "max"))
  testthat::expect_true("data.table" %in% class(indic))
  testthat::expect_true(all(colnames(indic) %in% c(
    "pct_zero", "pct_NA", "mean", "median", "sd", "min", "var", "max")))
})

test_that(".get_stat_indicators", {
  stats_num <- .get_stat_indicators(data = airquality, 
                                    vars = colnames(airquality),
                                    optional_stats = "all", 
                                    keep_dataframe = FALSE, 
                                    keep_datatable = TRUE)
  expect_true("datatables" %in% class(stats_num))
  expect_true("htmlwidget" %in% class(stats_num))
  
  stats_num <- .get_stat_indicators(data = airquality, 
                                    vars = colnames(airquality),
                                    optional_stats = "all", 
                                    keep_dataframe = TRUE, 
                                    keep_datatable = FALSE)
  expect_true("data.frame" %in% class(stats_num))
  
  stats_num <- .get_stat_indicators(data = airquality, 
                                    vars = colnames(airquality),
                                    optional_stats = "all", 
                                    keep_dataframe = TRUE, 
                                    keep_datatable = TRUE)
  expect_true(is.list(stats_num))
  expect_true(length(stats_num) == 2)
  
})

test_that(".get_factor_indicators", {
  stats_fact <- .get_factor_indicators(data = iris, 
                                       fact_vars = "Species", 
                                       optional_stats = "all",
                                       nb_modal2show = 3, 
                                       keep_dataframe = FALSE, 
                                       keep_datatable = TRUE)
  expect_true("datatables" %in% class(stats_fact))
  expect_true("htmlwidget" %in% class(stats_fact))
  
  stats_fact <- .get_factor_indicators(data = iris, 
                                       fact_vars = "Species", 
                                       optional_stats = "all",
                                       nb_modal2show = 3, 
                                       keep_dataframe = TRUE, 
                                       keep_datatable = FALSE)
  expect_true("data.frame" %in% class(stats_fact))
  
  stats_fact <- .get_factor_indicators(data = iris, 
                                       fact_vars = "Species", 
                                       optional_stats = "all",
                                       nb_modal2show = 3, 
                                       keep_dataframe = TRUE, 
                                       keep_datatable = TRUE)
  expect_true(is.list(stats_fact))
  expect_true(length(stats_fact) == 2)
  
})

test_that("get_summary_data_dt", {
  dt_num_fact <- suppressWarnings(
    get_summary_data_dt(airquality, optional_stats = "all", nb_modal2show = 3, keep_dataframe = FALSE))
  expect_true("list" %in% class(dt_num_fact))
  expect_true("datatables" %in% class(dt_num_fact$dt_num))
  expect_true("htmlwidget" %in% class(dt_num_fact$dt_num))
})
