context("get_filtered_data")

airquality <- data.table::copy(data.table(datasets::airquality))
iris <- data.table::copy(data.table(iris))
test_that(".filterDataTable", {
  filter <- "Ozone < 40 & Solar.R < 200"
  filter <- list()
  filter[[1]] <- list(column = "Ozone", fun = "<", values = 40)
  filter[[2]] <- list(column = "Solar.R", fun = "<", values = 200)
  dtfiltered <- .filterDataTable(airquality, filter)
  expect_true("data.table" %in% class(dtfiltered))
  expect_true(nrow(dtfiltered) < nrow(airquality))
  
  expect_true(all(dtfiltered$Ozone < 40))
  expect_true(all(dtfiltered$Solar.R < 200))
  
  expect_true(any(airquality$Ozone < 40))
  expect_true(any(airquality$Solar.R < 200))
})
