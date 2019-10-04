context("get_plots")

mtcars <- data.table::copy(data.table(datasets::mtcars))
iris <- data.table::copy(data.table(datasets::iris))
flights <- data.table(nycflights13::flights)

test_that(".plotScatterplot", {
  
  p1 <- .plotScatterplot(mtcars[, list(mpg, cyl)], type = "line", js = T)
  p2 <- .plotScatterplot(mtcars[, list(mpg, drat)], type = "point", js = T)
  p3 <- .plotScatterplot(mtcars[, list(drat)], type = "hist", js = T)
  p4 <- .plotScatterplot(mtcars[, list(drat, wt)], type = "point", js = F)
  expect_true("ramcharts_base" %in% class(p1))
  expect_true("htmlwidget" %in% class(p2))
  expect_true("AmChart" %in% class(p3))
  expect_true("ggplot" %in% class(p4))
  
})


test_that(".plotExploratory", {

  p1 <- .plotExploratory(iris[, list(Petal.Width, Species)], type = "barplot",
                         palette_ggplot = "RdYlGn")
  p2 <- .plotExploratory(iris[, list(Sepal.Width)], type = "boxplot",
                         palette_ggplot = "RdYlGn")
  p3 <- .plotExploratory(flights[, list(time_hour, distance)], type = "timeseries",
  aggregation = "Average", js = F, palette_ggplot = "RdYlGn")
  
  expect_true("ramcharts_base" %in% class(p1))
  expect_true("AmChart" %in% class(p2))
  expect_true("ggplot" %in% class(p3))

})
