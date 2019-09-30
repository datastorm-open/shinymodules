context("get_heatmap")

data(USArrests, "VADeaths")
USArrests <- USArrests [1:10,]
titanic <- data.table(datasets::Titanic)
test_that("get_Heatmapdt", {
  dt <- get_Heatmapdt(titanic[, list(Class, Age)])
  expect_true("datatables" %in% class(dt))
  expect_true("htmlwidget" %in% class(dt))
  
})

test_that("amHeatmap", {
  hmap <- suppressWarnings(
    amHeatmap(USArrests, nbclasses=10, col=c("#00FF00","#FF00FF","#0000FF"),
              labels = TRUE, cex = 10, main = "My title", xLabelsRotation = 45,
              colorby="all",legend = TRUE, rownames = rownames(USArrests)))
  
  expect_true("ramcharts_base" %in% class(hmap))
  expect_true("htmlwidget" %in% class(hmap))
  
})
