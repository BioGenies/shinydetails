library(ggplot2)
library(shiny)

test_that("Tabset panel server generates plots correctly.", {
  plot_out = reactive({
    ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, col = Species)) +
      geom_point()
  })
  shiny::testServer(tabsetPanel_SERVER, {
    output[["plot"]]
    }, args = list(plot_out = plot_out))
})

test_that("Tabset panel server generates table correctly.", {
  table_out = reactive({
    iris
  })
  shiny::testServer(tabsetPanel_SERVER, {
    output[["data"]]
  }, args = list(table_out = table_out))
})
