test_that("right row from data is selected by extract_tt_data_row", {

  library(ggplot2)
  data <- data.frame(x = c(1,2,5,6,8),
                     y = c(3,6,2,8,7),
                     vx = c(1,1.5,0.8,0.5,1.3),
                     vy = c(0.2,1.3,1.7,0.8,1.4))
  plot <- ggplot() +
    geom_segment(data = data,
                 mapping = aes(x = x, y = y, xend = x + vx, yend = y + vy),
                 size = 2, color = "blue")

  plot_data <- ggplot_build(plot)[["data"]][[1]]
  plot_type = "geom_segment"
  tt_range = 1

  hv <- list()
  hv[["x"]] <- 5
  hv[["y"]] <- 3
  result1 <- extract_tt_data_row(hv, plot_data, data, plot_type = plot_type, tt_range = tt_range)
  expect_equal(result1, data[3, ])

  hv[["x"]] <- 8
  hv[["y"]] <- 2
  result2 <- extract_tt_data_row(hv, plot_data, data, plot_type = plot_type, tt_range = tt_range)
  expect_equal(nrow(result2), 0)

  hv[["x"]] <- 8
  hv[["y"]] <- 7
  result3 <- extract_tt_data_row(hv, plot_data, data, plot_type = plot_type, tt_range = tt_range)
  expect_equal(result3, data[5, ])
})
