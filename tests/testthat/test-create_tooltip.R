library(dplyr)
library(HaDeX)

test_that("Displayed data is produced correctly.", {

  data = data.frame(mtcars,
                    car = rownames(mtcars))

  ggplot(data, aes(x = car, y = mpg, label = mpg)) +
    geom_segment(aes(y = 0,
                     x = car,
                     yend = mpg,
                     xend = car),
                 color = "black")

})




