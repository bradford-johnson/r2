library(tidyverse)

titlr <- function(p, title, x.axis, y.axis) {
  p + labs(title = title, x = x.axis, y = y.axis)
}

plot <- ggplot(mtcars,aes(x=mpg,y=wt)) +
  geom_point()


titlr(plot,"MPG and Weight", "MPG", "Weight")
