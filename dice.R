library("ggplot2")

roll <- function() {
  die <- 1:6
  weights <- c(1/8, 1/8, 1/8, 1/8, 1/8, 3/8)
  dice <- sample(x = die, size = 2, replace = TRUE, prob = weights)
  sum(dice)
}

rolls <- replicate(10000, roll())
qplot(rolls, binwidth = 1)