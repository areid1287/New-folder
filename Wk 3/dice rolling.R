roll_many_die <- function(sides, n) {
  dice <- sample(sides, size = n, replace =TRUE)
  sum(dice)
}
roll_many_die(sides = 1:8, n = 3)
#rolls an 8 sided dice 3 times and sums the results


weighted_rolls <- function() {
  output = sum(sample(1:6, 2, replace = TRUE, prob = c(1, 1, 1, 1, 1, 3)))
  return(output)
}
#prob of rolling a 6 is now 3/8, every other number is 1/8
rolls2 <- replicate(10000, weighted_rolls())
qplot(rolls2, binwidth = 1)