output <- vector("double", length = ncol(mtcars))
for (i in seq_along(mtcars)) {
  output[[i]] <- sum(mtcars[[i]])/nrow(mtcars)
  output
}

output <- vector("double", length = ncol(iris))
for (i in seq_along(iris)) {
  output[[i]] <- length(c(unique(iris[[i]])))
  output
}

rand_num <- function (means = c(-10, 0, 10, 100), n = 10) {
  output <- data.frame(matrix(0, n, length(means)))
  colnames(output) <- means
  for (i in seq_along(1:length(means))) {
    output[[i]] <- rnorm(n, mean = means[i])
  }
  return(output)
}
rand_num(means = c(-10, -5, 0, 5, 10, 50, 100))


