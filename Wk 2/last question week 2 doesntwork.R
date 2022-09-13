random_func <- function(N, rx1 = c(0, 10), rx2 = c(2, 12), beta = c(1, 5, 7), sigma = 1) {
  e  <- rnorm(N, 0, sigma)
  x1 <- runif(N, min(rx1), max(rx1))
  x2 <- runif(N, min(rx2), max(rx2))
  X  <- matrix(data = c(rep(1, N), c(x1), c(x2)), 
               nrow = N, 
               ncol = 3,
               byrow = FALSE)
  Y <- sweep(X, MARGIN = 2, STATS = beta, FUN = "*") + e
  y_dat <- rowSums(Y)
  par(mfrow = c(1,2))
  plot(y_dat, x1, col = c(35, "#0AFF12", 95))
  plot(y_dat, x2, col = c(38, 20, 30))
}
#random_func(N = 1000, rx1 = 1:2, rx2 = 10:11, sigma = 14)
random_func(N = 100)