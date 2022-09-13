ex_3 <- function(x, k, col = c("red", "blue")) {
  y1 <- x^k
  y2 <- x^(k+1)
  plot(x, y1, type = "l", col = col[1])
  lines(x, y2, col = col[2])
}
ex_3(1:100, 2, c("green", "purple"))



ex_3 <- function(x, k, col = c("red", "blue")) {
  y1 <- x^k
  y2 <- x^(k+1)
  plot(x, y1,
       type = "l",
       col = col[1],
       main = "Exercise 3 Comparative Line Graphs", 
       ylim = c(0, (tail(x, n = 1))^(k + 1)))
  lines(x, y2, col = col[2])
  legend(83.7, 2000, legend = c("x^k", "x^(k+1)"), 
         col = c(col[1], col[2]), 
         lty = 1:1,
         cex = 1)
}


ex_3 <- function(x, k, col = c("red", "blue")) {
  y1 <- x^k
  y2 <- x^(k+1)
  plot(x, y1, type = "l", col = col[1], main = "Exercise 3 Comparative Line Graphs",
             ylim = c(0, (tail(x, n = 1))^(k + 1)))
 lines(x, y2, col = col[2])
 legend("topleft", inset = 0.03, legend = c("x^k", "x^(k+1)"), 
               col = c(col[1], col[2]), 
               lty = 1:1,
               cex = 1.2)
}
ex_3(10:11, 4)



