
library(readr)
A1_Ex3_signif <- read_csv("Macquarie Uni/2022 Sem 2/Stat/Working Directory/A1_Ex3_signif.csv")
View(A1_Ex3_signif)

library(readr)
A1_Ex3_not_signif <- read_csv("Macquarie Uni/2022 Sem 2/Stat/Working Directory/A1_Ex3_not_signif.csv")
View(A1_Ex3_not_signif)


hypothesis <- function(x) {
  glue::glue("Null Hypothesis: β = 0 
              vs Alternative Hypothesis: β ≠ 0")
}
hypothesis()

  assumptions <- function(file_name = "A1_Ex3_signif.csv") {
  read_csv(file_name)
  colnames(data) <- c("X", "Y")
  lin_mod <- lm(Y ~ X, data)
  fittedy <- fitted(lin_mod)
  res <- resid(lin_mod)
  df <- data.frame(X = res, Y = fittedy)
  p1 <- ggplot(data, aes(x = X, y = Y)) + 
    geom_point() + 
    ggtitle("Testing for Linear Relationship between X and Y")
  p2 <- ggplot(df, aes(x = X, y = Y)) + 
    geom_point() + 
    ylab("Fitted Y Values") +
    xlab("Residuals") +
    ggtitle("Residuals vs Fitted Values")
  p3 <- ggplot(df, aes(x = res)) +
    geom_histogram( bins = 25) +
    xlab("Residuals") +
    ggtitle("Regression Residuals Histogram")
  # install.packages("patchwork")
  # library(patchwork)
  (p1 / p2) | p3
}

assumptions()
