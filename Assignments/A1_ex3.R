# Author: Alana Reid 
# Finished on: 26/08/2022
# Purpose: Runs a complete hypothesis test for linear regression between X and Y, 
# and determines whether or not there exists a significant relationship.


# Include required packages (and install if they don't exist)
req_pack <- c("readr", "patchwork", "tidyverse")

for (package in req_pack) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package);
    library(package, character.only = TRUE)
  }
}

# Create a function that prints hypothesis for regression test
hypothesis <- function(file_name = "A1_Ex3_signif.csv") {
  data <- read_csv(file_name, show_col_types = FALSE)
  cat("Null Hypothesis: β = 0 VS Alternate Hypothesis: β ≠ 0\r\n")
} 

# Create a function that displays graphs testing appropriateness of assumptions
assumptions <- function(file_name = "A1_Ex3_signif.csv") {
  # Read csv file into a dataframe
  data <- read_csv(file_name, show_col_types = FALSE)
  
  # rename columns of data frame to X and Y
  colnames(data) <- c("X", "Y")
  lin_mod <- lm(Y ~ X, data)
  res <- resid(lin_mod)
  df <- data.frame(X = res, Y = fitted(lin_mod))
 
  # Create scatter plot testing linearity between X and Y
   p1 <- ggplot(data, aes(x = X, y = Y)) + 
    geom_point() + 
    geom_smooth(method = "lm", formula = y ~ x) +
    ggtitle("Scatterplot of X vs Y", 
            subtitle = "Assumption: follows simple linear relationship")
  
  # Create scatter plot testing independence in residuals and fitted values
  p2 <- ggplot(df, aes(x = X, y = Y)) + 
    geom_point() + 
    geom_hline(yintercept = 0) +
    ylab("Fitted Y Values") +
    xlab("Residuals") +
    ggtitle("Residuals vs Fitted Values",
            subtitle = "Assumption: residuals independent of fitted values")
  
  # Create histogram testing normality of variance
  p3 <- ggplot(df, aes(x = res)) +
    geom_histogram( bins = 25) +
    xlab("Residuals") +
    ggtitle("Regression Residuals Histogram",
            subtitle = "Assumption: variance is normally distributed")
  
  # Display all three graphs in one plot
  (p1 / p2) | p3
}

# Assign fit to a function that computes and returns linear regression
fit <- function(file_name = "A1_Ex3_signif.csv") {
          data <- read_csv(file_name, show_col_types = FALSE)
          colnames(data) <- c("X", "Y")
          mylm <- lm(Y ~ X, data)
          lin_mod <- summary(mylm)
          estimate <- lin_mod$coefficients["X", "Estimate"]
          
          # Print summary of results of Linear Regression test
          cat("Linear Regression Summary:",
              "\nEstimated Slope ˆβ .......... ", estimate,
              "\nConfidence Interval for β ... ", "(", estimate - lin_mod$coefficients["X", "Std. Error"], ", ",
                                             estimate + lin_mod$coefficients["X", "Std. Error"], ")",
              "\nt-value ..................... ", lin_mod$coefficients["X", "t value"],
              "\nDegrees of Freedom .......... ", mylm$df,
              "\np-value ..................... ", lin_mod$coefficients["X", "Pr(>|t|)"],
            sep = ""
            )
          
          # Add class "mylm" to linear model 
          class(mylm) <- c("lm", "mylm")
          return(mylm)
}


decision <- function(x, ...) {
  UseMethod("decision")
}

# Implement decision method for mylm depending on p-value from results of test
decision.mylm <- function(obj) {
  if (summary(obj)$coefficients["X", "Pr(>|t|)"] < 0.05) {
    cat("\n\n\nReject Null Hypothesis: β = 0")
  } else {
    cat("\n\n\nDO NOT Reject Null Hypothesis: β = 0")
  }
}


conclusion <- function(x, ...) {
  UseMethod("conclusion")
}

# Implement conclusion method for mylm depending on p-value from results of test
conclusion.mylm <- function(obj) {
  if (summary(obj)$coefficients["X", "Pr(>|t|)"] < 0.05) {
    cat("\n\nThere is supporting evidence against the Null Hypothesis β = 0, demonstrating statistically significant results. 
Therefore, we can conlcude there is relevent regression between X and Y.")
  } else {
    cat("\n\nThere is insufficient evidence against the Null Hypothesis β = 0, demonstrating statistically insignificant results.
Therefore, we cannot conlcude that there exists relevent regression between X and Y.")
  }
}

# Compile all functions for regression testing into one singular function
mytest <- function(file_name = "A1_Ex3_signif.csv") {
  
  # Stop function if certain requirements of csv file are not met
  if(!file.exists(file_name)) stop("File doesn't exist!")
  data <- read_csv(file_name, show_col_types = FALSE)
  if(any(sapply(data, class) != "numeric")) {
    stop("Invalid data types, not numeric")
  }
  if (ncol(data) != 2) {
    stop("dataframe must consist of only 2 variables, X and Y")
  }
  
  # Print title at beginning of test
  cat("\n\t\tLinear Model Test\n")
  
  # Run functions for linear testing in order
  hypothesis(file_name)
  plot(assumptions(file_name))
  fit1 <- fit(file_name)
  decision(fit1)
  conclusion(fit1)
  
  # For display purposes  
  cat("\n\n")
}

# Run linear model test
mytest()

