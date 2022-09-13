mylm <- list(name = "My Linear Model", p=0.02)
class(mylm) <- "mylm"

print.mylm <- function(obj) {
  cat("My Name is:", obj$name)
}


decision <- function(x) {
  UseMethod("decision", x)
}

# Implement decision method for mylm
decision.mylm <- function(obj) {
  if (obj$p < 0.05) {
    cat("I reject you")
  } else {
    cat("I don't reject you")
  }
}

decision.numeric <- function(x) {
  cat(x)
}


print(mylm)

decision(mylm)
decision(2)
