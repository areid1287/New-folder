is_prime <- function (x = 5) {
  n <- 2
  if (x < 2) {
    return(FALSE)
  }
  while (n <= x) {
    if (n == x) {
      return(TRUE)
    } 
    if (x %% n == 0) {
      return(FALSE)
    }
    n <- n + 1
  }
  
}


is_prime1 <- function (x = 9) {
  n <- 2
  if (x < 2) {
    return(FALSE)
  }
  while (n < x) {
    if (x %% n == 0) {
      return(FALSE)
    }
    n <- n + 1
  }
  return(TRUE)
}


is_primeFOR <- function (x = 9) {
  n <- 2
  if (x < 2) {
    return(FALSE)
  }
  for (n in seq(2, x - 1)) {
    if (x == 2) {
      return (TRUE)
    } else if (x %% n == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}



clean <- function(x) {
  ifelse (x == -99 | x == 0 | x == 1, 
          NA,
          x)
}


library(tidyverse) #to export case_when
foo2 <- function(x) {
  case_when(
    x > 2 ~ "a",
    x < 2 & x != 1 ~ "b",
    x == 1 ~ "c",
    TRUE ~ "d"
  )
}
foo2(1)

