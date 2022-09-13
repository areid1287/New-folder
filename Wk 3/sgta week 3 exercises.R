#Exercises ~ Week 3 SGTA
#1

x <- runif(2, min = 0, max = 1)
if (any(x > 0.5) == TRUE) {
  glue::glue("At least one head has been flipped")
} else {
  glue::glue("No Heads")
}

x <- runif(2, min = 0, max = 1)
if (all(x <= 0.5) == TRUE) {
  glue::glue("Both Tails")
} else if (all(x > 0.5) == TRUE) {
  glue::glue("Both Heads")
} else {
  glue::glue("One Head and One Tail")
}

x <- runif(2, min = 0, max = 1)
if (x[1] < 0.65 && x[2] < 0.5) {
  glue::glue("Both Tails")
} else if (x[1] > 0.65 && x[2] > 0.5) {
  glue::glue("Both Heads")
} else {
  glue::glue("One Head and One Tail")
}

#2
fibonacci_numbers <- function (n = 30) {
  fibonacci_vec <- double(length = n)
  for (i in seq_along(1:n)) {
    if (i == 1) {
      fibonacci_vec[[i]] <- 0
    } else if (i == 2) {
      fibonacci_vec[i] <- 1
    } else {
      fibonacci_vec[[i]] <- fibonacci_vec[[i - 1]] + fibonacci_vec[[i - 2]]
    }
  }
  return(fibonacci_vec)
}

fibonacci_number <- function (k = 3) {
  fibonacci_vec <- c(0, 1)
  for (i in seq_along(1:(k - 1))) {
    x <- sum(fibonacci_vec)  
    fibonacci_vec[[1]] <- fibonacci_vec[[2]]
    fibonacci_vec[[2]] <- x
    }
  return(fibonacci_vec[2])
}

#3
countdown <- function(num = 7 ) {
  for (i in seq_along(0:7)) {
    if (i != num + 1) {
      print((num:0)[i])
    } else print(glue::glue("Boom!"))
    Sys.sleep(1)
  }
}
countdown()
    
#4
Snakes_eyes <- function() {
  counter <- 1
  while (sum(sample(1:6, 2, replace = TRUE)) != 2) {
    counter <- counter + 1 
  }
  return(counter)
}

#5
eEstimate <- function (num = 10) {
    ee <- c(1)
    for (i in seq_along(1:(num-1))) {
      ee <- ee + 1/factorial(i)
    }
  return(ee)
}

Estimate <- function () {
  counter <- 0
  sum <- c(0)
  while (1/factorial(counter) > (1 * 10^-10) || counter == 15) {
    sum <- sum + 1/factorial(counter)
    counter <- counter + 1
  }
  return(sum)
}
Estimate()

Estimates <- function(runs = 10, tolerence = 1 * 10^-10) {
  counter <- 0 
  terms <- c(0) 
  while (1/factorial(counter) > tolerence && counter < runs - 1) {
    terms <- append(terms, 1/factorial(counter))
    counter <- counter + 1
  }
  return(terms)
}

Estimates()
  
  