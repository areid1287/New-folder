clean <- function (x) {
  if (x == -99) {
    x <- NA
  } else if (x == ".") {
    x <- NA 
  } else if (x == "") {
    x <- NA
  } else if (x == "NaN") {
    x <- NA
  } else {
      x <- x
    }
  return(x)
}

clean("")


clean2 <- function(x) {
  if (x == -99 || 
      x == "." ||
      x == ""  ||
      x == "NaN") {
    x <- NA
  }
return(x)
}

clean22 <- function(x) {
  if (is.null(x)) {
    stop("x is NULL")
    } else if (x == -99 || 
               x == "." ||
               x == "" ||
               x == "NaN") {
      x <- NA
      return(x)
      } else {
      return(x)
    }
}
clean22(NULL)


clean3 <- function(x) {
  if (x == -99) {
    x <- NA
  }
  return(x)
  if (x == ".") {
    x <- NA
  }
  return(x)
  if (x == "NaN") {
    x <- NA
  }
  return(x)
return(x)
}