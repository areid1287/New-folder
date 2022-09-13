cards <- function(name = "Sebastian", 
                  age = "19", 
                  lang = c("Danish", "English"), 
                  cols = c("blue", "pink", "i dont know")) {
  data <- list(name = name, 
               age = age, 
               lang = lang,
               cols = cols
  )
  cat(glue("{name}, {age}, can speak {glue_collapse(lang,  sep = ', ', last = ' and ')}, and likes {glue_collapse(cols,  sep = ', ', last = ' and ')}"), "\n")
  data
}

cards()

cards2 <- function(name = "Sebastian", 
                   age = 19, 
                   lang = NULL, 
                   cols = NULL) {
  if (is.null(lang) & is.null(cols)) {
    cat(glue("{name}, {age}, cannot speak any languages, and has no favourite colours"), "\n")
  } else if (is.null(lang)) {
    cat(glue("{name}, {age}, cannot speak any languages, and likes {glue_collapse(cols,  sep = ', ', last = ' and ')}"), "\n")
  } else if (is.null(cols)) {
    cat(glue("{name}, {age}, can speak {glue_collapse(lang,  sep = ', ', last = ' and ')}, and has no favourite colours"), "\n")
  } else {
    cat(glue("{name}, {age}, can speak {glue_collapse(lang,  sep = ', ', last = ' and ')}, and likes {glue_collapse(cols,  sep = ', ', last = ' and ')}"), "\n")
  }
  data <- list(name = name, 
               age = age, 
               lang = lang,
               cols = cols
  )
  data
}

cards2(lang = c("Danish", "Mandarin", "French"))

cards3 <- function(name = "Sebastian", 
                   age = 19, 
                   lang = NULL, 
                   cols = NULL) {
  data <- list(name = name, 
               age = age, 
               lang = lang,
               cols = cols
  )
  class(data) <- c("card")
  message("Card created")
  data
}

cards3()
decision <- function(x, ...) {
  UseMethod("decision")
}

print.card <- function(obj) {
 print
}

