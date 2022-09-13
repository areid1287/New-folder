Nursery <-function (num = 10, structure = "bed") {
            words <- c(num:1)
            for (i in (1:num))
              if (i != num) {
                print(glue::glue("There were {words[i]} in the {structure}
                                 And the little one said,
                                 “Roll over! Roll over!”
                                 So they all rolled over and
                                 one fell out"
                                 
                                 ))
              } else {
                print(glue::glue("There was 1 in the {structure}
                                 And the little one said,
                                “Alone at last!”
                                “Good Night!”"
                                
                                  ))
              }
          }
Nursery(22, "piano")
