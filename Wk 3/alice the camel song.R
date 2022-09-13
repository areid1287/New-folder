for (i in c("five", "four", "three", "two", "one", "no")) {
  for(g in seq_along(1:3)) {
    if (i != "one") {
      print(glue::glue("Alice the camel has {i} humps"))
    } else {
      print(glue::glue("Alice the camel has {i} hump"))
    }
  }
  if (i != "no") {
    print(glue::glue("So go Alice go"))
  } else {
    print(glue::glue("Now Alice is a horse"))
  }
}
#Alice the camel song !