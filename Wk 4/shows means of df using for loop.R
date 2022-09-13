show_mean <- function (df = iris) {
  output <- vector("list", length = ncol(df))
  for (i in seq_along(df)) {
    output[[i]] <- glue::glue("{colnames(df)[[i]]}: {mean(df[[i]])}")
  }
  return(output)
}
show_mean()