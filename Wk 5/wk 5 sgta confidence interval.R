conf_int_func <- function(ssize = 50, CI = 0.95) {
  func <- function(.x) {
    tab <- tibble(data = rnorm(2000, 1.7, 0.1))
    sample <- sample_n(tab, size = ssize)
    est_mean <- mean(sample$data)
    se <-  qnorm((1-CI)/2, lower.tail = FALSE) * (0.1/sqrt(ssize))
    test <- data.frame(lower = est_mean - se, upper = est_mean + se, 
                       pop_mean = if(1.7 > est_mean - se & 1.7 < est_mean + se) {
                         "Yes"
                       } else ("No")
    )
    test
  }
  data1 <- map_dfr(1:100, func)
  ggplot(data1, aes(colour = pop_mean)) +
    geom_segment(aes(x = lower, xend = upper, y = 1:100, yend = 1:100)) +
    ylab("run") +
    xlab("CI") +
    ggtitle(paste(CI*100, "% CI, n = 50", sep = "")) +
    geom_vline(xintercept = 1.7)
}
conf_int_func()



