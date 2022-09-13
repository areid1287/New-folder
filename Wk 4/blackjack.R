for (i in seq_along(1:52)) {
  if (blackjack$face[[i]] %in% c("jack", "queen", "king")) {
    blackjack$value[[i]] <- 10
  }
}

## started trying to do blackjack, too hard gave up lol
blackjack_func <- function() {
  draw1 <- blackjack[sample(nrow(blackjack), 2), ]
  draw1 <- sum(draw1$value)
   if(draw1 < 21) {
     draw2 <- blackjack[sample(nrow(blackjack), 1), ]
     
    
  }
}