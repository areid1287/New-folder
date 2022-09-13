# black jack
# game class
#   start method (deals to player, and dealer)
#   deck attribute
#     Contains card class (or list) with suit and value
#
# player class (includes dealer as they are pretty much the same)
#   attribute hand
#
#
#
# print two options deal or hold
# get user decision using a 1 or 2 with as.integer(readline())
# check if bust
# if dealer has higher stop and say you lose
# else pick up card
#
# if tie say player loses (ignore blackjack)

Deck <- list()
Suits <- c("Spades", "Clubs", "Diamonds", "Hearts")
for (i in seq_along(Suits)) {
  for (j in seq_along(1:13)) {
    Deck[[j]] <- Suits[i]
  }
}

Deck <- list()
Suits <- c("Spades", "Clubs", "Diamonds", "Hearts")
for (i in 0:51) {
  Deck[[i + 1]] <- list(Suit = Suits[i %% 4 + 1],
                        Value = i %% 13 + 1)
  class(Deck[[i + 1]]) <- "Card"
}

print.Card <- function(x, ...) {
  Value <- dplyr::case_when(
    x$Value == 1 ~ "Ace",
    x$Value == 11 ~ "Jack",
    x$Value == 12 ~ "Queen",
    x$Value == 13 ~ "King",
    TRUE ~ as.character(x$Value)
  )
  print(glue::glue("{Value} of {x$Suit}"))
}

for (k in 1:52) {
  print(Deck[[k]])
}

shuffle <- function(x, ...) {
  UseMethod("shuffle")
}

shuffle.game <- function(game, ...) {
  shuffled_deck <- sample(Deck, 52, replace = FALSE)
  game$Deck = shuffled_deck
  return(game)
}

getValue <- function(x, ...) {
  UseMethod("getValue")
}

getValue.Card <- function(x, ...) {
  if (x$Value > 10) {
    return(10)
  } else if (x$Value == 1) {
    return(c(1, 11))
  } else {
    return(x$Value)
  }
}

# Create new game, consisting of a Deck, Players (multiple) and the Dealer (singular)
game <- list(Deck = Deck,
             Players = list(list()),
             Dealer = list())

# Assign class of game to "game"
class(game) <- "game"

start <- function(x, ...) {
  UseMethod("start")
}

start.game <- function(game, nplayers) {
  # Shuffle Deck
  game <- shuffle(game)
  
  # Deal two cards to dealer, then remove these cards from game deck
  game$Dealer <- sample(game$Deck, 2, replace = FALSE)
  game$Deck <- game$Deck[!(game$Deck %in% game$Dealer)]
  
  # Deal two cards to each player in the game, then remove cards from game deck
  for (i in 1:nplayers) {
    hand <- sample(game$Deck, 2, replace = FALSE)
    game$Players[[i]] <- hand
    game$Deck <- game$Deck[!(game$Deck %in% hand)]
  }
  return(game)
}

run1 <- start(game, 2)

# Redirects object to appropriate function
deal <- function(x, ...) {
  UseMethod("deal")
}

deal.game <- function(game, ..., player, dealer = FALSE) {
  newCard <- sample(game$Deck, 1, replace = FALSE)
  game$Deck <- game$Deck[!(game$Deck %in% newCard)]
  if (dealer) {
    DealerHand <- game$Dealer
    game$Dealer <- c(DealerHand, newCard)
  } else {
    PlayerHand <- game$Players[[player]]
    game$Players[[player]] <- c(PlayerHand, newCard)
  }
  return(game)
}

GetChoice <- function(game = run1, player = 1) {
  cat("\f")
  PlayerHand = game$Players[[player]]
  print(glue::glue("Player {player}'s turn.
                   Here is your current hand:"))
  print(PlayerHand)
  Score <- 0
  for (card in PlayerHand) {
    Score <- getValue(card) + Score
    
  }
  if (any(Score > 21)) {
    Score <- min(Score)
  } else {
    Score <- max(Score)
  }
  print(glue::glue("You're current score is {Score}"))
  if (Score > 21) {
    game$Players[[player]] <- append(game$Players[[player]], list(Score = Score))
    cat("You Lose :(")
    Sys.sleep(1.5)
    return(game)
    # Add function END GAME
  }
  cat("Would you like to hit again? Type 'Yes' to Hit or 'No' to Stand")
  if (readline() == "Yes") {
    # Add Deal Again Function
    game <- deal(game, player = player)
    game <- GetChoice(game, player)
  } else {
    game$Players[[player]] <- append(game$Players[[player]], list(Score = Score))
  }
  return(game)
}

DealerTurn <- function(game) {
  cat("\f")
  DealerHand <- game$Dealer
  print(glue::glue("Dealers turn.
                   Here is the Dealers hand:"))
  print(DealerHand)
  Score <- 0
  for (card in DealerHand) {
    Score <- getValue(card) + Score
  }
  if (any(Score > 21)) {
    Score <- min(Score)
  } else {
    Score <- max(Score)
  }
  print(glue::glue("Dealer's score is {Score}"))
  if (Score >= 17) {
    game$Dealer <- append(game$Dealer, list(Score = Score))
    if (Score > 21) {
      cat("Dealer Loses")
    } else {
      cat("Dealer Stands")
      return(game)
    }
  } else {
    cat("Dealer Hits")
    game <- deal(game, dealer = TRUE)
    game <- DealerTurn(game)
  } 
  return(game)
}

player1Run <- GetChoice()
player2Run <- GetChoice(player1Run, 2)
Dealer <- DealerTurn(player2Run)


# who wins:
# max(scores[scores <= 21])
# Where scores is a vector of all the scores 
