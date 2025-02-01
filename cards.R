deck <- read.csv("deck.csv", header = TRUE, sep = ",")

head(deck)

deal <- function(cards) {
	cards[1, ]
}

shuffle <- function(cards) {
    random <- sample(1:52, size=52)
    deck[random, ]
}

deck <- shuffle(deck)
deal(deck)