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

sum(deck$face == "ace")

deck$value[deck$face == "ace"] <- 14
deck$value[deck$face == "ace"]

deck$value <- 0
deck$value[deck$suit == "hearts"] <- 1
deck$value[deck$suit == "spades" & deck$face == "queen"] <- 13

deck <- read.csv("deck.csv", header = TRUE, sep = ",")
deck$value[deck$face %in% c("jack", "queen", "king")] <- 10
deck$value[deck$face == "ace"] <- NA

head(deck)
tail(deck)