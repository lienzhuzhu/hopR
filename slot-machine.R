## slot-machine.R
## A simple slot machine simulation based on the Manitoba controversy
## Lien Zhu

get_symbols <- function() {
    wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
    sample(wheel, size = 3, replace = TRUE, 
           prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}

score <- function(symbols) {
    if (all(symbols == symbols[1])) {
        payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25,
                     "B" = 10, "C" = 10, "0" = 0) # Example of a LOOKUP TABLE
        prize <- unname(payouts[symbols[1]])
    } else if (all(symbols %in% c("B", "BB", "BBB"))) {
        prize <- 5
    } else {
        cherries <- sum(symbols == "C")
        prize <- c(0, 2, 5)[cherries + 1]
    }
    
    diamonds <- sum(symbols == "DD")
    prize * 2^diamonds
}

play <- function() {
    symbols <- get_symbols()
    score(symbols)
}

play()