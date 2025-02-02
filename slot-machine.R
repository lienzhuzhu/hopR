## slot-machine.R
## A simple slot machine simulation based on the Manitoba controversy
## Lien Zhu

# TODO: handle Diamonds correctly

get_symbols <- function() {
    wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
    sample(wheel, size = 3, replace = TRUE, 
           prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
}

better_score <- function(symbols) {
    
    diamonds <- sum(symbols == "DD")
    cherries <- sum(symbols == "C")
    
    # identify case
    # since diamonds are wild, only nondiamonds 
    # matter for three of a kind and all bars
    slots <- symbols[symbols != "DD"]
    same <- length(unique(slots)) == 1
    bars <- slots %in% c("B", "BB", "BBB")
    
    # assign prize
    if (diamonds == 3) {
        prize <- 100
    } else if (same) {
        payouts <- c("7" = 80, "BBB" = 40, "BB" = 25,
                     "B" = 10, "C" = 10, "0" = 0)
        prize <- unname(payouts[slots[1]])
    } else if (all(bars)) {
        prize <- 5
    } else if (cherries > 0) {
        # diamonds count as cherries
        # so long as there is one real cherry
        prize <- c(0, 2, 5)[cherries + diamonds + 1] # add one for proper indexing
    } else {
        prize <- 0
    }
    
    # double for each diamond
    prize * 2^diamonds
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
    structure(score(symbols), symbols = symbols, class = "slots")
}

slot_display <- function(prize) {
    symbols <- attr(prize, "symbols")
    symbols <- paste(symbols, collapse = " ")
    string <- paste(symbols, prize, sep = "\n$")
    cat(string)
}

print.slots <- function(x, ...) {
    slot_display(x)
}

wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
prob <- c("DD" = 0.03, "7" = 0.03, "BBB" = 0.06, "BB" = 0.1,
         "B" = 0.25, "C" = 0.01, "0" = 0.52)
combos <- expand.grid(wheel, wheel, wheel, stringsAsFactors = FALSE)
combos$prob1 <- prob[combos$Var1]
combos$prob2 <- prob[combos$Var2]
combos$prob3 <- prob[combos$Var3]
combos$prob <- combos$prob1 * combos$prob2 * combos$prob3

combos$prize <- NA

for (i in 1:nrow(combos)) {
    #print(combos[i, 1:3])
    #combos$prize[i] <- score(as.character(combos[i, 1:3]))
    symbols <- c(combos[i, 1], combos[i, 2], combos[i, 3])
    combos$prize[i] <- better_score(symbols)
}

head(combos, 10)

sum(combos$prob * combos$prize)


get_many_symbols <- function(n) {
    wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
    vec <- sample(wheel, size = 3 * n, replace = TRUE,
                  prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))
    matrix(vec, ncol = 3) # Reshapes into an n x 3 matrix
}

play_many <- function(n) {
    symb_mat <- get_many_symbols(n = n)
    data.frame(w1 = symb_mat[,1], w2 = symb_mat[,2],
               w3 = symb_mat[,3], prize = score_many(symb_mat))
}