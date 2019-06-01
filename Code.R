# Project 3 : Slot Machine----
# Chapter 9 Programs----
get_symbols <- function() {
  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  sample(
    wheel,
    size = 3,
    replace = TRUE,
    prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52)
  )
}
get_symbols()

#9.2 If statements----
if (this) {
  that
}

#9.3 else statements----
if (this) {
  #Plan A
} else {
  #Plan B
}

a <- 3.14
dec <- a - trunc(a)
if (dec >=0.5) {
  a <- trunc(a) + 1
} else {
  a <- trunc(a)
}
a

a <- 1
b <- 1

if (a > b) {
  print("A wins!")
} else if (a < b) {
  print("B wins!")
} else {
  print("Tie.")
}

symbols <- c("7", "7", "7")
score <- function(symbols) {
  # identify case
  same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
  bars <- symbols %in% c("B", "BB", "BBB")
  # get prize
  if ( same) {
    payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25, 
                 "B" = 10, "C" = 10, "0" = 0)
    prize <- unname(payouts[symbols[1]])
    } else if ( all(bars) ) {
      prize <- 5
      } else {
        cherries <- sum(symbols == "C")
        prize <- c(0, 2, 5)[cherries + 1]
      }
  
  # adjust for diamonds
  diamonds <- sum(symbols == "DD")
  prize * 2 ^ diamonds
}

play <- function(){
  symbols <- get_symbols()
  print(symbols)
  score(symbols)
}

play()

