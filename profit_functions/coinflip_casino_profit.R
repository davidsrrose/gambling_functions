
# Simulates the coin flip casino offer from  "this is not financial advice" paper
# the casino promo is a payout of $2.00 for win, from a required $1.00 wager,
# loss of $1.00 for a resutl of a tails

# inputs
# n - the number of coin flips per promo

# outputs
# NA - total profit from one promo simulation

# notes on improvements and additions to function
# none

#---------------------------------------------------------------------------------------------------------------------------------------------------
coinflip_casino_profit <- function(n = 1000) {

  # profit variable for inner for loop
  profit <- 0
  for (i in 1:n) {
    if (runif(1) > .5) {
      # result of coin flip is over .5, so we win $2.00 and get our $1.00 back
      profit <- profit + 2
    } else {
      # result of coin flip is under .5, so we win $0.00 and lose our $1.00
      profit <- profit - 1
    }
  }

  return(profit)
}