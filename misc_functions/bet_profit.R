# returns bet profit given stake, odds, fair in p

# inputs

# outputs

# notes on improvements and additions to function
# none

#-------------------------------------------------------------------------------

bet_profit <- function(stake,
                       odds,
                       fair_win_p) {

  # initialize variables
  bet_profit <- 0

  # make the bet - and fill bet profit, updated bankroll value fields
  if (runif(1) < fair_win_p) {
    # win bet, set profit
    bet_profit <- stake * payout_fraction(odds)
  } else {
    # lose bet, set profit
    bet_profit <- -stake
  }

  return(bet_profit)
}