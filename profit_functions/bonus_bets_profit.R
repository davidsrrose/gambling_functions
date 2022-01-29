# runs number of bonus bets

# inputs:

# outputs:

# notes on improvements and additions to function

#-------------------------------------------------------------------------------
bonus_bets_profit <- function(total_bonus_bets = 100,
                              bonus_bets_bet = 100,
                              bonus_odds = 400,
                              bonus_stake = 20) {

  # error message if we try to bet more than the total number of bets we have
  if (total_bonus_bets < bonus_bets_bet) {
    # warning message to user
    print("Error in function, bonus_bets cannot be larger than total_bets")
    # exit function
    stop()
  }

  # profit total all runs
  total_profit <- 0

  # bonus betting sets to null to start   #theres got to be a better way to do this than those
  bonus_bets_bet_set <- c(NULL)
  bonus_bets_converted_set <- c(NULL)

  # set of bonus bets bet
  if (bonus_bets_bet > 0) {
    bonus_bets_bet_set <- c(1:bonus_bets_bet)
  } else {
    bonus_bets_bet_set <- NULL
  }

  # set of bonus bets converted
  if ((total_bonus_bets - bonus_bets_bet) > 0) {
    bonus_bets_converted_set <- c(1:(total_bonus_bets - bonus_bets_bet))
  } else {
    bonus_bets_converted_set <- NULL
  }

  print(bonus_bets_bet_set)
  print(bonus_bets_converted_set)


  # free bets bet
  for (i in bonus_bets_bet_set) { # free bets
    if (runif(1) < implied_p(bonus_odds)) {
      # win the bonus bet. Note that we only get the payout, no stake back w/free bet play
      total_profit <- total_profit +
        bonus_stake *
          payout_fraction(bonus_odds)
      print(
        paste(
          "bonus win",
          total_profit
        )
      )
    } else {
      print(
        paste(
          "bonus loss",
          total_profit
        )
      )
    }
  }
  # free bets converted
  for (i in bonus_bets_converted_set) {
    total_profit <- total_profit + bonus_stake * .7
    print(
      paste(
        "bonus convert",
        total_profit
      )
    )
  }
  return(total_profit)
}