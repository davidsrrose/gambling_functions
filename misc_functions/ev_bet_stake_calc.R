# returns the bankroll fraction to bet based on simple kelly criterion

# inputs

# outputs

# notes on improvements and additions to function
# none

#-------------------------------------------------------------------------------

ev_bet_stake_calc <- function(bankroll,
                              odds,
                              fair_win_p,
                              kelly_multiplier) {
  bet_stake <- 0


  # calculate exact stake
  bet_stake <-
    bankroll *
      kelly_fraction(
        odds,
        fair_win_p,
        kelly_multiplier
      )

  # if kelly fraction is negative...
  # stake will be negative and we should not bet
  # this shouldnt happen w/ just EV bets but lets put a message here
  if (bet_stake < 0) {
    print("WARNING negative kelly fraction?? WHAT ARE YOU DOING WRONG")
  }

  # round to whole dollar if less than 50,
  if (bet_stake < 50) {
    bet_stake <- round(bet_stake, 0)
  } else { # otherwise to nearest 5
    bet_stake <- round(bet_stake / 5) * 5
  }

  # calculated stake for bet is larger than current bankroll
  # adjust down to remaining bankroll
  if (bet_stake > bankroll) {
    bet_stake <- bankroll
  }

  return(bet_stake)
}