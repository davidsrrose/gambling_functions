# generates a df bankroll change over bets made, logs bet made and other data

# inputs:

# outputs:

# notes on improvements and additions to function

#-------------------------------------------------------------------------------
ev_bankroll_df_generator <- function(bets = 1000,
                                     bankroll_start = 1000,
                                     kelly_multiplier = 1) {

  # initialize variables
  # bankroll dataframe
  ev_bankroll_df <- data.frame(matrix(0, ncol = 6, nrow = bets))
  # ev bet parameters
  ev_bet_parameters <- c(0, ncol = 2)

  # bankroll dataframe
  colnames(ev_bankroll_df) <- c(
    "bankroll",
    "stake",
    "odds",
    "fair_win_p",
    "bet_profit",
    "post_bet_bankroll"
  )

  # set bankroll at first bet
  ev_bankroll_df$bankroll[1] <- bankroll_start

  # loop through number of bets specified
  for (bet_row in 1:bets) {
    # generate parameters for the bet
    ev_bet_parameters <- ev_bet_generator()

    # fill df with odds, fair win, stake
    # bet odds
    ev_bankroll_df$odds[bet_row] <- ev_bet_parameters[1]

    # bet fair win probability
    ev_bankroll_df$fair_win_p[bet_row] <- ev_bet_parameters[2]

    # bet stake
    ev_bankroll_df$stake[bet_row] <- ev_bet_stake_calc(
      ev_bankroll_df$bankroll[bet_row],
      ev_bankroll_df$odds[bet_row],
      ev_bankroll_df$fair_win_p[bet_row],
      kelly_multiplier
    )

    # fill profit
    ev_bankroll_df$bet_profit <- bet_profit(
      ev_bankroll_df$stake[bet_row],
      ev_bankroll_df$odds[bet_row],
      ev_bankroll_df$fair_win_p[bet_row]
    )

    # set updated bankroll value
    ev_bankroll_df$post_bet_bankroll[bet_row] <-
      ev_bankroll_df$bankroll[bet_row] +
      ev_bankroll_df$bet_profit[bet_row]

    # set next row bankroll start - except for last row (bets)
    if (bet_row < bets) {
      ev_bankroll_df$bankroll[bet_row + 1] <-
        ev_bankroll_df$post_bet_bankroll[bet_row]
    }
  }

  return(ev_bankroll_df)
}