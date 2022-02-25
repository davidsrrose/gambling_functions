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
  ev_bankroll_df <- data.frame(matrix(ncol = 6, nrow = bets))
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
    # check if we are broke


    # generate parameters for the bet
    ev_bet_parameters <- ev_bet_generator()

    # bet odds
    ev_bankroll_df$odds[bet_row] <- ev_bet_parameters[1]

    # bet fair win probability
    ev_bankroll_df$fair_win_p[bet_row] <- ev_bet_parameters[2]

    # bet stake
    ev_bankroll_df$stake[bet_row] <-
      ev_bankroll_df$bankroll[bet_row] *
        kelly_fraction(
          odds = ev_bankroll_df$odds[bet_row],
          fair_win_p = ev_bankroll_df$fair_win_p[bet_row],
          kelly_multiplier = kelly_multiplier
        )
    # calculated stake for bet is larger than current bankroll, adjust down
    if (ev_bankroll_df$stake[bet_row] > ev_bankroll_df$bankroll[bet_row]) {
      ev_bankroll_df$stake[bet_row] <- ev_bankroll_df$bankroll[bet_row]
    }

    # stake is negative, we are out of money
    if (ev_bankroll_df$bankroll[bet_row] <= 0) {
      print("ran out of money")
      # fill the rest of the dataframe with zeros
      ev_bankroll_df[bet_row:bets, ] <- 0
      # then exit
      return(ev_bankroll_df)
      stop()
    }

    # print(paste("odds", ev_bankroll_df$odds[bet_row]))
    # print(paste("fair win p", ev_bankroll_df$fair_win_p[bet_row]))
    # print(paste("kelly multiplier", kelly_multiplier))
    # print(paste("stake", ev_bankroll_df$stake[bet_row]))


    # if kelly fraction is negative, that means dont do the bet.
    # this shouldnt happen w/ just EV bets but lets put a message here
    if (ev_bankroll_df$stake[bet_row] < 0) {
      print("WARNING negative kelly fraction?? WHAT ARE YOU DOING WRONG")
    }

    # make the bet - and fill bet profit, updated bankroll value fields
    if (runif(1) < ev_bankroll_df$fair_win_p[bet_row]) {
      # win bet, set profit
      ev_bankroll_df$bet_profit[bet_row] <-
        ev_bankroll_df$stake[bet_row] *
          payout_fraction(ev_bankroll_df$odds[bet_row])
    } else {
      # lose bet, set profit
      ev_bankroll_df$bet_profit[bet_row] <- -ev_bankroll_df$stake[bet_row]
    }

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