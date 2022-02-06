# returns odds and fair win probabilty for a randomly selected bet from input dataframe

# inputs
# min_ev - min ev of bets we want in our returned dataframe
# max_ev - max ev of bets we want in our returned dataframe
# min odds - max odd of bets we want in our returned dataframe
# max odds -min odd of bets we want in our returned dataframe
# type - type of bet we want to place "standard" or "live" or "both"

# outputs
# bet parameters matrix - [ev bet odd, ev bet fair win probability]

# notes on updates and improvements

#-------------------------------------------------------------------------------
ev_bet_generator <- function(min_ev = -Inf,
                             max_ev = Inf,
                             min_odd = -Inf,
                             max_odd = Inf,
                             bet_type = "both",
                             bet_df_to_filter = ev_bet_df) {

  # initialize variables
  # random row of df for bet
  random_bet_row <- 0

  # bet parameters matrix
  ev_bet_parameters <- matrix(0, nrow = 1, ncol = 2)
  colnames(ev_bet_parameters) <- c("odds", "fair_win_p")

  # sample dataframe to pick from
  ev_bet_sample_df <- data.frame(NULL)
  ev_bet_sample_df <- ev_bet_df_filter(
    min_ev = min_ev,
    max_ev = max_ev,
    min_odd = min_odd,
    max_odd = max_odd,
    bet_type = bet_type,
    bet_df_to_filter = bet_df_to_filter
  )

  # randomly select bet row from from ev_bets dataframe, based on parameters
  random_bet_row <- sample(1:nrow(ev_bet_sample_df), 1)

  # set odds and fair win probability
  # odds for bet
  ev_bet_parameters[1] <- ev_bet_sample_df$ev_odd[random_bet_row] # nolint

  # fair win probability for bet
  ev_bet_parameters[2] <-
    no_vig_p(
      ev_bet_sample_df$oddsj_ev_odd[random_bet_row],
      ev_bet_sample_df$oddsj_ev_odd_opp[random_bet_row]
    )

  return(ev_bet_parameters)
}