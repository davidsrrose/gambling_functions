# returns odds and fair win probabilty for a randomly selected bet from ev_bets

# inputs
# min_ev - minimum ev of bets we want to place
# type - type of bet we want to place "standard" or "live" or "both"

# outputs
# vector of odds and fair win probabilty for ev bet we want to make

# notes on updates and improvements

#-------------------------------------------------------------------------------
ev_bet_generator <- function(min_ev = .05, bet_type = "standard") {

  # initialize variables
  ev_bet_parameters <- c(0, 0)
  bet_row <- 0
  ev_bets_sample_df <- data.frame(NULL)

  # select ev_bets_sample_df for min ev
  ev_bets_sample_df <- ev_bets[ev_bets$ev >= min_ev, ]

  # select for type of not "both"
  if (bet_type != "both") {
    ev_bets_sample_df <- ev_bets_sample_df[ev_bets_sample_df$bet_type == bet_type, ]
  }

  # randomly select bet from from ev_bets dataframe, based on parameters
  bet_row <- sample(1:nrow(ev_bets), 1)

  # set odds and fair win probability
  # odds for bet
  ev_bet_parameters[1] <- ev_bets$ev_odd[bet_row] # nolint


  # fair win probability for bet
  ev_bet_parameters[2] <-
    no_vig_p(
      ev_bets$oddsj_ev_odd[bet_row],
      ev_bets$oddsj_ev_odd_opp[bet_row]
    )

  return(ev_bet_parameters)
}