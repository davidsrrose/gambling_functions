# generates a dataframe of the bankroll totals over # of bets #attemps
# bankroll value over time (bets placed) vectors for plotting

# inputs:
# bets_per_attempt - number of ev bets placed per bankroll growth vector
# bankroll_start - starting $$ of bankroll for each run
#

# outputs:
# dataframe of

# notes on improvements and additions to function

#-------------------------------------------------------------------------------
ev_bankroll_looper <- function(simulations = 1000,
                               bets_per_simulation = 1001,
                               bankroll_start = 1000,
                               kelly_multiplier = 1) {

  # initialize variables
  # bankroll growth vectors dataframe
  ev_bankroll_simulations_df <<- data.frame(
    matrix(
      0,
      ncol = simulations,
      nrow = bets_per_simulation
    )
  )

  # temporary loop dataframe for each bankroll df generated in loop
  # 6 columns based on outpud of ev_bankroll_df_generator
  ev_bankroll_df <- matrix(0, nrow = bets_per_simulation, ncol = 6)

  # loop for #of simualtions
  for (i in 1:simulations) {
    ev_bankroll_df[, ] <- 0
    # generate/pull a bankroll growth df
    ev_bankroll_df <- ev_bankroll_df_generator(
      bets = bets_per_simulation,
      bankroll_start = bankroll_start,
      kelly_multiplier = kelly_multiplier
    )
    # store first column bankroll growth
    ev_bankroll_simulations_df[, i] <- ev_bankroll_df$bankroll

    # pull first column and then
  }
  return(ev_bankroll_simulations_df)
}