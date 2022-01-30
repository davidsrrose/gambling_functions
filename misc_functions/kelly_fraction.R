# returns the bankroll fraction to bet based on simple kelly criterion

# inputs

# outputs

# notes on improvements and additions to function
# none

#-------------------------------------------------------------------------------
<<<<<<< HEAD

=======
>>>>>>> drying_odds_ev_scatter_plot
kelly_fraction <- function(odds = 400,
                           fair_win_p = .25,
                           kelly_multiplier = 1) {
  # set kelly fraction
  kelly_fraction <- kelly_multiplier *
    (fair_win_p - (1 - fair_win_p) / payout_fraction(odds))
  return(kelly_fraction)
}