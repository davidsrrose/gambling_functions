
# returns the payout fraction for the given american odd

# inputs
# odds - the given american odds for any bet

# outputs
# payout_fraction - the bet payout fraction for the input american odds

# notes on improvements and additions to function
# none

<<<<<<< HEAD
#---------------------------------------------------------------------------------------------------------------------------------------------------

=======
#-------------------------------------------------------------------------------
>>>>>>> drying_odds_ev_scatter_plot
payout_fraction <- function(odds) {
  if (odds > 0) {
    payout_fraction <- odds / 100
  } else {
    payout_fraction <- 100 / -odds
  }
  return(payout_fraction)
}