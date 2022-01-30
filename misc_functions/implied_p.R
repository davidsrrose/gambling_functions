# returns the implied probability for the given an american odd

# inputs
# odds - the given american odds for any bet

# outputs
# p - the implied probability for the input odds

# notes on improvements and additions to function
# none

<<<<<<< HEAD
#---------------------------------------------------------------------------------------------------------------------------------------------------

=======
#-------------------------------------------------------------------------------
>>>>>>> drying_odds_ev_scatter_plot
implied_p <- function(odds) {
  if (odds > 0) {
    p <- 100 / (odds + 100)
  } else {
    p <- -odds / (-odds + 100)
  }
  return(p)
}