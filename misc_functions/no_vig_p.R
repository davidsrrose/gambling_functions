# returns the the no-vig probability for
# the first odd given in the input odds pair
# both odds should be given for the best no-vig calculation

# inputs
# odds_1 - the given american odds for any bet side 1
# odds_2 - the given american odds for theside opposite of odds_1

# outputs
# payout_fraction - the bet payout fraction for the input american odds

# notes on updates and improvements

#-------------------------------------------------------------------------------
no_vig_p <- function(odds_1,
                     odds_2) {

  # calculate implied odds of odds1
  p_1 <- implied_p(odds_1)

  # calculate implied odds of odds2
  p_2 <- implied_p(odds_2)

  # return no-vig probability of odds1
  return(p_1 / (p_1 + p_2))
}