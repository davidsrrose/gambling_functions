# runs one instance of the wynnbet 200% wager match bonus promotion
# Strategy variables (and therefore the variation between strategies) is pulled from the wynnbet static strategy dataframe

# inputs:
# strategy - the strategy that we want to employ when we simulation the wynnbet promotionw

# outputs:
# my_return - the total $$ return of the simulated wynnbet promotion run (can be positive or negative)

# notes on improvements and additions to function
# how can we generalize this function to run multiple platforms' promotional offers?
#------------------------------------------------------------------------------------------------------------------------------------------------
wynnbet_static_profit <- function(strategy = "static0") {
  # initialize variables
  # Row for our particular strategy in order to easily call data about that strategy
  strategy_row <- which(wynnbet_static_strategy_df$strategy == strategy)

  # profit for our single run
  profit <- 0

  # sets for bonus bets bet and bonus bets converted
  bonus_bets_bet <- c(NULL)
  bonus_bets_converted <- c(NULL)

  # set of bonus bets bet
  if (wynnbet_static_strategy_df$bonus_bets[strategy_row] > 0) {
    bonus_bets_bet <- c(1:wynnbet_static_strategy_df$bonus_bets[strategy_row])
  } else {
    bonus_bets_bet <- NULL
  }
  # set of bonus bets converted
  if (wynnbet_static_strategy_df$bonus_converts[strategy_row] > 0) {
    bonus_bets_converted <- c(1:wynnbet_static_strategy_df$bonus_converts[strategy_row])
  } else {
    bonus_bets_converted <- NULL
  }

  # initial wager bet
  # win initial wager bet
  if (runif(1) < implied_p(wynnbet_static_strategy_df$wager_stake_odds[strategy_row])) {
    profit <- wynnbet_static_strategy_df$wager_stake[strategy_row] *
      payout_fraction(wynnbet_static_strategy_df$wager_stake_odds[strategy_row]) -
      wynnbet_static_strategy_df$hedge_stake[strategy_row]

    # print(paste("win first bet",profit))
    # lose initial wager bet
  } else {
    profit <- wynnbet_static_strategy_df$hedge_stake[strategy_row] *
      payout_fraction(wynnbet_static_strategy_df$hedge_stake_odds[strategy_row]) -
      wynnbet_static_strategy_df$wager_stake[strategy_row]
    # print(paste("lose first bet",profit))
  }

  # free bets bet
  for (i in bonus_bets_bet) { # free bets
    if (runif(1) < implied_p(wynnbet_static_strategy_df$bonus_stake_odds[strategy_row])) {
      # win the bonus bet. Note that we only get the payout, no stake back w/free bet play
      profit <- profit + wynnbet_static_strategy_df$bonus_stake[strategy_row] *
        payout_fraction(wynnbet_static_strategy_df$bonus_stake_odds[strategy_row])
      # print(paste("bonus win",profit))
    } else {
      # print(paste("bonus loss",profit))
    }
  }
  # free bets converted
  for (i in bonus_bets_converted) {
    # assume 70 % conversion
    profit <- profit + wynnbet_static_strategy_df$bonus_stake[strategy_row] * .7
    # print(paste("bonus convert",profit))
  }
  return(profit)
}