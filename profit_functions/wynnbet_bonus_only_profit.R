
#Runs the 5 wynnbet bonus bets at the given odds, bonus bets and n, returns the avg profit of n runs

#inputs:
#bonus_odds   the odds used for each bonus bet
#bonus_bets   the number of the bonus bets which are bet. 5- bonus_bets would be the number converted
#n            the number of runs

#outputs: 
#avg_profit

#notes on improvements and additions to function

#-------------------------------------------------------------------------------------------------------------------------------------------------
wynnbet_bonus_only_profit <- function(bonus_odds = 400,bonus_bets = 0,n=1){
  
  #profit for our single run
  profit <- 0
  #profit total all runs
  total_profit <- 0
  #avg profit
  avg_profit <-0
  
  #bonus betting sets to null to start   #theres got to be a better way to do this than those 
  bonus_bets_bet <- c(NULL)
  bonus_bets_converted <- c(NULL)
  
  #set of bonus bets bet
  if(bonus_bets > 0){
    bonus_bets_bet <- c(1:bonus_bets)
  } else {
    bonus_bets_bet <- NULL
  }
  
  #set of bonus bets converted
  if( (5-bonus_bets)> 0){
    bonus_bets_converted <- c(1:(5-bonus_bets))
  } else {
    bonus_bets_converted <- NULL
  }
  
  for (k in 1:n){
    #free bets bet
    for (i in bonus_bets_bet){#free bets
      if(runif(1) < implied_p(bonus_odds)){
        #win the bonus bet. Note that we only get the payout, no stake back w/free bet play
        total_profit <- total_profit  + 300*payout_fraction(bonus_odds)
        #print(paste("bonus win",profit))
      } else {
        #print(paste("bonus loss",profit))
      }
    }
    #free bets converted
    for (i in bonus_bets_converted){
      total_profit <- total_profit + 300*.7
      #print(paste("bonus convert",profit))
    }
  }
  
  #calculate avg profit of all runs
  avg_profit <- total_profit/n
  
  return(avg_profit)
}