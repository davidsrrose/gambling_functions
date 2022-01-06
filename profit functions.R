#------------------------------------------------------------------------------------------------------------------------------------------------
#runs one instance of the wynnbet 200% wager match bonus promotion
#Strategy variables (and therefore the variation between strategies) is pulled from the wynnbet static strategy dataframe

#inputs:
#my_strategy - the strategy that we want to employ when we simulation the wynnbet promotionw

#outputs: 
#my_return - the total $$ return of the simulated wynnbet promotion run (can be positive or negative)

#notes on improvements and additions to function
#how can we generalize this function to run multiple platforms' promotional offers?

wynnbet_static <- function(my_strategy = "static0"){
  #initialize variables
  #Row for our particular strategy in order to easily call data about that strategy
  strategy_row<- which(wynnbet_static_strategy_df$strategy == my_strategy)
  
  #profit for our single run
  profit <- 0
  
  #sets for bonus bets bet and bonus bets converted
  bonus_bets_bet <- c(NULL)
  bonus_bets_converted <- c(NULL)
  
  #set of bonus bets bet
  if(wynnbet_static_strategy_df$bonus_bets[strategy_row] > 0){
    bonus_bets_bet <- c(1:wynnbet_static_strategy_df$bonus_bets[strategy_row])
  }else{
    bonus_bets_bet <- NULL
  }
  #set of bonus bets converted
  if(wynnbet_static_strategy_df$bonus_converts[strategy_row] > 0){
    bonus_bets_converted <- c(1:wynnbet_static_strategy_df$bonus_converts[strategy_row])
  }else{
    bonus_bets_converted <- NULL
  }
  
  #initial wager bet
  if(runif(1) < implied_p(wynnbet_static_strategy_df$wager_stake_odds[strategy_row])){ #win initial wager bet
    profit  <-  wynnbet_static_strategy_df$wager_stake[strategy_row]*#...
      payout_fraction(wynnbet_static_strategy_df$wager_stake_odds[strategy_row]) -#...
      wynnbet_static_strategy_df$hedge_stake[strategy_row]
    
    #print(paste("win first bet",profit))
  } else { #lose initial wager bet
    profit  <-  wynnbet_static_strategy_df$hedge_stake[strategy_row]*#...
      payout_fraction(wynnbet_static_strategy_df$hedge_stake_odds[strategy_row]) -#...
      wynnbet_static_strategy_df$wager_stake[strategy_row]
    #print(paste("lose first bet",profit))
  }
  
  #free bets bet
  for (i in bonus_bets_bet){#free bets
    if(runif(1) < implied_p(wynnbet_static_strategy_df$bonus_stake_odds[strategy_row])){
      #win the bonus bet. Note that we only get the payout, no stake back w/free bet play
      profit <- profit  + wynnbet_static_strategy_df$bonus_stake[strategy_row]*#...
        payout_fraction(wynnbet_static_strategy_df$bonus_stake_odds[strategy_row])
      #print(paste("bonus win",profit))
    } else {
      #print(paste("bonus loss",profit))
    }
  }
  #free bets converted
  for (i in bonus_bets_converted){
    #assume 60-75%
    #profit <- profit + wynnbet_static_strategy_df$bonus_stake[strategy_row]*((runif(1)*15+60)/100)
    #assume 70
    profit <- profit + wynnbet_static_strategy_df$bonus_stake[strategy_row]*.7
    #print(paste("bonus convert",profit))
    
  }
  
  #calculate profit from the return
  
  return(profit)
}##################################################################################################################################################

#-------------------------------------------------------------------------------------------------------------------------------------------------
#Runs the 5 wynnbet bonus bets at the given odds, bonus bets and n, returns the avg profit of n runs

#inputs:
#bonus_odds   the odds used for each bonus bet
#bonus_bets   the number of the bonus bets which are bet. 5- bonus_bets would be the number converted
#n            the number of runs

#outputs: 
#avg_profit

#notes on improvements and additions to function

wynnbet_bonus_only <- function(bonus_odds = 400,bonus_bets = 0,n=1){
  
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
}#################################################################################################################################################
#-------------------------------------------------------------------------------------------------------------------------------------------------
#Runs the 5 wynnbet bonus bets at the given odds, bonus bets and n, returns the avg profit of n runs

#inputs:
#bonus_odds   the odds used for each bonus bet
#bonus_bets   the number of the bonus bets which are bet. 5- bonus_bets would be the number converted
#n            the number of runs

#outputs: 
#avg_profit

#notes on improvements and additions to function

maxim_strategy <- function(bonus_odds = 400,bonus_bets = 100,bonus_stake=20, n=1){
  
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
        total_profit <- total_profit  + bonus_stake*payout_fraction(bonus_odds)
        #print(paste("bonus win",total_profit))
      } else {
        #print(paste("bonus loss",total_profit))
      }
    }
    #free bets converted
    for (i in bonus_bets_converted){
      total_profit <- total_profit + bonus_stake*.7
      #print(paste("bonus convert",total_profit))
    }
  }
  
  #calculate avg profit of all runs
  avg_profit <- total_profit/n
  
  return(avg_profit)
}#################################################################################################################################################
#-------------------------------------------------------------------------------------------------------------------------------------------------
#outputs average of n profits generated by the function that places your +EV bet

#inputs:
#bonus_odds   the odds used for each bonus bet
#bonus_bets   the number of the bonus bets which are bet. 5- bonus_bets would be the number converted
#n            the number of runs

#outputs: 
#avg_profit

#notes on improvements and additions to function

ev_bet <- function(oddsjam_odd_1=131,oddsjam_odd_2=-175,bad_odd=300, stake=20){
  
  #initialize
    #profit for our single run
    profit <- 0
    #avg profit
    avg_profit <-0
    #fair win probability
    fair_win_p = no_vig_p(oddsjam_odd_1,oddsjam_odd_2)
    
    #is this change here accurate for +EV odds?
      if(runif(1) < fair_win_p){
        #win the bonus bet. Note that we only get the payout, no stake back w/free bet play
        profit <-  stake*payout_fraction(bad_odd)
       #print(paste("bonus win",profit))
      } else {
        profit <- profit - stake
        #print(paste("bonus loss",profit))
      }
  return(profit)
}#################################################################################################################################################
#-------------------------------------------------------------------------------------------------------------------------------------------------
#generates a vector of bets made

#inputs:
#bonus_odds   the odds used for each bonus bet
#bonus_bets   the number of the bonus bets which are bet. 5- bonus_bets would be the number converted
#n            the number of runs

#outputs: 
#avg_profit

#notes on improvements and additions to function

ev_bankroll_df_generator <- function(bets=1000,bankroll_start = 1000,kelly_multiplier=1){

  
  #initialize variables
    #bankroll dataframe
    ev_bankroll_df <<- data.frame(matrix(ncol = 6, nrow = bets))
    #bankroll dataframe 
    colnames(ev_bankroll_df) <<- c("bankroll","stake","odds","fair_win_p","bet_profit","post_bet_bankroll")
    
    #set bankroll at first bet
    ev_bankroll_df$bankroll[1] <<- bankroll_start

  #loop through all bets
  for (bet_row in 1:bets){
  
  #generate parameters
    
    #bet odds 
    ev_bankroll_df$odds <<- 400
    #fair win 
    ev_bankroll_df$fair_win_p <<- .25
    #bet stake
    ev_bankroll_df$stake[bet_row] <<- ev_bankroll_df$bankroll[bet_row]* kelly_fraction(odds=ev_bankroll_df$odds[bet_row], #...
                                                                               fair_win_p=ev_bankroll_df$fair_win_p[bet_row], #...
                                                                               kelly_multiplier=kelly_multiplier)
    
    #if kelly fraction is negative, that means dont do the bet. this shouldnt happen if we only play EV bets but lets put a message here to be safe
    if(ev_bankroll_df$stake[bet_row] < 0){
      print("WARNING negative kelly fraction?? WHAT ARE YOU DOING WRONG")
    }
                                                
  #make the bet - and fill bet profit, updated bankroll value fields
    if(runif(1) < ev_bankroll_df$fair_win_p[bet_row]){
      #win bet, set profit
      ev_bankroll_df$bet_profit[bet_row] <<-  ev_bankroll_df$stake[bet_row]*payout_fraction(ev_bankroll_df$odds[bet_row])     

    } else {
      #lose bet, set profit
      ev_bankroll_df$bet_profit[bet_row] <<- -ev_bankroll_df$stake[bet_row] 
    }
    
    #set updated bankroll value
    ev_bankroll_df$post_bet_bankroll[bet_row] <<- ev_bankroll_df$bankroll[bet_row] + ev_bankroll_df$bet_profit[bet_row]
    
    #set next row bankroll start - except for last row (bets)
    if (bet_row < bets){
      ev_bankroll_df$bankroll[bet_row+1] <<- ev_bankroll_df$post_bet_bankroll[bet_row]
    }
  }
return(ev_bankroll_df$post_bet_bankroll[bets])
}#################################################################################################################################################


#-------------------------------------------------------------------------------------------------------------------------------------------------
#

#inputs:
#my_strategy  the strategy that we want to employ when we simulation the wynnbet promotion

#outputs: 
#my_return - the total $$ return of the simulated wynnbet promotion run

#notes on improvements and additions to function
#how can we generalize this function to run multiple platforms' promotional offers?

wynnbet_caesars <- function(my_strategy = "actual1"){
  #initialize variables
  #Row for our particular strategy in order to easily call data about that strategy
  strategy_row<- which(wynnbet_static_strategy_df$strategy == my_strategy)
  
  #actual bonus bet odds bet
  bonus_bet_odds <- c(310,370,450,650,650)
  bonus_bet_n <- length(bonus_bet_odds)
  #profit for our single run
  profit <- 0
  
  
  #initial wager bets
  if(runif(1) < no_vig_p(betting_strategies_data_df$wager_stake_odds[strategy_row])){ #win initial wager bet
    profit  <-  betting_strategies_data_df$wager_stake[strategy_row]*#...
      payout_fraction(betting_strategies_data_df$wager_stake_odds[strategy_row]) -#...
      betting_strategies_data_df$hedge_stake[strategy_row]
    #print(paste("win first bet",profit))
  } else { #lose initial wager bet
    profit  <-  betting_strategies_data_df$hedge_stake[strategy_row]*#...
      payout_fraction(betting_strategies_data_df$hedge_stake_odds[strategy_row]) -#...
      betting_strategies_data_df$wager_stake[strategy_row]
    #since we lost the inital wager, we will convert 2 bonus bets, only bet 3
    bonus_bet_n <- 3
    #convert two
    # profit <- profit + betting_strategies_data_df$bonus_stake*1.5
    profit <- profit + (2*betting_strategies_data_df$bonus_stake*.75)
    
    #print(paste("lose first bet",profit))
  }
  
  #free bets bet
  for (i in 1:bonus_bet_n){#free bets
    if(runif(1) < no_vig_p(bonus_bet_odds[i])){
      #win the bonus bet. Note that we only get the payout, no stake back w/free bet play
      profit <- profit  + betting_strategies_data_df$bonus_stake[strategy_row]*#...
        payout_fraction(bonus_bet_odds[i])
      #print(paste("bonus win",profit))
    } else {
      #print(paste("bonus loss",profit))
    }
  }
  
  #convert caesars free bet to profit
  profit <- profit + 1000*.75
  
  #print(paste("caesars convert",profit))
  
  return(profit)
}#################################################################################################################################################
