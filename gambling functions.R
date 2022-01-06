#------------------------------------------------------------------------------------------------------------------------------------------------
#returns the implied probability for the given an american odd

#inputs
#odds - the given american odds for any bet

#outputs 
#p - the implied probability for the input odds 

#notes on improvements and additions to function
#none

implied_p <- function(odds){
  if(odds > 0) {
    p <- 100/(odds+100)
  } else{
    p <- -odds/(-odds + 100)
  }
  return(p)
}#################################################################################################################################################
#------------------------------------------------------------------------------------------------------------------------------------------------------------
#returns the payout fraction for the given american odd

#inputs
  #odds - the given american odds for any bet

#outputs 
  #payout_fraction - the bet payout fraction for the input american odds

#notes on improvements and additions to function
  #none

payout_fraction <- function(odds){
  if(odds > 0) {
    payout_fraction <- odds/100
  } else{
    payout_fraction <- 100/-odds
  }
  return(payout_fraction)
}#################################################################################################################################################
#------------------------------------------------------------------------------------------------------------------------------------------------------------
#returns the bankroll fraction to bet

#inputs

#outputs 

#notes on improvements and additions to function
#none

kelly_fraction <- function(odds=400,fair_win_p=.25,kelly_multiplier=1){

  #set kelly fraction
  kelly_fraction <- kelly_multiplier* (fair_win_p - (1-fair_win_p)/payout_fraction(odds))

  return(kelly_fraction)
}###############################################################################################################################################


#------------------------------------------------------------------------------------------------------------------------------------------------------------
#returns the the no-vig probability for the first odd given in the input odds pair
#both odds should be given for the best no-vig calculation
#... but if no odds_2 is given, default will return a no-vig odds with an assumed 5% juice split evenly between + and - si


#inputs
  #odds_1 - the given american odds for any bet side 1
  #odds_2 - the given american odds for theside opposite of odds_1

#outputs
  #payout_fraction - the bet payout fraction for the input american odds

#notes on updates and improvements
  #could be improved by studying no-vigs odds assumption of 5% split evenly. Looks like the juice comes out of underdog side
  #2.5% changes on probablity of winning is MASSIVE - especially when compounding and playing underdogs

no_vig_p <- function(odds_1,odds_2 = 0){
  if(odds_2 !=  0) {#both odds have been given
    #calculate implied odds of odds1
      if(odds_1 > 0) {
        p_1 <- 100/(odds_1+100)
      } else{
        p_1 <- -odds_1/(-odds_1 + 100)
      }
    #calculate implied odds of odds2
      if(odds_2 > 0) {
        p_2 <- 100/(odds_2+100)
      } else{
        p_2 <- -odds_2/(-odds_2 + 100)
      }
    #no-vig return for two odds given
    return(p_1/(p_1 + p_2))
  } else {
    #"no-vig" return for one odd given
    return(max(implied_p(odds_1)-.025),0)
  }
}#################################################################################################################################################
#-------------------------------------------------------------------------------------------------------------------------------------------------
#Simulates the "david's coin flip casino" offer from  "this is not financial advice" paper
#the casino promo is a payout of $2.00 for win, from a required $1.00 wager,
#loss of $1.00 for a resutl of a tails

#inputs
#n - the number of coin flips per promo 

#outputs
#NA - total profit from one promo simulation

#notes on improvements and additions to function
#none


coinflip_casino_promo_return <- function(n=1000){
  
  #profit variable for inner for loop
  profit <- 0
  for (i in 1:n){
    if(runif(1) > .5){
      #result of coin flip is over .5, so we win $2.00 and get our $1.00 back
      profit = profit +  2
    }else{
      #result of coin flip is under .5, so we win $0.00 and lose our $1.00
      profit = profit - 1
    }
  }
  
  return(profit)
}#################################################################################################################################################
#list of packages needed for everything
my_packages <- function(){

  # CRAN packages
  library(ggplot2)
  library(viridis)
  library(plyr)
  library(palmerpenguins)
  library(gridExtra)
  library(grid)
  
  # non-CRAN packages (had to load from GITHUB)
  library(ggpattern)
  
  return()
}################################################################################################################################################