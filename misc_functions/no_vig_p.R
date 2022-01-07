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

#---------------------------------------------------------------------------------------------------------------------------------------------------

no_vig_p <- function(odds_1,odds_2 = 0){
  
  #both odds have been given
  if(odds_2 !=  0) {
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
    
  #"no-vig" return for one odd given
  } else {
    return(max(implied_p(odds_1)-.025),0)
  }
  
}