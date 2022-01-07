
#generates a df bankroll change over bets made, logs bet made and other data

#inputs:


#outputs: 


#notes on improvements and additions to function

#-------------------------------------------------------------------------------------------------------------------------------------------------


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
    
    #if kelly fraction is negative, that means dont do the bet. 
    #this shouldnt happen if we only play EV bets but lets put a message here to be safe
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
}