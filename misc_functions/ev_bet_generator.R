#returns odds and fair win probabilty for a randomly selected bet from ev_bets

# inputs
# onone

# outputs
# vector - ev bet and eb

# notes on updates and improvements

#-------------------------------------------------------------------------------
ev_bet_generator <- function() {

  #initialize variables
  ev_bet_parameters <- c(0,0)
  bet_row <- 0
  
  #randomly select bet from from ev_bets dataframe  

  bet_row <- sample(1:nrow(ev_bets), 1)

  #set odds and fair win probability
  #odds for bet
  ev_bet_parameters[1] <- ev_bets$ev_odd[bet_row]
  
  
  #fair win probability for bet
  ev_bet_parameters[2] <- 
    no_vig_p(
      ev_bets$oddsj_ev_odd[bet_row],
      ev_bets$oddsj_ev_odd_opp[bet_row]
    )
  
  return(ev_bet_parameters)
}