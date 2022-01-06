#------------------------------------------------------------------------------------------------------------------------------------------------------------
#Gives a scatter plot of expected profit of wynnbet bonus bets bet across different odds
#this gives a good visual indication of the optional +odds to play free bets at by viewing the EV of each of the 20+ odds

#inputs:
  #simulations_per_odd - the number of monte carlo simulations we want to make per odd we loop through (-150 to 2000ish)

#outputs: 
  #saves a file ev_scatter_plot_by_odd.png of the plot 

#notes on improvements and additions to function
  #how can we generalize this function to run multiple platforms' promotional offers?
  #add a progressbar, instead of printing odds simululation completion to the terminal like a n00b
  #outsource data generation to a new function to seperate data generation and plotting

odds_ev_scatter_plot <- function(simulations_per_odd=1000){
  
  #initialize variables
    #number of odds we want to loop across
    odds_looped <- 50
    
    #number of bonus bet levels we want to loop across
    bonus_bets_bet_looped <- 6
    
    #total profit per odd variable
    total_profit_per_odd <- 0
    
    #matrix of EV by bonus bet odds
    e_profit_df <- data.frame(matrix(0,nrow=odds_looped, ncol=7))
    colnames(e_profit_df) <- c("odds","profit_0_bet","profit_1_bet","profit_2_bet", #...
                               "profit_3_bet","profit_4_bet","profit_5_bet")
    
    #set first values of the odds
    e_profit_df$odds[1:4] <- c(-150,-110,110,150)
    #then increment the rest by 50
    for (i in 5:odds_looped){ 
      e_profit_df$odds[i] <- 150 + (i-4)*50
    }

  #loop and fill profit data in e_profit_df
    for (i in 1:(odds_looped)){
      for (bonus_bets in 0:5){
        e_profit_df[i,bonus_bets+2] <- wynnbet_bonus_only(bonus_odds=e_profit_df$odds[i],bonus_bets = bonus_bets,n=simulations_per_odd)
      }
      print(paste("odds completed",e_profit_df$odds[i]))
    }
    
  #plot the data
  ev_scatter_plot <- ggplot(e_profit_df, aes(x=odds,y=value,color=variable)) +
    
    #title
    ggtitle(paste("Distribution of Expected Profit\nWynnbet Promo Bonus Bets")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    
    #set colors for the series
    scale_fill_viridis(discrete=TRUE,option="magma") +
  
    #plot all 6 y variable sets
    geom_point(aes(y = profit_0_bet,col = "0 bet, 5 converted")) +
    geom_point(aes(y = profit_1_bet,col = "1 bet, 4 converted")) +
    geom_point(aes(y = profit_2_bet,col = "2 bet, 3 converted")) +
    geom_point(aes(y = profit_3_bet,col = "3 bet, 2 converted")) +
    geom_point(aes(y = profit_4_bet,col = "4 bet, 1 converted")) +
    geom_point(aes(y = profit_5_bet,col = "5 bet, 0 converted")) +

    #x limit, label, breaks, size
    xlim(min(e_profit_df$odds), max(e_profit_df$odds)) +
    xlab("Bonus Bet Odds") +
    
    #y limit and label & x and y label sizing
    ylim(0,max(e_profit_df[,2:7])) +
    ylab("Expected Profit ($)") +
    
    #floor lines and labels for each strategy
    geom_hline(aes(yintercept=1050),col = "gray41", linetype="dashed", size=.8,alpha=.9) +
      annotate(geom = "text", label = " \nMin profit (0 bet, 5 converted): $1050",x = 1550, y = 1050, hjust="left",color = "gray41", size = 3) +
    geom_hline(aes(yintercept=840),col = "gray41", linetype="dashed", size=.8,alpha=.9) +
      annotate(geom = "text", label = "Min profit (1 bet, 4 converted): $840\n ",x = 1550, y = 840, hjust="left",color = "gray41", size = 3) +
    geom_hline(aes(yintercept=630),col = "gray41", linetype="dashed", size=.8,alpha=.9) +
      annotate(geom = "text", label = "Min profit (2 bet, 3 converted): $630\n ",x = 1550, y = 630, hjust="left",color = "gray41", size = 3) +
    geom_hline(aes(yintercept=420),col = "gray41", linetype="dashed", size=.8,alpha=.9) +
      annotate(geom = "text", label = "Min profit (3 bet, 2 converted): $420\n ",x = 1550, y = 420, hjust="left",color = "gray41", size = 3) +
    geom_hline(aes(yintercept=210),col = "gray41", linetype="dashed", size=.8,alpha=.9) +
      annotate(geom = "text", label = "Min profit (4 bet, 1 converted): $210\n ",x = 1550, y = 210, hjust="left",color = "gray41", size = 3) +
    geom_hline(aes(yintercept=0),col = "gray41", linetype="dashed", size=.8,alpha=.9) +
      annotate(geom = "text", label = "Min profit (5 bet, 0 converted): $0\n ",x = 1550, y = 0, hjust="left",color = "gray41", size = 3)
    
  #save png of the scatter plot
  ggsave(file=paste("ev_scatter_plot_by_odd.png"), ev_scatter_plot,width = 12,height=7,units="in")
  
    
}############################################################################################################################################################
#-------------------------------------------------------------------------------------------------------------------------------------------------
#creates a profit density histogram of the 

#inputs
#profit vector

#outputs 

#notes on improvements and additions to function
#none
#---------------------------------------------
money_gram_ev_betting <- function(n = 100){
  
  #load CRAN packages
  library(ggplot2)
  library(viridis)
  library(plyr)
  library(palmerpenguins)
  library(gridExtra)
  library(grid)
  
  #load non-CRAN packages (had to load from GITHUB)
  library(ggpattern)
  
  #initialize variables
  #profit vector
  profit_vector <- c(0,nrow = n, ncol = 1)
  #simulation n attempts of strategy and record profits to profit_vector
  for (i in 1:n){
    #profit_vector[i] <- wynnbet_static(strategy)
    profit_vector[i] <- ev_bankroll_df_generator(bets=1000,bankroll_start=5000,kelly_multiplier=1)
  }
  print(profit_vector)
  #print(profit_vector)
  
  #because rounding errors were making the graphs bad
  profit_vector <-round(profit_vector,2)
  
  #create a profit_df dataframe of profit frequencies
  profit_df <- data.frame(count(profit_vector))
  
  #add column to profit_df with the probability of each profit outcome
  profit_df$probability <- profit_df$freq/sum(profit_df$freq)
  
  #because rounding errors were making the graphs bad
  profit_df$probability <-round(profit_df$probability,3)
  
  #max probability
  probability <- max(profit_df$probability)
  
  #add column to profit_df with true/false on of the profit is positive or negative
  profit_df$pos <- profit_df$x > 0
  
  #add column for left or right justification. Set to left, then switch to right if next bar is bigger
  profit_df$bar_label_justification <- "left"
  if(nrow(profit_df) > 1){ #only do it if there's more than 1 bar
    for (i in 1:(nrow(profit_df)-1)){
      if(profit_df$probability[i] < profit_df$probability[i+1]){
        profit_df$bar_label_justification[i] <- "right"
      }
    }
  }
  
  #update column names
  colnames(profit_df) <- c("profit","frequency","probability","positive","bar_label_justification")
  
  #calculate variables needed for plot
  average_profit <- round(mean(sum(profit_vector)),2)
  #mean line annotation y value
  annotation_y <- max(profit_df$probability)
  #probability of profit & max profit
  p_profit <- round(sum(profit_vector > 0)/n,4)
  print(sum(profit_df$profit >0))
  
  max_profit <- round(max(profit_df$profit),2)
  #probability of loss & min profit
  p_loss <- round(1-p_profit,4)
  min_profit <- round(min(profit_df$profit),2)
  #graph bar width
  bar_width <- (max_profit-min_profit)/nrow(profit_df)*.75
  #x axis limits - w/buffer for range
  x_axis_min <- min(profit_df$profit) - bar_width
  x_axis_max <- max(profit_df$profit) + bar_width  + 500
  # y axis limits
  y_axis_max <- max(profit_df$probability)*1.25
  #bar color (need an if for when there is no negative return - all green!)
  fill_colors <-c("darkred","darkgreen")
  if(min(profit_df$profit >0)){
    fill_colors <- "darkgreen"
  }
  #bar fill (need an if for when there is no negative return - no hash, just green!)
  fill_patterns <-c("stripe","none")
  if(min(profit_df$profit >0)){
    fill_patterns <- "none"
  }
  
  #add column for x value of the bar label
  profit_df$bar_label_x <- profit_df$profit-bar_width/2+10
  profit_df$bar_label_x[which(profit_df$bar_label_justification =="right")] <- profit_df$profit[which(profit_df$bar_label_justification =="right")]+bar_width/2-10
  
  #variable for center bar (bar where the E(profit) line will cover the bar label)
  #e_profit_bar_probability <- 
  e_profit_bar_label_probablity <- profit_df$probability[which.min(abs(average_profit - profit_df$profit))]
  
  #plot it(dataframe, aestices x,y, fill based on positive or negative profit)
  distribution_plot <- ggplot(profit_df, aes(x=profit,y=probability,fill=positive,pattern=positive)) +
    #make it a bar chart of the options
    geom_bar_pattern(stat="identity",width=bar_width,alpha =.55,color="black",
                     #bar sripe settings
                     pattern_fill = "black",
                     pattern_angle = 60,
                     pattern_density = .1,
                     pattern_spacing = .02) +
    #turn off legend for striping
    theme(legend.position = 'none') +
    
    #color red or green
    scale_fill_manual(aes(density=.9),values = fill_colors, guide="none") + 
    #pattern only for red/negative returns
    scale_pattern_manual(values=fill_patterns) +
    
    #title
    ggtitle(paste("Expected bankroll after 1000 Bets, \n all +400 bets, fair win p = 0.25, kelly multilier = 1")) +
    theme(plot.title = element_text(hjust = 0.5,size=20)) +
    
    #add vertical mean line bottom (around vertical mean line bar label)
    #geom_vline(aes(xintercept=average_profit),color="darkgreen", linetype="dashed", size=.8) +
    geom_segment(aes(x = average_profit, y = 0, xend = average_profit, yend = e_profit_bar_label_probablity-y_axis_max/100), linetype = "dashed", col = "darkgreen",size=.8,alpha=.8) +
    #add vertical mean line top (around vertical mean line bar label)
    geom_segment(aes(x = average_profit, y =  e_profit_bar_label_probablity+y_axis_max/12, xend = average_profit, yend = Inf), linetype = "dashed", col = "darkgreen",size=.8,alpha=.8) +
    #mean label 
    annotate(geom = "text", label = paste("E(profit)"),x = average_profit+30, y = y_axis_max,hjust="left",color = "darkgreen", size = 4) +
    
    #bar $ return label
    geom_text(aes(label = paste("$",profit), x = bar_label_x),position=position_dodge(width=0.9), vjust=-0.35, #...
              hjust=profit_df$bar_label_justification,size=4) + #...
    
    #left justify if, right justify if  the one to the right is bigger
    #bar % return label
    geom_text(aes(label = paste("p",probability), x = bar_label_x),position=position_dodge(width=0.9), vjust=-2, #...
              hjust=profit_df$bar_label_justification,size=4) +
    
    #x limit, label, breaks, size
    xlim(x_axis_min, x_axis_max) +
    xlab("Profit ($)") +
    theme(axis.title=element_text(size=15),axis.text=element_text(size=10)) +
    
    #y limit and label & x and y label sizing
    ylim(0,y_axis_max) +
    ylab("Probability") +
    
    #stats annotation labels
    annotate(geom = "text", x = x_axis_max*.7, y = y_axis_max, hjust = "left",vjust=1,size=4, #...
             label = paste("Strategy     ", #...
                           "\nSimulations ",
                           "\nInital Wager Odds ",
                           "\nBonus Bet Odds ",
                           "\nHedge Initial Bet?",
                           "\nFree Bets Converted ",
                           "\nFree Bets Bet ",
                           
                           "\n ",
                           "\nE(profit)      ",
                           "\nmin profit   ",
                           "\nmax profit  ",
                           "\np(profit)     ",
                           "\np(loss)       ")) +
    
    #stats annotation values
    annotate(geom = "text", x = x_axis_max*.7+x_axis_max*.23, y = y_axis_max, hjust = "left",vjust=1,size=4, #...
             label = paste("| ","kelly multiplier = 1", #...
                           "\n| ",n,
                           "\n| -150",
                           "\n| +400",
                           "\n| yes",
                           "\n| 2",
                           "\n| 3",
                           "\n ",
                           "\n| $",average_profit,
                           "\n| $",min_profit,
                           "\n| $",max_profit,
                           "\n| ",p_profit,
                           "\n| ",p_loss))
  

  #  my_grob <- arrangeGrob(distribution_plot, profit_data_table,assumption_table, as.table=TRUE,layout_matrix =lay)
  ggsave(file=paste("money_gram_ev_betting_strategy",".png"), distribution_plot,width = 12,height=7,units="in")
  
  
}################################################################################################################################
#-------------------------------------------------------------------------------------------------------------------------------------------------
#Function money_gram makes a density histogram of the input profit vector

#inputs
#profit vector

#outputs 

#notes on improvements and additions to function
#none
#---------------------------------------------
money_gram <- function(n = 10000,strategy = "static0"){
  
  #load CRAN packages
  library(ggplot2)
  library(viridis)
  library(plyr)
  library(palmerpenguins)
  library(gridExtra)
  library(grid)
  
  #load non-CRAN packages (had to load from GITHUB)
  library(ggpattern)
  
  #initialize variables
  #profit vector
  profit_vector <- c(0,nrow = n, ncol = 1)
  #simulation n attempts of strategy and record profits to profit_vector
  for (i in 1:n){
    #profit_vector[i] <- wynnbet_static(strategy)
    profit_vector[i] <- maxim_strategy()
  }
  
  #because rounding errors were making the graphs bad
  profit_vector <-round(profit_vector,2)
  
  #create a profit_df dataframe of profit frequencies
  profit_df <- data.frame(count(profit_vector))
  
  #add column to profit_df with the probability of each profit outcome
  profit_df$probability <- profit_df$freq/sum(profit_df$freq)
  
  #because rounding errors were making the graphs bad
  profit_df$probability <-round(profit_df$probability,3)
  
  #max probability
  probability <- max(profit_df$probability)
  
  #add column to profit_df with true/false on of the profit is positive or negative
  profit_df$pos <- profit_df$x > 0
  
  #add column for left or right justification. Set to left, then switch to right if next bar is bigger
  profit_df$bar_label_justification <- "left"
  if(nrow(profit_df) > 1){ #only do it if there's more than 1 bar
    for (i in 1:(nrow(profit_df)-1)){
      if(profit_df$probability[i] < profit_df$probability[i+1]){
        profit_df$bar_label_justification[i] <- "right"
      }
    }
  }
  
  #update column names
  colnames(profit_df) <- c("profit","frequency","probability","positive","bar_label_justification")
  
  #calculate variables needed for plot
  average_profit <- round(mean(profit_vector),2)
  #mean line annotation y value
  annotation_y <- max(profit_df$probability)
  #probability of profit & max profit
  p_profit <- round(sum(profit_vector > 0)/n,4)
  max_profit <- round(max(profit_df$profit),2)
  #probability of loss & min profit
  p_loss <- round(1-p_profit,4)
  min_profit <- round(min(profit_df$profit),2)
  #graph bar width
  bar_width <- (max_profit-min_profit)/nrow(profit_df)*.75
  #x axis limits - w/buffer for range
  x_axis_min <- min(profit_df$profit) - bar_width
  x_axis_max <- max(profit_df$profit) + bar_width  + 500
  # y axis limits
  y_axis_max <- max(profit_df$probability)*1.25
  #bar color (need an if for when there is no negative return - all green!)
  fill_colors <-c("darkred","darkgreen")
  if(min(profit_df$profit >0)){
    fill_colors <- "darkgreen"
  }
  #bar fill (need an if for when there is no negative return - no hash, just green!)
  fill_patterns <-c("stripe","none")
  if(min(profit_df$profit >0)){
    fill_patterns <- "none"
  }
  
  #add column for x value of the bar label
  profit_df$bar_label_x <- profit_df$profit-bar_width/2+10
  profit_df$bar_label_x[which(profit_df$bar_label_justification =="right")] <- profit_df$profit[which(profit_df$bar_label_justification =="right")]+bar_width/2-10
  
  #variable for center bar (bar where the E(profit) line will cover the bar label)
  #e_profit_bar_probability <- 
  e_profit_bar_label_probablity <- profit_df$probability[which.min(abs(average_profit - profit_df$profit))]
  
  #plot it(dataframe, aestices x,y, fill based on positive or negative profit)
  distribution_plot <- ggplot(profit_df, aes(x=profit,y=probability,fill=positive,pattern=positive)) +
    #make it a bar chart of the options
    geom_bar_pattern(stat="identity",width=bar_width,alpha =.55,color="black",
                     #bar sripe settings
                     pattern_fill = "black",
                     pattern_angle = 60,
                     pattern_density = .1,
                     pattern_spacing = .02) +
    #turn off legend for striping
    theme(legend.position = 'none') +
    
    #color red or green
    scale_fill_manual(aes(density=.9),values = fill_colors, guide="none") + 
    #pattern only for red/negative returns
    scale_pattern_manual(values=fill_patterns) +
    
    #title
    ggtitle(paste("Wynnbet Promo Distribution of Expected Profit \nStrategy:",strategy)) +
    theme(plot.title = element_text(hjust = 0.5,size=20)) +
    
    #add vertical mean line bottom (around vertical mean line bar label)
    #geom_vline(aes(xintercept=average_profit),color="darkgreen", linetype="dashed", size=.8) +
    geom_segment(aes(x = average_profit, y = 0, xend = average_profit, yend = e_profit_bar_label_probablity-y_axis_max/100), linetype = "dashed", col = "darkgreen",size=.8,alpha=.8) +
    #add vertical mean line top (around vertical mean line bar label)
    geom_segment(aes(x = average_profit, y =  e_profit_bar_label_probablity+y_axis_max/12, xend = average_profit, yend = Inf), linetype = "dashed", col = "darkgreen",size=.8,alpha=.8) +
    #mean label 
    annotate(geom = "text", label = paste("E(profit)"),x = average_profit+30, y = y_axis_max,hjust="left",color = "darkgreen", size = 4) +
    
    #bar $ return label
    geom_text(aes(label = paste("$",profit), x = bar_label_x),position=position_dodge(width=0.9), vjust=-0.35, #...
              hjust=profit_df$bar_label_justification,size=4) + #...
    
    #left justify if, right justify if  the one to the right is bigger
    #bar % return label
    geom_text(aes(label = paste("p",probability), x = bar_label_x),position=position_dodge(width=0.9), vjust=-2, #...
              hjust=profit_df$bar_label_justification,size=4) +
    
    #x limit, label, breaks, size
    xlim(x_axis_min, x_axis_max) +
    xlab("Profit ($)") +
    theme(axis.title=element_text(size=15),axis.text=element_text(size=10)) +
    
    #y limit and label & x and y label sizing
    ylim(0,y_axis_max) +
    ylab("Probability") +
    
    #stats annotation labels
    annotate(geom = "text", x = x_axis_max*.7, y = y_axis_max, hjust = "left",vjust=1,size=4, #...
             label = paste("Strategy     ", #...
                           "\nSimulations ",
                           "\nInital Wager Odds ",
                           "\nBonus Bet Odds ",
                           "\nHedge Initial Bet?",
                           "\nFree Bets Converted ",
                           "\nFree Bets Bet ",
                           
                           "\n ",
                           "\nE(profit)      ",
                           "\nmin profit   ",
                           "\nmax profit  ",
                           "\np(profit)     ",
                           "\np(loss)       ")) +
    
    #stats annotation values
    annotate(geom = "text", x = x_axis_max*.7+x_axis_max*.23, y = y_axis_max, hjust = "left",vjust=1,size=4, #...
             label = paste("| ",strategy, #...
                           "\n| ",n,
                           "\n| -150",
                           "\n| +400",
                           "\n| yes",
                           "\n| 2",
                           "\n| 3",
                           "\n ",
                           "\n| $",average_profit,
                           "\n| $",min_profit,
                           "\n| $",max_profit,
                           "\n| ",p_profit,
                           "\n| ",p_loss))
  
  ggsave(file=paste(strategy,".png"), distribution_plot,width = 12,height=7,units="in")
  
  
}################################################################################################################################

#------------------------------------------------------------------------------------------------------------------------------------------------------------
#Function money_gram makes a density histogram of the input profit vector

#inputs
#profit vector

#outputs 

#notes on improvements and additions to function
#none
#---------------------------------------------
money_gram_coin_flip <- function(n = 10000,strategy="Coin Flip Promo"){
  
  #load CRAN packages
  library(ggplot2)
  library(viridis)
  library(plyr)
  library(palmerpenguins)
  library(gridExtra)
  library(grid)
  
  #load non-CRAN packages (had to load from GITHUB)
  library(ggpattern)
  
  #initialize variables
  #profit vector
  profit_vector <- c(0,nrow = n, ncol = 1)
  #simulation n attempts of strategy and record profits to profit_vector
  for (i in 1:n){
    profit_vector[i] <-coinflip_casino_promo_return()
  }
  
  #because rounding errors were making the graphs bad
  profit_vector <-round(profit_vector,2)
  
  #create a profit_df dataframe of profit frequencies
  profit_df <- data.frame(count(profit_vector))
  
  #add column to profit_df with the probability of each profit outcome
  profit_df$probability <- profit_df$freq/sum(profit_df$freq)
  
  #because rounding errors were making the graphs bad
  profit_df$probability <-round(profit_df$probability,3)
  
  #max probability
  probability <- max(profit_df$probability)
  
  #add column to profit_df with true/false on of the profit is positive or negative
  profit_df$pos <- profit_df$x > 0
  
  #add column for left or right justification. Set to left, then switch to right if next bar is bigger
  profit_df$bar_label_justification <- "left"
  if(nrow(profit_df) > 1){ #only do it if there's more than 1 bar
    for (i in 1:(nrow(profit_df)-1)){
      if(profit_df$probability[i] < profit_df$probability[i+1]){
        profit_df$bar_label_justification[i] <- "right"
      }
    }
  }

  #update column names
  colnames(profit_df) <- c("profit","frequency","probability","positive","bar_label_justification")
  
  #calculate variables needed for plot
  average_profit <- round(mean(profit_vector),2)
  
  #mean line annotation y value
  annotation_y <- max(profit_df$probability)
  
  #probability of profit & max profit
  p_profit <- round((sum(profit_vector > 0))/n,4)
  max_profit <- round(max(profit_df$profit),2)
  
  #probability of loss & min profit
  p_loss <- round(1-p_profit,4)
  
  min_profit <- round(min(profit_df$profit),2)

  #graph bar width
  bar_width <- (max_profit-min_profit)/nrow(profit_df)*.75
  
  #x axis limits - w/buffer for range
  x_axis_min <- 150
  x_axis_max <- max(profit_df$profit) + bar_width  + 100
  
#delete this....................
  x_axis_min <- -2
  x_axis_max <- 4
  
  
  
  
  
  # y axis limits
  y_axis_max <- max(profit_df$probability)*1.25
  
  #bar color (need an if for when there is no negative return - all green!)
  fill_colors <-c("darkred","darkgreen")
  if(min(profit_df$profit >0)){
    fill_colors <- "darkgreen"
  }
  #bar fill (need an if for when there is no negative return - no hash, just green!)
  fill_patterns <-c("stripe","none")
  if(min(profit_df$profit >0)){
    fill_patterns <- "none"
  }
  
  #add column for x value of the bar label
  profit_df$bar_label_x <- profit_df$profit-bar_width/2+10
  profit_df$bar_label_x[which(profit_df$bar_label_justification =="right")] <- profit_df$profit[which(profit_df$bar_label_justification =="right")]+bar_width/2-10
  
  #variable for center bar (bar where the E(profit) line will cover the bar label)
  e_profit_bar_label_probablity <- profit_df$probability[which.min(abs(average_profit - profit_df$profit))]

  #plot it(dataframe, aestices x,y, fill based on positive or negative profit)
  distribution_plot <- ggplot(profit_df, aes(x=profit,y=probability,fill=positive,pattern=positive)) +
    #make it a bar chart of the options
    geom_bar_pattern(stat="identity",width=bar_width,alpha =.55,color="black",
                     #bar sripe settings
                     pattern_fill = "black",
                     pattern_angle = 60,
                     pattern_density = .1,
                     pattern_spacing = .02) +
    #turn off legend for striping
    theme(legend.position = 'none') +
    
    #color red or green
    scale_fill_manual(aes(density=.9),values = fill_colors, guide="none") + 
    #pattern only for red/negative returns
    scale_pattern_manual(values=fill_patterns) +

    #title
    ggtitle(paste("Con Flip Promo Distribution of Expected Profit")) +
    theme(plot.title = element_text(hjust = 0.5,size=20)) +
    
    #add vertical mean line bottom (around vertical mean line bar label)
      #add vertical mean line top (around vertical mean line bar label)
      geom_segment(aes(x = average_profit, y =  0, xend = average_profit, yend = Inf), linetype = "dashed", col = "darkgreen",size=.8,alpha=.8) +
      
#..... change back to y = y_axis_max, x = average_profit+5
    
    #mean label 
      annotate(geom = "text", label = paste("E(profit)"),x = average_profit+.1, y =.55 ,hjust="left",color = "darkgreen", size = 4) +
    
    
    #x limit, label, breaks, size
    xlim(x_axis_min, x_axis_max) +
    xlab("Profit ($)") +
    theme(axis.title=element_text(size=15),axis.text=element_text(size=10)) +
    
    #y limit and label & x and y label sizing
    ylim(0,y_axis_max) +
    ylab("Probability") +

    #stats annotation labels
    
#..... change back to x = x_axis_max*.8
    annotate(geom = "text", x = x_axis_max*.68, y = y_axis_max, hjust = "left",vjust=1,size=4, #...
             label = paste("Coin Flips per Promotion     ", #...
                           "\nSimulations ",
                           
                           "\n ",
                           "\nE(profit)      ",
                           "\nmin profit   ",
                           "\nmax profit  ",
                           "\np(profit)     ",
                           "\np(loss)       ")) +
    
    #stats annotation values
    annotate(geom = "text", x = x_axis_max*.8+x_axis_max*.15, y = y_axis_max, hjust = "left",vjust=1,size=4, #...
             label = paste("|  1000", #...
                           "\n| ",n,

                           "\n ",
                           "\n| $",average_profit,
                           "\n| $",min_profit,
                           "\n| $",max_profit,
                           "\n| ",p_profit,
                           "\n| ",p_loss))

  ggsave(file="coinflippromo.png", distribution_plot,width = 12,height=7,units="in")


}################################################################################################################################

#------------------------------------------------------------------------------------------------------------------------------------------------------------
#Function money_gram makes a density histogram of the input profit vector

#inputs
#profit vector

#outputs 

#notes on improvements and additions to function
#none
#---------------------------------------------
money_gram_actual <- function(n = 10000,strategy = "actual1"){
  #initialize variables
  #profit vector
  profit_vector <- c(0,nrow = n, ncol = 1)
  
  #simulation n attempts of strategy and record profits to profit_vector
  for (i in 1:n){
    #profit[i] = bet loops_per_odd(bets,odds)  
    profit_vector[i] <- wynnbet_caesars()
  }
  
  
  #because rounding errors were making the graphs bad
  profit_vector <-round(profit_vector,2)
  
  #create a profit_df dataframe of profit frequencies
  profit_df <- data.frame(count(profit_vector))
  
  #add column to profit_df with the probability of each profit outcome
  profit_df$probability <- profit_df$freq/sum(profit_df$freq)
  
  #because rounding errors were making the graphs bad
  profit_df$probability <-round(profit_df$probability,3)
  
  #max probability
  probability <- max(profit_df$probability)
  
  #add column to profit_df with true/false on of the profit is positive or negative
  profit_df$pos <- profit_df$x > 0
  
  #add column for left or right justification. Set to left, then switch to right if next bar is bigger
  profit_df$bar_label_justification <- "left"
  if(nrow(profit_df) > 1){ #only do it if there's more than 1 bar
    for (i in 1:(nrow(profit_df)-1)){
      if(profit_df$probability[i] < profit_df$probability[i+1]){
        profit_df$bar_label_justification[i] <- "right"
      }
    }
  }
  
  #update column names
  colnames(profit_df) <- c("profit","frequency","probability","positive","bar_label_justification")
  
  #calculate variables needed for plot
  average_profit <- round(mean(profit_vector),2)
  #mean line annotation y value
  annotation_y <- max(profit_df$probability)
  #probability of profit & max profit
  p_profit <- round(sum(profit_vector > 0)/n,8)
  max_profit <- round(max(profit_df$profit),2)
  #probability of loss & min profit
  p_loss <- 1-p_profit
  min_profit <- round(min(profit_df$profit),2)
  #graph bar width
  bar_width <- (max_profit-min_profit)/nrow(profit_df)*.75
  #x axis limits - w/buffer for range
  x_axis_min <- min(profit_df$profit) - 2*bar_width
  x_axis_max <- max(profit_df$profit) + 2*bar_width+350
  # y axis limits
  y_axis_max <- max(profit_df$probability)*1.25
  #bar color (need an if for when there is no negative return - all green!)
  fill_colors <-c("darkred","darkgreen")
  if(min(profit_df$profit >0)){
    fill_colors <- "darkgreen"
  }
  #percentile of actual calculation
  actual_percentile <- sum(profit_vector < 2750)/n
  
  #add column for x value of the bar label
  profit_df$bar_label_x <- profit_df$profit-bar_width/2+10
  profit_df$bar_label_x[which(profit_df$bar_label_justification =="right")] <- profit_df$profit[which(profit_df$bar_label_justification =="right")]+bar_width/2-10
  
  
  #plot it(dataframe, aestices x,y, fill based on positive or negative profit)
  my_plot <- ggplot(profit_df, aes(x=profit,y=probability,fill=positive)) +
    #remove guide for bar color fill
    guides(alpha="none") +
    #make it a bar chart of the options
    geom_bar(stat="identity",color="black",width=bar_width,alpha =.3) +
    scale_fill_manual(values = fill_colors ,guide="none") + 
    #title
    ggtitle(paste("Wynnbet + Caesars Promo Distribution of Expected Profit\nStrategy:",strategy)) +
    theme(plot.title = element_text(hjust = 0.5,size=15)) +
    
    #add vertical mean line
    geom_vline(aes(xintercept=average_profit),color="darkgreen", linetype="dashed", size=.8,alpha=.9) +
    #mean label 
    annotate(geom = "text", label = paste("E(profit) line"),x = average_profit+30, y = y_axis_max,hjust="left",color = "darkgreen", size = 4) +
    
      #add vertical actual line
    geom_vline(aes(xintercept=2750),color="black", linetype="dashed", size=.8,alpha=.6) +
    #actual label 
    annotate(geom = "text", label = paste("Actual Profit"),x = 2750+30, y = y_axis_max*.95,hjust="left",color = "black", size = 4) +
    
    #bar $ return label
    #    geom_text(aes(label = paste("$",profit), x = bar_label_x),position=position_dodge(width=0.9), vjust=-0.35, #...
    #                hjust=profit_df$bar_label_justification,size=3.25) + #...
    
    #left justify if, right justify if  the one to the right is bigger
    #bar % return label
    #    geom_text(aes(label = paste("p",probability), x = bar_label_x),position=position_dodge(width=0.9), vjust=-2, #...
    #                hjust=profit_df$bar_label_justification,size=3.25) +
    
    #x limit, label, breaks, size
  xlim(x_axis_min, x_axis_max) +
    xlab("Profit ($)") +
    theme(axis.title=element_text(size=10)) +
    
    #y limit and label & x and y label sizing
    ylim(0,y_axis_max) +
    ylab("Probability") +
    
    #stats annotation labels
    annotate(geom = "text", x = x_axis_max*.7, y =y_axis_max*.75, hjust = "left",size=4.5, #>..
             label = paste("     ", #...
                           "\nn = ",
                           "\nE(profit)      ",
                           "\nactual profit",
                           "\nactual profit %",
                           "\nmin profit possible ",
                           "\nmax profit possible",
                           "\np(min profit)     ",
                           "\np(max profit)       ",
                           "\np(profit)     ",
                           "\np(loss)       ")) +
    #stats annotation values
    annotate(geom = "text", x = x_axis_max*.7+x_axis_max*.2, y =y_axis_max*.75, hjust = "left",size=4.5, #>..
             label = paste("", #...
                           "\n",n,
                           "\n$",average_profit,
                           "\n$ 2750.00",
                           "\n",actual_percentile,
                           "\n$",min_profit,
                           "\n$",max_profit,
                           "\n",profit_df$probability[1],
                           "\n",profit_df$probability[length(profit_df$probability)],
                           "\n",p_profit,
                           "\n",p_loss))
  
  
  #ggsave(my_plot,filename = paste(strategy,"_expected_profit_distribution.png"),path ="Plots/Wynnbet Expected Return", #...
  # width = 8, height = 5, dpi = 300, units = "in", device='png')
  my_plot
  
}################################################################################################################################