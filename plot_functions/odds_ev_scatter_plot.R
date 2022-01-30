# generates and saves a scatter plot of expected profit
# of wynnbet bonus bets bet across different odds
# this gives a good visual indication of the optional +
# odds to play free bets at by viewing the EV of each of the 20+ odds

# inputs:
# n - the number of monte carlo simulations
# we want to make per odd we loop through (-150 to 2000ish)

# outputs:
# saves a file ev_scatter_plot_by_odd.png of the plot

# notes on improvements and additions to function
# how can we generalize this function to run multiple platforms' promotional offers?
# add a progress bar, instead of printing odds simulation completion to the terminal
# outsource data generation to a new function to separate data generation and plotting
#-------------------------------------------------------------------------------
odds_ev_scatter_plot <- function(n = 1000) {

  # initialize variables
  # number of odds we want to loop across
  odds_looped <- 50

  # number of bonus bet levels we want to loop across
  bonus_bets_bet_looped <- 6

  # total profit per odd variable
  total_profit_per_odd <- 0

  # matrix of EV by bonus bet odds
  e_profit_df <- data.frame(
    matrix(
      0,
      nrow = odds_looped,
      ncol = 7
    )
  )
  colnames(e_profit_df) <- c(
    "odds",
    "profit_0_bet",
    "profit_1_bet",
    "profit_2_bet",
    "profit_3_bet",
    "profit_4_bet",
    "profit_5_bet"
  )

  # set first values of the odds
  e_profit_df$odds[1:4] <- c(
    -150,
    -110,
    110,
    150
  )
  # then increment the rest by 50
  for (i in 5:odds_looped) {
    e_profit_df$odds[i] <- 150 + (i - 4) * 50
  }

  # loop and fill profit data in e_profit_df
  for (i in 1:(odds_looped)) {
    for (bonus_bets in 0:5) {
      e_profit_df[i, bonus_bets + 2] <- wynnbet_bonus_only(
  # column names for e_profit_df
  # colnames vector
  e_profit_df_colnames_vector <- rep(0, 7)
  e_profit_df_colnames_vector[1] <- "odds"
  for (i in 0:5) {
    e_profit_df_colnames_vector[i + 2] <- gsub(" ", "", paste("profit_", i, "_bet"))
  }

  # initialize dataframe of EV by bonus bet odds
  e_profit_df <- data.frame(
    matrix(
      0,
      nrow = odds_looped,
      ncol = 7
    )
  )
  # name e_profit_df columns
  colnames(e_profit_df) <- e_profit_df_colnames_vector

  # set first values of the odds
  e_profit_df$odds[1:4] <- c(
    -150,
    -110,
    110,
    150
  )
  # then increment the rest by 50
  for (i in 5:odds_looped) {
    e_profit_df$odds[i] <- 150 + (i - 4) * 50
  }

  # loop and fill profit data in e_profit_df
  for (i in 1:odds_looped) {
    for (bonus_bets in 0:5) {
      e_profit_df[i, bonus_bets + 2] <- wynnbet_bonus_only_profit(
        bonus_odds = e_profit_df$odds[i],
        bonus_bets = bonus_bets, n = n
      )
    }
    print(
      paste(
        "odds completed",
        e_profit_df$odds[i]
      )
    )
  }

  # plot the data
    # print(paste("odds completed",e_profit_df$odds[i]))
  }

  # create the plot
  ev_scatter_plot <- ggplot(
    e_profit_df,
    aes(
      x = odds,
      y = value,
      color = variable
    )
  ) +

    # title
    ggtitle(paste("Expected Profit - 6 levels of bonus convert vs bet")) +
    theme(plot.title = element_text(hjust = 0.5)) +

    # set colors for the series
    scale_fill_viridis(
      discrete = TRUE,
      option = "magma"
    ) +

    # plot all 6 y variable sets
    # LOOP THIS
    geom_point(aes(y = profit_0_bet, col = "0 bet, 5 converted")) +
    geom_point(aes(y = profit_1_bet, col = "1 bet, 4 converted")) +
    geom_point(aes(y = profit_2_bet, col = "2 bet, 3 converted")) +
    geom_point(aes(y = profit_3_bet, col = "3 bet, 2 converted")) +
    geom_point(aes(y = profit_4_bet, col = "4 bet, 1 converted")) +
    geom_point(aes(y = profit_5_bet, col = "5 bet, 0 converted")) +

    # x limit, label, breaks, size
    xlim(min(e_profit_df$odds), max(e_profit_df$odds)) +
    xlab("Bonus Bet Odds") +

    # y limit and label & x and y label sizing
    ylim(0, max(e_profit_df[, 2:7])) +
    ylab("Expected Profit ($)") +

# LOOP THIS
    # floor lines and labels for each strategy
    geom_hline(
      aes(yintercept = 1050),
      col = "gray41",
      linetype = "dashed",
      size = .8,
      alpha = .9) +
    annotate(
      geom = "text",
      label = " \nMin profit (0 bet, 5 converted): $1050",
      x = 1550,
      y = 1050,
      hjust = "left",
      color = "gray41",
      size = 3) +
    geom_hline(aes(yintercept = 1050),col = "gray41",linetype = "dashed",size = .8, alpha = .9) +
    annotate(geom = "text",label = " \nMin profit (0 bet, 5 converted): $1050",x = 1550,y = 1050,hjust = "left",color = "gray41",size = 3) +
    geom_hline(aes(yintercept = 840), col = "gray41", linetype = "dashed", size = .8, alpha = .9) +
    annotate(geom = "text", label = "Min profit (1 bet, 4 converted): $840\n ", x = 1550, y = 840, hjust = "left", color = "gray41", size = 3) +
    geom_hline(aes(yintercept = 630), col = "gray41", linetype = "dashed", size = .8, alpha = .9) +
    annotate(geom = "text", label = "Min profit (2 bet, 3 converted): $630\n ", x = 1550, y = 630, hjust = "left", color = "gray41", size = 3) +
    geom_hline(aes(yintercept = 420), col = "gray41", linetype = "dashed", size = .8, alpha = .9) +
    annotate(geom = "text", label = "Min profit (3 bet, 2 converted): $420\n ", x = 1550, y = 420, hjust = "left", color = "gray41", size = 3) +
    geom_hline(aes(yintercept = 210), col = "gray41", linetype = "dashed", size = .8, alpha = .9) +
    annotate(geom = "text", label = "Min profit (4 bet, 1 converted): $210\n ", x = 1550, y = 210, hjust = "left", color = "gray41", size = 3) +
    geom_hline(aes(yintercept = 0), col = "gray41", linetype = "dashed", size = .8, alpha = .9) +
    annotate(geom = "text", label = "Min profit (5 bet, 0 converted): $0\n ", x = 1550, y = 0, hjust = "left", color = "gray41", size = 3)

  # filename with spaces removed
  filename <- gsub(
    " ",
    "",
    paste(
      "ev_scatter_plot_by_odd_n_",
      n,
      ".png"
    )
  )

  filename <- gsub(" ", "", paste("ev_scatter_plot_by_odd_n_", n, ".png"))
  # save png file
  ggsave(
    file = filename,
    path = "plots",
    ev_scatter_plot,
    width = 12,
    height = 7,
    units = "in"
  )
  ev_scatter_plot
}