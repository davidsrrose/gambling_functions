# Creates a "density histogram"
# (but its actually a bar chart of probability by profit)
# Per the example outlined in the "this is not financial advice" paper

# inputs
# n = number of coin flips per promition

# outputs
# saves a jpg of the plot output to plots folder

# notes on improvements and additions to function
# none
# inputs
# NA

# outputs
# NA

# notes on improvements and additions to function
# none

#-------------------------------------------------------------------------------
coin_flip_money_gram <- function(n = 1000) {
  # initialize variables
  # profit vector
  profit_vector <- c(
    0,
    nrow = n,
    ncol = 1
  )
  profit_vector <-
    c(
      0,
      nrow = n,
      ncol = 1
    )
  # simulation n attempts of strategy and record profits to profit_vector
  for (i in 1:n) {
    profit_vector[i] <- coinflip_casino_profit()
  }

  # because rounding errors were making the graphs bad
  profit_vector <- round(profit_vector, 2)

  # create a profit_df dataframe of profit frequencies
  profit_df <- data.frame(count(profit_vector))

  # add column to profit_df with the probability of each profit outcome
  profit_df$probability <- profit_df$freq /
    sum(profit_df$freq)

  # because rounding errors were making the graphs bad
  profit_df$probability <- round(profit_df$probability, 3)

  # max probability
  probability <- max(profit_df$probability)

  # add column to profit_df with true/false
  # on of the profit is positive or negative
  profit_df$pos <- profit_df$x > 0

  # add column for left or right justification. Set to left, then switch to right if next bar is bigger
  # add column for left or right justification.
  # set to left, then switch to right if next bar is bigger
  profit_df$bar_label_justification <- "left"
  if (nrow(profit_df) > 1) { # only do it if there's more than 1 bar
    for (i in 1:(nrow(profit_df) - 1)) {
      if (profit_df$probability[i] < profit_df$probability[i + 1]) {
        profit_df$bar_label_justification[i] <- "right"
      }
    }
  }

  # update column names
  colnames(profit_df) <- c(
    "profit",
    "frequency",
    "probability",
    "positive",
    "bar_label_justification"
  )
  colnames(profit_df) <-
    c(
      "profit",
      "frequency",
      "probability",
      "positive",
      "bar_label_justification"
    )

  # calculate variables needed for plot
  average_profit <- round(mean(profit_vector), 2)

  # mean line annotation y value
  annotation_y <- max(profit_df$probability)

  # probability of profit & max profit
  p_profit <- round((sum(profit_vector > 0)) / n, 4)
  max_profit <- round(max(profit_df$profit), 2)

  # probability of loss & min profit
  p_loss <- round(1 - p_profit, 4)

  min_profit <- round(min(profit_df$profit), 2)

  # graph bar width
  bar_width <- (max_profit - min_profit) / nrow(profit_df) * .75

  # x axis limits - w/buffer for range
  x_axis_min <- 150
  x_axis_max <- max(profit_df$profit) + bar_width + 100

  # y axis limits
  y_axis_max <- max(profit_df$probability) * 1.25

  # bar color (need an if for when there is no negative return - all green!)
  fill_colors <- c("darkred", "darkgreen")
  if (min(profit_df$profit > 0)) {
    fill_colors <- "darkgreen"
  }
  # bar fill (need an if for when there is
  # no negative return - no hash, just green!)
  fill_patterns <- c("stripe", "none")
  if (min(profit_df$profit > 0)) {
    fill_patterns <- "none"
  }

  # add column for x value of the bar label
  profit_df$bar_label_x <- profit_df$profit - bar_width / 2 + 10
  # WHAT IS THIS DOING?
  profit_df$bar_label_x[which(profit_df$bar_label_justification == "right")] <-
    profit_df$profit[which(profit_df$bar_label_justification == "right")] +
    bar_width / 2 - 10

  # variable for center bar (bar where the E(profit)
  # line will cover the bar label)
  e_profit_bar_label_probablity <-
    profit_df$probability[which.min(abs(average_profit - profit_df$profit))]

  # plot it(dataframe, aestices x,y, fill based on positive or negative profit)
  distribution_plot <- ggplot(
    profit_df,
    aes(
      x = profit,
      y = probability,
      fill = positive,
      pattern = positive
    )
  ) +
    # make it a bar chart of the options
    geom_bar_pattern(
      stat = "identity", width = bar_width, alpha = .55, color = "black",
      # bar sripe settings
      pattern_fill = "black",
      pattern_angle = 60,
      pattern_density = .1,
      pattern_spacing = .02
    ) +
    # turn off legend for striping
    theme(legend.position = "none") +

    # color red or green
    scale_fill_manual(
      aes(density = .9),
      values = fill_colors,
      guide = "none"
    ) +
    # pattern only for red/negative returns
    scale_pattern_manual(values = fill_patterns) +

    # title
    ggtitle(paste("Coin Flip Promo Distribution of Expected Profit")) +
    # title theme settings
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 20
      )
    ) +

    # add vertical mean line bottom (around vertical mean line bar label)
    # add vertical mean line top (around vertical mean line bar label)
    geom_segment(
      aes(
        x = average_profit,
        y = 0,
        xend = average_profit,
        yend = Inf
      ),
      linetype = "dashed",
      col = "darkgreen",
      size = .8,
      alpha = .8
    ) +

    # mean label annotation
    annotate(
      geom = "text",
      label = paste("E(profit)"),
      x = average_profit + 5,
      y = y_axis_max,
      hjust = "left",
      color = "darkgreen",
      size = 4
    ) +

    # x limit, label, breaks, size
    xlim(
      x_axis_min,
      x_axis_max
    ) +
    xlab("Profit ($)") +
    theme(
      axis.title = element_text(size = 15),
      axis.text = element_text(size = 10)
    ) +

    # y limit and label & x and y label sizing
    ylim(0, y_axis_max) +
    ylab("Probability") +

    # stats annotation labels
    annotate(
      geom = "text",
      x = x_axis_max * .8,
      y = y_axis_max,
      hjust = "left",
      vjust = 1,
      size = 4,
      label = paste(
        "Coin Flips per Promotion     ",
        "\nSimulations ",
        "\n ",
        "\nE(profit)      ",
        "\nmin profit   ",
        "\nmax profit  ",
        "\np(profit)     ",
        "\np(loss)       "
      )
    ) +

    # stats annotation values
    annotate(
      geom = "text",
      x = x_axis_max * .8 + x_axis_max * .15,
      y = y_axis_max,
      hjust = "left",
      vjust = 1,
      size = 4,
      label = paste(
        "|  1000",
        "\n| ", n,
        "\n ",
        "\n| $", average_profit,
        "\n| $", min_profit,
        "\n| $", max_profit,
        "\n| ", p_profit,
        "\n| ", p_loss
      )
    )

  filename <-
    gsub(
      " ",
      "",
      paste(
        "coin_flip_money_gram_n_",
        n,
        "_flips.png"
      )
    )

  # save png file
  ggsave(
    file = filename,
    path = "plots",
    distribution_plot,
    width = 12,
    height = 7,
    units = "in"
  )
}