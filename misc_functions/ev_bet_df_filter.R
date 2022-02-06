# returns a dataframe, based on input dataframe and settings

# inputs
# min_ev - min ev of bets we want in our returned dataframe
# max_ev - max ev of bets we want in our returned dataframe
# min odds - max odd of bets we want in our returned dataframe
# max odds -min odd of bets we want in our returned dataframe
# type - type of bet we want to place "standard" or "live" or "both"

# outputs
# filtered dataframe

# notes on updates and improvements

#-------------------------------------------------------------------------------
ev_bet_df_filter <- function(min_ev = -Inf,
                             max_ev = Inf,
                             min_odd = -Inf,
                             max_odd = Inf,
                             bet_type = "both",
                             bet_df_to_filter = ev_bet_df) {

  # initialize filtered dataframe
  ev_bet_df_filtered <- data.frame(NULL)

  # select out based on min/max ev, min/max odds
  ev_bet_df_filtered <- bet_df_to_filter[
    bet_df_to_filter$ev >= min_ev &
      bet_df_to_filter$ev <= max_ev &
      bet_df_to_filter$ev_odd >= min_odd &
      bet_df_to_filter$ev_odd <= max_odd,
  ]

  # select for type of not "both"
  if (bet_type != "both") {
    ev_bet_df_filtered <- ev_bet_df_filtered[
      ev_bet_df_filtered$bet_type == bet_type,
    ]
  }

  return(ev_bet_df_filtered)
}