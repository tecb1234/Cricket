## Partnerships

library(dplyr)
library(tidyr)

partnerships_by_match <- ball_by_ball_df %>%
  group_by(match_id, batting_side, bowling_side, batsman, non_striker) %>%
  summarise(wicket = first(wicket),
            balls_faced = n(),
            runs_total = sum(`runs-total`),
            runs_batsman = sum(`runs-batsman`),
            runs_extras = sum(`runs-extras`))








