#this script will do some analysis at the individual match level

source("process_cricsheet.R")

#get the win/loss record for each team
team_summary <- matches_df %>%
    group_by(teams) %>%
    summarise(games = length(teams),
              wins = sum(outcome.winner==teams, na.rm = TRUE),
              losses = sum(outcome.winner!= teams, na.rm = TRUE) - sum(is.na(outcome.winner), na.rm = TRUE),
              no_result = sum(result == "no result", na.rm = TRUE),
              tie = sum(outcome.result == "tie", na.rm= TRUE))



 