#this script will do some analysis at the individual match level

library(ggplot2)

source("process_cricsheet.R")

#get the win/loss record for each team
team_summary <- matches_df %>%
    group_by(teams) %>%
    summarise(games = length(teams),
              wins = sum(outcome.winner==teams, na.rm = TRUE),
              losses = sum(outcome.winner!= teams, na.rm = TRUE) - sum(is.na(outcome.winner), na.rm = TRUE),
              no_result = sum(result == "no result", na.rm = TRUE),
              tie = sum(outcome.result == "tie", na.rm= TRUE))



p <- ggplot(team_summary, aes( x = teams, y = wins)) + geom_bar(stat = "identity") +
        theme(axis.title.x = element_blank(),
              axis.text.x  = element_text(angle=90, vjust=0.3, hjust = 1, size=10))

p

#reorder the levels in teams so that the bars are in the plot are in the right order

team_summary$teams <- factor(team_summary$teams, levels = team_summary$teams[order(team_summary$wins, decreasing = TRUE)])

order(team_summary$wins)


q <- ggplot(team_summary, aes( x = teams, y = wins)) + geom_bar(stat = "identity") +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.3, hjust = 1, size=10))

q


team_summary_tidy <- team_summary %>%
  select(-games) %>%
  gather(outcome, number, -teams) %>%
  arrange(teams)

ggplot(team_summary_tidy) + 
  geom_bar(aes(x = outcome, y = number), stat = "identity", fill = "Blue") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x  = element_text(angle=90, vjust=0.3, hjust = 1, size=10)) +
  facet_wrap(~teams, ncol = 6)




test_team_summary_tidy <- team_summary_tidy %>%
  filter(teams %in% c("England", "Australia", "India", "Pakistan", "South Africa", "Sri Lanka",
                      "New Zealand", "West Indies", "Bangladesh", "Zimbabwe"))

 