## Partnerships

batsman_at_crease <- ball_by_ball_df %>%
  gather(batting_role, batter, -c(1:4, 6, 8:length(ball_by_ball_df))) %>%
  arrange(match_id, batting_side, over, ball)

#batsman_at_crease <- batsman_at_crease[c(1:5,length(batsman_at_crease)-1,length(batsman_at_crease),6:length(batsman_at_crease)-2)]

partnerships_by_match <- ball_by_ball_df %>%
  group_by(match_id, batting_side, batsman, non_striker) %>%
  summarise(balls_faced = n(),
            runs_total = sum(`runs-total`),
            runs_batsman = sum(`runs-batsman`),
            runs_extras = sum(`runs-extras`))

partnerships_summary <- ball_by_ball_df %>%
  group_by(batting_side, batsman, non_striker) %>%
  summarise(balls_faced = n(),
            runs_total = sum(`runs-total`),
            runs_batsman = sum(`runs-batsman`),
            runs_extras = sum(`runs-extras`))

library(networkD3)

england_partners <- partnerships_summary %>%
    filter(batting_side == "England")


simpleNetwork(england_partners, Source = "batsman", Target = "non_striker")

england_partners_reduced <- england_partners %>%
    filter(balls_faced > 6)

simpleNetwork(england_partners_reduced, Source = "batsman", Target = "non_striker")

england_players <- batsman_at_crease %>%
    filter(batting_side == "England") %>%
    select(batter) %>%
    distinct(batter) %>%
    mutate(node_key = 0:(n()-1)) %>%
    mutate(Group = rep(1,n()))

england_partners_links <- england_partners %>%
    left_join(england_players, by = c("batsman" = "batter")) %>%
    rename(batter_node_key = node_key) %>%
    left_join(england_players, by = c("non_striker" = "batter")) %>%
    rename(non_striker_node_key = node_key) 
                
forceNetwork(Links = england_partners_links, 
             Nodes = england_players,
             NodeID = "batter",
             Source = "batter_node_key", 
             Target = "non_striker_node_key", Group = "Group", Value = "runs_total")


england_partners_links_reduced <- england_partners %>%
  filter(runs_total > 20) %>%
  left_join(england_players, by = c("batsman" = "batter")) %>%
  rename(batter_node_key = node_key) %>%
  left_join(england_players, by = c("non_striker" = "batter")) %>%
  rename(non_striker_node_key = node_key) 

forceNetwork(Links = england_partners_links_reduced, 
             Nodes = england_players,
             NodeID = "batter",
             Source = "batter_node_key", 
             Target = "non_striker_node_key", Group = "Group", Value = "runs_total")
