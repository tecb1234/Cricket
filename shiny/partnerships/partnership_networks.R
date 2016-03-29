## Partnerships

library(networkD3)
library(tidyr)
library(dplyr)

load("matches.RData")
load("ball_by_ball.RData")

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

countries <- batsman_at_crease %>%
  distinct(batting_side) %>%
  ungroup() %>%
  select(batting_side)

# england_nodes <- batsman_at_crease %>%
#   select(batting_side, batter, match_id) %>%
#   filter(batting_side == "England") %>%
#   filter(match_id == 211028) %>%
#   distinct(batter) %>%
#   mutate(node_key = 0:(n()-1)) %>%
#   mutate(Group = rep(1,n()))
#   
# england_links <- partnerships_by_match %>%
#   filter(batting_side == "England") %>%
#   filter(match_id == 211028) %>%
#     left_join(england_nodes, by = c("batsman" = "batter", "batting_side", "match_id")) %>%
#   rename(batter_node_key = node_key) %>%
#   left_join(england_nodes, by = c("non_striker" = "batter", "batting_side", "match_id")) %>%
#   rename(non_striker_node_key = node_key)
# 
# forceNetwork(Links = england_links,
#              Nodes = england_nodes,
#              NodeID = "batter",
#              Source = "batter_node_key", 
#              Target = "non_striker_node_key", 
#              Group = "Group", 
#              Value = "runs_total")





