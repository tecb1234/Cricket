library(rprojroot)
library(networkD3)

project_root <- find_root(criterion = is_rstudio_project)

load(file.path(project_root,"matches.RData"))
load(file.path(project_root,"ball_by_ball.RData"))
source(file.path(project_root,"partnership_networks.R"))

countries <- ball_by_ball_df %>%
  distinct(batting_side) %>%
  select(batting_side)
