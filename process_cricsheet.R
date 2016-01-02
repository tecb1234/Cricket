library(yaml)
library(tidyr)

library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggmap)
library(ggplot2)

data <- yaml.load_file("C:/Users/Thomas/Documents/Analysis/Cricket/Cricsheet data/630951.yaml")



#### deal with match info data
#extract the file names from the data folder
files <- list.files("Cricsheet data")

# the final file is the readme, we don't want that
files <- files[1:(length(files)-1)]

files_df <- data.frame(files)

#read in all the yaml files
get_filename <- function(filepath){
  #this function extracts the filename from the filepath, in this case the bit between the '/' and the '.'
  
  #position of '/'
  slash_pos <- str_locate(filepath, '/')[[1]]
  
  #position of '.'
  dot_pos <- str_locate(filepath, '\\.')[[1]]
  
  return <- as.integer(str_sub(filepath, slash_pos + 1, dot_pos-1))
  
}
get_yaml_file <- function(filepath){
  # a wrapper function for yaml.load_file, which adds an id value to the list object
  
  match_id <- get_filename(filepath)
  yaml_file <- yaml.load_file(filepath)
  yaml_file$match_id <- match_id
  return <- yaml_file
}
yaml_files <- lapply(paste("Cricsheet data/", files_df$files, sep=""), get_yaml_file)


#for these purposes we are only interested in the $info part of each file
extract_info_df <- function(yaml_file){
  # this function takes the $info part of the file and turns it into a dataframe
  info_df <- as.data.frame(yaml_file$info, stringsAsFactors = FALSE)
  
  #add the filename as an id variable and call it a match_id
  match_id <- yaml_file$match_id
  match_id_col <- as.integer(rep(match_id, nrow(info_df)))
  info_df <- cbind(match_id_col, info_df)
  info_df <- rename(info_df, match_id = match_id_col)
  return <- info_df
}


#apply the extract_info_df function to every file and then rbind them into 1 big dataframe
info_dfs <- lapply(yaml_files, extract_info_df)
combined_info_df <- rbind_all(info_dfs)

if(!exists("cities_revised")) {load("cities.RData")}

combined_info_df <- cities_revised %>%
    left_join(combined_info_df)

#work out home and away teams
combined_info_df <- combined_info_df %>%
    mutate(home_away = ifelse(country == teams, "home", "away"))

# but if both teams are away then in reality they are both neutral

check_neutral <- function(home_away){
  #this function takes a vector of 2 entries and if both are "away" changes them to neutral
  
  if (home_away[1] == "away" && home_away[2] == "away"){
    return <- c("neutral", "neutral")
  } else{
    return <- home_away
  }
  
}

combined_info_df <- combined_info_df %>%
    group_by(match_id) %>%
    mutate(home_away1 = check_neutral(home_away)) %>%
    select(-home_away) %>%
    rename(home_away = home_away1)

#the umpire data is untidy, because it sort of implies that the umpire belongs to one of the teams, it doesn't so separate
# it out and remove from the main data frame, we'll then call this the matches_df
umpires_info <- combined_info_df %>%
    select(match_id, umpires)

matches_df <- combined_info_df %>%
    select(-umpires)

save(matches_df, file = "matches.RData")

#### deal with the ball-by-ball data from each match

##first some helper functions

clean_col_names <- function(split_col_name){
  # this function does the final stage of cleaning up the column headings, by joining together the consituent parts
  #   after we have split up the original parts
  return <- paste(split_col_name[-(1:2)], collapse="-")
}

process_delivery <- function(delivery_list){
  #this function takes the list of deliveries and converts it into a dataframe of just 1 row,
  # with consistent column headings and values for the ball and over.
  
  
  #convert to dataframe
  delivery_df <- as.data.frame(delivery_list, stringsAsFactors = FALSE)
  
  # get the current column headings
  col_names <- colnames(delivery_df)
  
  # remove the first x
  col_names <- str_sub(col_names,2,str_length(col_names))
  
  # split the column headings up so that we can access the over and ball data
  split_col_name <- str_split(col_names, pattern='\\.')  
  
  over <- data.frame(over = as.integer(rep(split_col_name[[1]][1], nrow(delivery_df))))
  ball <- data.frame(ball = as.integer(rep(split_col_name[[1]][2], nrow(delivery_df))))
  
  #strip off the over and ball, i.e everything before the second dot, and update the df column headings
  col_names <- lapply(split_col_name, clean_col_names)
  
  colnames(delivery_df) <- col_names
  
  #merge the over and ball information back in
  delivery_df <- bind_cols(as.data.frame(over),as.data.frame(ball), delivery_df)
  return <- delivery_df
}

process_innings <- function(innings_list){
  # this function applies to the process_delviery function to each delivery list in an innings
  # and joins all of the single row dataframes into one dataframe for the whole innings
  innings_df <- lapply(innings_list[[1]]$deliveries, process_delivery)
  
  innings_df <- rbind_all(innings_df)
  
  #add a column with who is batting
  batting_side <- innings_list[[1]]$team
  batting_side_df <- data.frame(batting_side = rep(batting_side, nrow(innings_df)), stringsAsFactors = FALSE)
  innings_df <- cbind(batting_side_df, innings_df)
  return <- innings_df
}

get_bowling_side <- function(batting_side, teams) {
  #this function, works out the bowling side by working out which side isn't the batting side
  
  return <- rep(teams[batting_side[[1]] != teams], length(batting_side))
}

process_match <- function(yaml_file) {
  #This function applies the process_innings function to each innings in the match 
  # to create a single dataframe and adds a match_id col
  both_innings_dfs <- lapply(yaml_file$innings[1:2], process_innings)
  
  both_innings_dfs[[1]] <- both_innings_dfs[[1]] %>%
    mutate(bat_first_chasing = "bat_first")
  both_innings_dfs[[2]] <- both_innings_dfs[[2]] %>%
    mutate(bat_first_chasing = "chasing")
  both_innings_df <- rbind_all(both_innings_dfs)
  
  match_id <- yaml_file$match_id
  match_id_col <- rep(match_id, nrow(both_innings_df))
  both_innings_df <- cbind(match_id_col, both_innings_df)
  both_innings_df <- rename(both_innings_df, match_id = match_id_col)
  both_innings_df <- both_innings_df %>%
    group_by(batting_side) %>%
    mutate(bowling_side = get_bowling_side(batting_side, yaml_file$info$teams))
}

check_long_overs <- function(ball_vector) {
  #There is an error in the raw data where overs that are longer than 10 balls, the 10th ball gets 
  # called ball 1 again.  This function checks overs to make sure that this doesn't happen
  
  # see if there is more than one ball 1
  ball_vector <- as.numeric(ball_vector)
  
  num_ball_ones <- length(ball_vector[ball_vector == 1])
  
  if (num_ball_ones > 1){
    #we potentially have a problem, but need to check we don't have a double entry no ball
    if(ball_vector[2] !=1){
      #we do have a problem
      ball_vector[ball_vector == 1] <- 10
      ball_vector[1] <- 1
      if(ball_vector[2] == 10){ball_vector[2] <- 1}
    }
  }
  return <- ball_vector
}
#so now lets do get a massive dataframe with the ball by ball data for each match

ball_by_ball_dfs <- lapply(yaml_files, process_match)
ball_by_ball_df <- rbind_all(ball_by_ball_dfs)


# clean up the data to sort out the issue where the 10th ball of the over gets called the 1st again

ball_by_ball_df <- ball_by_ball_df %>%
  group_by(match_id, batting_side, over) %>%
  mutate(ball1 = check_long_overs(ball)) %>%
  select(-ball) %>%
  rename(ball = ball1)

  
# and then reorder
ball_by_ball_df <- ball_by_ball_df[c(1:3, length(ball_by_ball_df), 4:(length(ball_by_ball_df)-1))]

#add 2 columns which show who won.
#first separate the data from the main matches_df

winners <- matches_df %>%
  select(match_id, outcome.winner) %>%
  distinct(match_id)

ball_by_ball_df <- ball_by_ball_df %>%
  left_join(winners) %>%
  mutate(batter_wins = outcome.winner == batting_side,
         bowler_wins = !(outcome.winner == batting_side))

#and for the moment I don't think I really care about the fielders in runouts, so lets just get a dataframe
# where there is one row per ball

ball_by_ball_df <- ball_by_ball_df %>%
  select(-`wicket-fielders`) %>%
  distinct(match_id, batting_side, over, ball)

# I want to work out which wicket/partnership was going on
get_wickets <- function(wicket_kind) {
  if(length(wicket_kind) == 1){wickets <- 1}
  else{
    
    wickets <- rep(1, length(wicket_kind))
    for (ball in 1:(length(wicket_kind)-1)){
      if(is.na(wicket_kind[ball])){
        #no wicket
        wickets[ball+1] <- wickets[ball]
      }else{
        #wicket
        wickets[ball+1] <- wickets[ball] +1
      }
      
    }
  }
  return <- wickets
}

ball_by_ball_df <- ball_by_ball_df %>%
  group_by(match_id, batting_side) %>%
  mutate(wicket = get_wickets(`wicket-kind`))

save(ball_by_ball_df, file = "ball_by_ball.RData")
