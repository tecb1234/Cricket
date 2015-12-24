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
  
  return <- str_sub(filepath, slash_pos + 1, dot_pos-1)
  
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
  match_id_col <- rep(match_id, nrow(info_df))
  info_df <- cbind(match_id_col, info_df)
  info_df <- rename(info_df, match_id = match_id_col)
  return <- info_df
}


#apply the extract_info_df function to every file and then rbind them into 1 big dataframe
info_dfs <- lapply(yaml_files, extract_info_df)
combined_info_df <- rbind_all(info_dfs)



#work out the country from the city
# need to be slightly clever about it, as the API call takes ages
# so work out the list of cities, do the API call on those and join back in

cities_raw <- combined_info_df %>%
  select(city) %>%
  distinct(city) %>%
  filter(!is.na(city)) %>%
  mutate(country = as.character(geocode(city, output = "more")$country))

#manually do Mirpur because that isn't on google list
cities_revised <- cities_raw %>%
  mutate(revised_country = ifelse(city == "Mirpur", "Bangladesh", country)) %>%
  select(-country) %>%
  rename(country = revised_country)

#Change the UK ones to find the Leve1 Admin Area i.e England or Scotland
cities_revised <- cities_revised %>%
  mutate(revised_country = ifelse(country == "United Kingdom", as.character(geocode(city, output = "more")$administrative_area_level_1), country)) %>%
  select(-country) %>%
  rename(country = revised_country)

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
  
  over <- data.frame(over = rep(split_col_name[[1]][1], nrow(delivery_df)))
  ball <- data.frame(ball = rep(split_col_name[[1]][2], nrow(delivery_df)))
  
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
  batting_side_df <- data.frame(batting_side = rep(batting_side, nrow(innings_df)))
  innings_df <- cbind(batting_side_df, innings_df)
  return <- innings_df
}

process_match <- function(yaml_file) {
  #This function applies the process_innings function to each innings in the match 
  # to create a single dataframe and adds a match_id col
  both_innings_dfs <- lapply(yaml_file$innings, process_innings)
  both_innings_df <- rbind_all(both_innings_dfs)
  
  match_id <- yaml_file$match_id
  match_id_col <- rep(match_id, nrow(both_innings_df))
  both_innings_df <- cbind(match_id_col, both_innings_df)
  both_innings_df <- rename(both_innings_df, match_id = match_id_col)
}


#so now lets do get a massive dataframe with the ball by ball data for each match

ball_by_ball_dfs <- lapply(yaml_files, process_match)
ball_by_ball_df <- rbind_all(ball_by_ball_dfs)


