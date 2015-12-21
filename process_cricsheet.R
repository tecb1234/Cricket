library(yaml)
library(tidyr)

library(plyr)
library(dplyr)
library(stringr)

data <- yaml.load_file("C:/Users/Thomas/Documents/Analysis/Cricket/Cricsheet data/630951.yaml")



#### deal with match info data
files <- list.files("Cricsheet data")
files <- files[1:(length(files)-1)]

files_df <- data.frame(files)

yaml_files <- lapply(paste("Cricsheet data/", files_df$files, sep=""), yaml.load_file)

extract_info_df <- function(yaml_file){
  info_df <- as.data.frame(yaml_file$info)
  return <- info_df
}
    
info_dfs <- lapply(yaml_files, extract_info_df)

combined_info_df <- rbind_all(info_dfs)

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
  delivery_df <- as.data.frame(delivery_list)
  
  # get the current column headings
  col_names <- colnames(delivery_df)
  
  # remove the first x
  col_names <- str_sub(col_names,2,str_length(col_names))
  
  # split the column headings up so that we can access the over and ball data
  split_col_name <- str_split(col_names, pattern='\\.')  
  
  over <- data.frame(over = split_col_name[[1]][1])
  ball <- data.frame(ball = split_col_name[[1]][2])
  
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
  return <- innings_df
}







