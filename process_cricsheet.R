library(yaml)
library(tidyr)

library(plyr)
library(dplyr)
library(stringr)

data <- yaml.load_file("C:/Users/Thomas/Documents/Analysis/Cricket/Cricsheet data/630951.yaml")



#deal with match info data
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


# convert the first innings data into a list of dataframes, one for each ball
first_innings <- lapply(data$innings[[1]]$`1st innings`$deliveries,as.data.frame)

first_innings1 <- lapply(data$innings[[1]],as.data.frame)
a <- first_innings1[[1]]

clean_up_ball <- function(ball_row_df){
  # get the current column headings
  col_names <- colnames(ball_row_df)
  
  # remove the first x
  col_names <- str_sub(col_names,2,str_length(col_names))
  
  # split the column headings up so that we can access the over and ball data
  split_col_name <- str_split(col_names, pattern='\\.')
  
  new_column_names <- lapply(split_col_name, clean_column_names)
  
  colnames(ball_row_df) <- new_column_names
  
  # extract the over and ball information from the old split up column names
  over <- data.frame(over = split_col_name[[1]][1])
  ball <- data.frame(ball = split_col_name[[1]][2])
  
  # merge in the over and ball data into the data row
  ball_row_df <- bind_cols(as.data.frame(over),as.data.frame(ball), ball_row_df)
  return(ball_row_df)
}

first_innings<- lapply(first_innings, clean_up_ball)


first_innings_df <- first_innings %>%
  bind_rows() %>%
  as.data.frame()






