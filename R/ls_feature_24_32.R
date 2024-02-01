#' Compute Custom Features 24 to 32
#'
#' This function computes a set of custom features (features 24 to 32) based on the solver's output.
#' It involves calling the solver with different parameters, collecting results, and calculating features.
#' The function iterates 20 times to gather statistics.
#' 
#' @param solver_path [\code{character}]\cr
#'   Path to the solver executable.
#'
#' @param instance_path [\code{character}]\cr
#'   Path to the instance file.
#'
#' @name ls_feature_24_32
#' @return
#' A vector containing the computed features.
#'
#' @examples
#' \dontrun{
#'   # Usage example (replace with actual paths)
#'   result <- feature_24_32("path/to/solver", "path/to/instance")
#' }
#' @export


##====================================LIBRARIES REQUIRED==========================================================
# Install and load the e1071 package
if (!requireNamespace("e1071", quietly = TRUE)) {
  install.packages("e1071")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}

library(dplyr)
library(e1071)

##==============================CALCULATE EACH FEATURE (MEAN,VARIATION COFFICIENT,SKEW)===========================

calculation<-function(data){
  data<-as.numeric(data)
  
  # Calculate Mean
  mean_value <- mean(data)
  
  # Calculate Standard Deviation
  sd_value <- sd(data)
  
  # Calculate Coefficient of Variation
  coefficient_of_variation <- (sd_value / mean_value) * 100
  
  # Calculate skewness
  skew_value <- skewness(data)
  
  return(list(mean_value=mean_value,coefficient_of_variation=coefficient_of_variation,skew_value=skew_value))
}

##============================FEATURE 24-26===========================================================================

steps_to_local_minima <- function(result){
  all_steps<-c()
  all_steps_local_minima<-c()
  best_cycle_lengths<-c()
  
  for( line in result)  {
    if (grepl("Steps\\s+Best:", line)) {
      # Trim whitespaces and split the line by spaces
      elements <- strsplit(trimws(line), "\\s+")[[1]]
      
      # Extract and print the first element
      steps <- elements[1]
      #cat("First Element:", all_steps, "\n")
      
      # Add the first element to the vector
      all_steps <- c(all_steps, steps)
    }
  }
  
  for (step in 2:length(all_steps)){
    if (all_steps[step]== 0){
      all_steps_local_minima<-c(all_steps_local_minima,all_steps[step-1])
    }
  }
  all_steps_local_minima<-c(all_steps_local_minima,all_steps[length(all_steps)])
  feature_24_26<-calculation(all_steps_local_minima)
  return(feature_24_26)
}
##===========================FEATURE 27-29===========================================================

distance_local_minima <- function(path) {
  
  df<- read.csv(path, header = FALSE, skip=1, col.names = c("Start","end","dist" ), sep = " ")
  
  df$start <- as.numeric(as.character(df$Start))
  df$end <- as.numeric(as.character(df$end))
  # Create a new column 'pair' to store sorted pairs
  df <- df %>%
    rowwise() %>%
    mutate(pair = toString(sort(c(start, end))))
  
  # Count occurrences of each pair
  pair_counts <- df %>%
    group_by(pair) %>%
    summarize(freq = n())
  
  # Extract frequencies for the first and remaining pairs
  first_freq <- pair_counts$freq[1]
  remaining_freq <- pair_counts$freq[-1]
  
  # Calculate the sum of frequencies for pairs from 2 to n
  sum_of_2_to_end <- sum(remaining_freq)
  
  # Calculate the sum of all frequencies
  sum_of_all_freq <- sum(pair_counts$freq)
  
  # Calculate the ratio
  ratio <- sum_of_2_to_end / sum_of_all_freq
  
  return(ratio)
}


##===============================FEATURE 30-32====================================================

prob_of_edges <-function(solution_path){
  df<- read.csv(solution_path, header = FALSE, skip=1, col.names = c("Start","end","dist" ), sep = " ")
  
  # Convert 'start' and 'end' to numeric
  df$start <- as.numeric(as.character(df$Start))
  df$end <- as.numeric(as.character(df$end))
  
  # Create a new column 'pair' to store sorted pairs
  df <- df %>%
    rowwise() %>%
    mutate(pair = toString(sort(c(start, end))))
  
  # Count occurrences of each pair
  pair_counts <- df %>%
    group_by(pair) %>%
    summarize(freq = n())
  
  prob_edge <- c()
  for(idx in 1:nrow(pair_counts)){
    prob_edge <- c(prob_edge, as.numeric(pair_counts[idx,2]/ sum(pair_counts$freq) ))
  }
  feature_30_32<-calculation(prob_edge)
  return(feature_30_32)
}

##=============================== MAIN ========================================================
ls_feature_24_32<-function(solver_path,instance_path){
  feature_27_29<-c()
  results_feature_27_29<-c()
  results<-c()
  for (i in 1:20){
    # Call the function and store the result
    #result <- system2(solver_path, args = c("-R 100" ,paste("-s",i),"-o ./solution.tsp",instance_path), stdout = TRUE, stderr = TRUE)
    result <- run.concorde_linkern(solver_path, instance_path, method="linkern" ,args = paste("-R 100 -s",i, "-o solution.tsp"))
    results<-c(results,result)
    
    results_feature_27_29<-c(results_feature_27_29, distance_local_minima("solution.tsp"))

  }
  feature_24_26<-steps_to_local_minima(results)
  feature_27_29<-calculation(results_feature_27_29)
  feature_30_32<-prob_of_edges("solution.tsp")
  
  return(c(feature_24_26,feature_27_29,feature_30_32))
}
#================================================================================

#ls_feature_24_32("/home/aryman/Desktop/R_Seminar/concorde/TSP", "../dataset/a280.tsp")
