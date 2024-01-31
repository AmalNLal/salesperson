#' Compute features for the TSP instance using linkern algorithm. from feat: 15-23
#'
#' This function combines multiple features computed using linkern algorithm for a TSP instance.
#' @name ls_feature_15_23
#' @param solver_path
#'   Path to the Linkern algorithm executable.
#'
#' @param instance_path
#'   Path to the TSP instance file.
#'
#' @return
#'   A vector containing computed features.
#'
#' @examples
#' \dontrun{
#'   bc_features("/path/to/linkern", "/path/to/tsp_instance")
#' }
#'
#' @export




# Load necessary library for skewness
library(e1071)
library(TSP)

calculate_stats <- function(arr) {
  # Calculate mean
  mean_value <- mean(arr, na.rm = TRUE)
  
  # Calculate standard deviation
  sd_value <- sd(arr, na.rm = TRUE)
  
  # Calculate coefficient of variation
  # Note: Coefficient of Variation = SD / Mean
  cv_value <- sd_value / mean_value
  
  # Calculate skewness
  skew_value <- skewness(arr, type = 3, na.rm = TRUE)
  
  # Return the results as an array
  return(c(mean_value, cv_value, skew_value))
}

getTourCost <- function(dataset_path, solver_path) {
  
  instance <- read_TSPLIB(dataset_path)
  instance.distance_matix<- TSP(dist(instance))
  
  concorde_path(solver_path)
  solutionlk <- solve_TSP(instance.distance_matix, method = "linkern")
  return(tour_length(solutionlk))
}


getMinTourLength <- function(dataset_path, linkern_path) {
  results <- system2(linkern_path, args = c(dataset_path), stdout = TRUE, stderr=TRUE)
  local_min_tour <- NULL
  for (line in results) {
    if (grepl("Best cycle length: (\\d+).*", line)) {
      local_min_tour <- as.numeric(gsub("[^0-9]", "", line))
    }
  }
  return(local_min_tour)
}


getImpPerStep <- function(dataset_path, linkern_path) {
  results <- system2(linkern_path, args = c(dataset_path), stdout = TRUE, stderr=TRUE)
  #results
  steps <- NULL
  costs <- NULL
  best_sol <- NULL
  imp_per_step <- NULL
  for (line in results) {
    if (grepl("\\s+(\\d+) Steps.*Best:", line)) {
      steps <- c(steps, as.numeric(gsub("\\s+(\\d+) Steps.*Best:.*", "\\1", line)))
      costs <- c(costs, as.numeric(gsub(".*Best: (\\d+).*", "\\1", line)))
    }
    if (grepl("Best cycle length: (\\d+).*", line)) {
      best_sol <- as.numeric(gsub("[^0-9]", "", line))
    }
  }
  for (i in 1:length(costs)) {
    if (best_sol == costs[i]) {
      imp_per_step <- (costs[1] - best_sol) / steps[i]
      break
    }
  }
  return(imp_per_step)
}


ls_feature_15_23<-function(solver_path, instance_path){
  tour_costs<-c()
  min_tour_lengths<-c()
  impPerStep<-c()
  linkern_path <- paste(solver_path,"/linkern", sep = "")
  for (i in 1:20){
    # Call the function and store the result
    tour_costs <- c(tour_costs, getTourCost(instance_path, solver_path))
    min_tour_lengths <- c(min_tour_lengths, getMinTourLength(instance_path,linkern_path))
    impPerStep <- c(impPerStep, getImpPerStep(instance_path,linkern_path))
  }
  feature_15_17<-calculate_stats(tour_costs)
  feature_18_20<-calculate_stats(min_tour_lengths)
  feature_21_23<-calculate_stats(impPerStep)
  
  return(c(feature_15_17,feature_18_20,feature_21_23))
}

