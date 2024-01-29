library('salesperson')
library(e1071)

dataset_path <- "../ALL_tsp/dsj1000.tsp"

linker_path <- "../concorde/TSP/linkern"

number_of_iterations <- 20

all_tour_costs <- NULL
all_improvements_per_step <- NULL
all_tour_lengths_local_minima <- NULL

for (x in 1:number_of_iterations) {
  steps_to_local_minimum <- NULL
  tour_lengths_local_minima <- NULL
  tour_cost <- NULL
  total_length <- NULL
  total_steps <- NULL
  prev_best_cycle_length <- NA
  results <- system2(linker_path, args = c(dataset_path), stdout = TRUE, stderr=TRUE)
  results
  for (line in results) {
    
    # Extracting the tour cost
    if (grepl("Best cycle length: (\\d+).*", line)) {
      tour_cost <- as.numeric(gsub("[^0-9]", "", line))
    }
    
    # Extracting the total steps
    if (grepl("(\\d+).* Total Steps.", line)) {
      total_steps <- as.numeric(gsub("[^0-9]", "", line))
    }
    
    # Extracting steps to local minimum (Feature 24)
    if (grepl("\\s+(\\d+) Steps.*Best:", line)) {
      steps_to_local_minimum <- c(steps_to_local_minimum, as.numeric(gsub("\\s+(\\d+) Steps.*Best:.*", "\\1", line)))
      
      # Extracting tour length of local minima (Feature 25) from the same line
      tour_lengths_local_minima <- c(tour_lengths_local_minima, as.numeric(gsub(".*Best: (\\d+).*", "\\1", line)))
    }
    
  }
  
  all_tour_costs <- c(all_tour_costs, tour_cost)
  
  all_tour_lengths_local_minima <- c(all_tour_lengths_local_minima, tour_lengths_local_minima)
  
  all_improvements_per_step <- c(all_improvements_per_step, (tour_lengths_local_minima[1] - tour_cost) / total_steps)
  
}

mean(all_tour_costs)
sd(all_tour_costs) / mean(all_tour_costs) * 100
skewness(all_tour_costs)


mean(all_tour_lengths_local_minima)
sd(all_tour_lengths_local_minima) / mean(all_tour_lengths_local_minima) * 100
skewness(all_tour_lengths_local_minima)


mean(all_improvements_per_step)
sd(all_improvements_per_step) / mean(all_improvements_per_step) * 100
skewness(all_improvements_per_step)