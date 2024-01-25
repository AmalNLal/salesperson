library('salesperson')
library(TSP)

testing <- salesperson::generateRandomNetwork(50,name="Aryman_Testing")
testing
salesperson::exportToTSPlibFormat(testing, "Aryman_testing.tsp", "Aryman_testing")

testing.distance_matix <- TSP(dist(testing$coordinate))
testing.distance_matix

concorde_path("/home/ajayum/Documents/seminar/concorde/TSP")
linkern_help()
getwd()
solutionlk <- solve_TSP(testing.distance_matix, method = "linkern")
solutionlk


solve_TSP_for_dataset <- function(dataset)
{
  #Read dataset
  path <- "./inst/testdata"
  #path <- "../ALL_tsp"
  dataset_path<-paste(path,dataset,sep="/")
  tsp_data <- read_TSPLIB(dataset_path)
  
  #Create distance matrix
  tsp_distance_matrix <- TSP(dist(tsp_data))
  
  #solve for TSP using Lin-Kernighan
  solution <- solve_TSP(tsp_distance_matrix,method = "linkern")
}

res <- solve_TSP_for_dataset("a280.tsp")
res