library('salesperson')
testing <- salesperson::generateRandomNetwork(100,name="Aryman_Testing")
testing
salesperson::exportToTSPlibFormat(testing, "Aryman_testing.tsp","Aryman_testing")

library(TSP)
testing.distance_matix<- TSP(dist(testing$coordinate))
testing.distance_matix

concorde_path("/home/aryman/Desktop/R_Seminar/concorde/TSP")
concorde_help()
linkern_help()
getwd()

tour_lengths <- c()
solutionlk <- solve_TSP(testing.distance_matix, method = "linkern")
for(i in 1:5){
  solutionlk <- solve_TSP(testing.distance_matix, method = "linkern")
  #print(solutionlk)
  tour_lengths<- c(tour_lengths, tour_length(solutionlk) )
}
tour_lengths
solutionconcorde <- solve_TSP(testing.distance_matix, method = "concorde")


write_TSPLIB(as.TOUR(solutionconcorde), "home/Desktop/R_Seminar/test_output_file.sol")

system2("../concorde/TSP/linkern", args = c("-o final_tour.tsp","-r 5","../dataset/att48.tsp"), stdout = T, stderr = T)
readTSPlibTOURFile("./final_tour.tsp")