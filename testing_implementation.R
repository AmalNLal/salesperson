library('salesperson')
testing <- salesperson::generateRandomNetwork(50,name="Aryman_Testing")
testing
salesperson::exportToTSPlibFormat(testing, "Aryman_testing.tsp","Aryman_testing")

library(TSP)
testing.distance_matix<- TSP(dist(testing$coordinate))
testing.distance_matix

concorde_path("/home/aryman/Desktop/R_Seminar/concorde/TSP")
concorde_help()
linkern_help()
getwd()
solutionlk <- solve_TSP(testing.distance_matix, method = "linkern")
solutionconcorde <- solve_TSP(testing.distance_matix, method = "concorde")


write_TSPLIB(as.TOUR(solutionconcorde), "home/Desktop/R_Seminar/test_output_file.sol")
