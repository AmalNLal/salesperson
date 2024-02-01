run_hutter_demo = function(){
  tsp_files <- list.files(path = "./stats_collection/", pattern = "\\.tsp$", full.names = TRUE)
  print(tsp_files)
}
run_hutter_demo()
