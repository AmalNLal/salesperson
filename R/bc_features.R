#' Compute features for the TSP instance using Concorde algorithm.
#'
#' This function combines multiple features computed using Concorde algorithm for a TSP instance.
#'
#' @param path_concorde
#'   Path to the Concorde algorithm executable.
#'
#' @param instance_path
#'   Path to the TSP instance file.
#'
#' @return
#'   A vector containing computed features.
#'
#' @examples
#' \dontrun{
#'   bc_features("/path/to/concorde", "/path/to/tsp_instance")
#' }
#'
#' @export




bc_features = function(path_concorde, instance_path){
  print("feat_33_43 > bc_features")
  tryCatch({
    imp <- imp_per_cut_ub_lb_ratio(path_concorde, instance_path)
  },error=function(e){
    imp <- imp_per_cut_ub_lb_ratio(path_concorde, instance_path, runtime=120)
    
  })
  
  features = c(imp, after_probing(path_concorde, instance_path ))
  return(features)
}

#bc_features(path_concorde = "../concorde/TSP/concorde", instance_path = "../dataset/a280.tsp")

imp_per_cut_ub_lb_ratio = function(path_concorde, instance_path, normalize = FALSE, runtime=2){
  print("feat_33_43 > imp_per_cut_ub_lb_ratio")
  # Based on 2s runs of concorde
  result <- run.concorde_linkern(solver_path = path_concorde, method = "concorde", instance_path = instance_path, args = "-B -x -v", concorde_runtime = runtime)
  #print(result)
  #str(result)
  cuts<-c()
  lps<-c()
  filtered_result <- trimws(result[grep("^\\s*Add", result)])
  #cat("length of filtered_results", length(filtered_result), "\n")

  for(temp in filtered_result ){
    cut <-unlist(strsplit(temp,","))[1]
    lp  <-unlist(strsplit(temp,","))[2]
    #cat(cut, "----------", lp,"\n")
    cut <-unlist(strsplit( trimws(sub("Add", "", cut)), " " ))[1]
    #print(cut)
    cuts<-c(cuts ,as.numeric(cut))
    lps<- c(lps, as.numeric(unlist(strsplit(trimws(lp), " "))[2]))
    #cat( tail(cuts,n=1), "<---->", tail(lps, n=1), "\n" )
  }
  
  upperbound <- as.numeric( unlist(strsplit(result[grep("^\\s*Set initial upperbound", result)], " "))[5] )
  #print(upperbound)
  lowerbound <- min(lps)
  #print(lowerbound)
  #print(length(lps))
  #print(length(cuts))
  
  data <- data.frame(Cuts = cuts, LPs = lps)
  write.csv(data, file = "concorde_temp.csv", row.names = FALSE)
  if(normalize){
    data <- scale(data)
  }
  ratio <- upperbound / lowerbound
  #print(ratio)
  
  #print(upperbound)
  imp_per_cut_data <- c((upperbound-data[1,2])/data[1,1] )
  #print(imp_per_cut_data)
  for(i in 2:length(data[,2])){
    imp <- (data[i-1,2] - data[i,2]) / data[i,1]
    imp_per_cut_data <- c(imp_per_cut_data, imp)
  }
  m_imp <- mean(imp_per_cut_data)
  sd_imp <- sd(imp_per_cut_data)
  vc_imp <- (sd_imp/m_imp)*100
  skew_imp<- skew(imp_per_cut_data)
  #cat("mean=>",m_imp, "\nVC=>", vc_imp, "\nSkew=>",skew_imp,"\n")
  return( c(m_imp, vc_imp, skew_imp, ratio) )
}

after_probing = function(path_concorde, instance_path, delete_temp=FALSE){
  print("feat_33_43 > after_probing")
  if(file.exists("./tmp")){
    print("tmp folder exists")
  }
  else{
    dir.create("tmp")
  }
  run.concorde_linkern(solver_path = path_concorde, method = "concorde", instance_path = instance_path, args = "-B -s 123456 -x -X ./tmp/final_sol.lptour", concorde_runtime=120)
  #file_contents <- readLines("./tmp/final_sol.lptour")
  #print(file_contents)
  file_data <- na.omit(read.csv("./tmp/final_sol.lptour", header = F, sep=" ", col.names = c("Start","Stop","Dist")))
  non_integer <- file_data$Dist[file_data$Dist %% 1 != 0]
  percent_non_int <- length(non_integer)/length(file_data[,3])
  #print(percent_non_int)
  percent_int <- (length(file_data[,3]) - length(non_integer) )/ length(file_data[,3])
  #print(percent_int)
  
  min_non_int <- min(non_integer)
  max_non_int <- max(non_integer)
  #print(min_non_int)
  #print(max_non_int)
  quantiles_non_int <- quantile(non_integer, c(0.25,0.50,0.75))
  #print(quantiles_non_int)
  
  if(delete_temp){
    #deletes tmp folder and all it's contents.
    unlink("./tmp", recursive = TRUE)
  }
  return(c(percent_int, percent_non_int, max_non_int, min_non_int, quantiles_non_int))
  
}

#data <-after_probing(path_concorde = "../concorde/TSP/", instance_path = "../dataset/a280.tsp", delete_temp=F)


#data <- imp_per_cut_ub_lb_ratio(path_concorde = "../concorde/TSP/", instance_path = "../dataset/att532.tsp")

