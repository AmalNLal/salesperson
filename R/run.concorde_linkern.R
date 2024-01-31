#' Runs the Concorde or Linkern algorithm and returns the output.
#'
#' @param solver_path [\code{character(1)}]\cr
#'   Path to the Concorde or Linkern solver executable.
#' @param method [\code{character(1)}]\cr
#'   Method to use (e.g., "concorde" or "linkern").
#' @param instance_path [\code{character(1)}]\cr
#'   Path to the instance file to be solved.
#' @param args [\code{character(1)}]\cr
#'   Additional command-line arguments for the solver. Default is an empty string.
#' @param concorde_runtime [\code{numeric(1)}]\cr
#'   Maximum runtime limit for Concorde algorithm (in seconds). Default is 0 (no limit).
#' @return result [\code{character(n)}]\cr
#'   Output of the Concorde or Linkern algorithm.
#' @export

run.concorde_linkern = function(solver_path, method, instance_path, args="", concorde_runtime=0){
  #print(solver_path)
  execuatable <- tail(unlist(strsplit(solver_path,"/")), n=1)
  if(  !is.element(execuatable,c("concorde","linkern")) ){
    
    if(substr(solver_path, nchar(solver_path)-(1-1), nchar(solver_path)) == "/"){
      solver_path<- paste(solver_path, method, sep="")
      #print(solver_path)
    }
    else{
      solver_path<- paste(solver_path, paste("/",method, sep=""), sep="")
      #print(solver_path)
    }
    
  }
  
  if(args != ""){
    args <- unlist(strsplit( trimws(args), " " ))
  }
  print(args)

  if(method == "concorde"){
    #print("concorde")
    if(concorde_runtime != 0){
      args <- c( paste(concorde_runtime ,"s",sep = ""), solver_path ,args, instance_path )
      print(args)
      result <- system2("timeout", args = args,stdout = T, stderr = T)
      return(result)
    }
    else{
      print(solver_path)
      args <- c(args, instance_path)
      result<- system2(solver_path, args= args ,stdout = T, stderr = T)
      return(result)
    }
  }
  else if(method == "linkern"){
    args <- c(args, instance_path)
    result <- system2(solver_path, args=args, stdout = T, stderr = T)
    return(result)
  }
  else{
    stop("ERROR: method must be either 'concorde' or 'linkern' ")
  }
}

#run.concorde_linkern(solver_path="../concorde/TSP/", method = "concorde", instance_path = "../dataset/att532.tsp", args=" -x -v -V", concorde_runtime = 2)
#run.concorde_linkern(solver_path="../concorde/TSP/", method = "linkern", instance_path = "../dataset/att532.tsp", args="", concorde_runtime = 2)

