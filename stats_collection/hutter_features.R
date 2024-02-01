#' Compute all hutter features.
#'
#' @name hutter_features
#' @param dataset_folder_path
#'   Path to the datasets
#'
#' @param solver_path
#'   Path to the solver.
#'   
#' @param target_file
#'   Path to the target file.
#'
#' @return
#'   Nothing
#'
#' @examples
#' \dontrun{
#'   hutter_features("/path/to/concorde", "/path/to/tsp_instance","test.csv")
#' }
#'
#' @export


library(TSP)

hutter_features = function(dataset_folder_path, solver_path, target_file){
  tsp_files <- list.files(path = dataset_folder_path, pattern = "\\.tsp$", full.names = TRUE)
  cols<-c("dataset","No_of_Cities","tour_length","TC_mean","TC_vc","TC_skew", "LMTL_mean","LMTL_vc","LMTL_skew", "IPS_mean","IPS_vc","IPS_skew",
          "STLM_mean","STLM_vc","STLM_skew", "DBLM_mean","DBLM_vc","DBLM_skew", "POELM_mean","POELM_vc","POELM_skew",
          "IPC_mean","IPC_vc","IPC_skew","Ratio_UBLB","Pct_int","Pct_nonint","Min_nonint","Max_nonint","q25_nonint","q50_nonint","q75_nonint")
  # df <- data.frame(matrix(ncol = length(cols), nrow = 0))
  # colnames(df) <- cols
  
  ignore_datasets <- c("../dataset/brd14051.tsp")
  print(length(tsp_files))
  for(i in 68:70){
    
    tryCatch({
      df <- read.csv("instance_feature_set.csv", header = T)
      #colnames(df) <- cols
      
      #all datasets
      cat("\014")
      sink("./logging.txt", append = F)
      cat(i,"----",tsp_files[i],"\n")
      sink()

      if(tsp_files[i] %in% ignore_datasets){
        next
      }
      dataset<- unlist(strsplit(tsp_files[i],"/"))[length(unlist(strsplit(tsp_files[i],"/")))]
      row<-c(dataset)
      
      tryCatch({
        data <- read_TSPLIB(tsp_files[i])
        row <- c(row, n_of_cities(data))
        row <- c(row, tour_length(data))
      }, error = function(e) {
        row <- c(row, NA, NA)
        #traceback()
      })
      
      print("feat_15_23")
      feat_15_23 <- ls_feature_15_23(solver_path, tsp_files[i])
      row <- c(row, feat_15_23)
  
  
      print("feat_24_32")
      feat_24_32 <- ls_feature_24_32(solver_path, tsp_files[i])
  
      feat_24_32_items<-c()
      for(item in 1:9){
        feat_24_32_items <- c(feat_24_32_items, feat_24_32[[item]])
      }
      row<-c(row, feat_24_32_items)
  
      print("feat_33_43")
      tryCatch({
        feat_33_43 <- bc_features(solver_path, tsp_files[i])
        row<-c(row, feat_33_43[1:8], feat_33_43[[9]], feat_33_43[[10]], feat_33_43[[11]])
      },error=function(x){
        row<-c(row, rep(NA, 11))
        
      })
      
      print(row)
      print(length(row))
      df <- rbind(df, row)
      write.csv(df, file = target_file, row.names = FALSE)
    }, error=function(e){
      message("Error: ", conditionMessage(e))
      sink("./error_datasets.txt", append = F)
      cat(i,"---",tsp_files[i],"\n")
      sink()
    })
    
  }
}

hutter_features("../dataset", "../concorde/TSP", "./stats_collection/random_get_features.csv")


