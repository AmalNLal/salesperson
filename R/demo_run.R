#' Compute all hutter features.
#'
#'
#' @return
#'   Nothing
#'
#'
#' @export


demo =function(solver_path){
  datasets <- c("../dataset/brg180.tsp", "../dataset/a280.tsp")
  
  cols<-c("dataset","No_of_Cities","tour_length","TC_mean","TC_vc","TC_skew", "LMTL_mean","LMTL_vc","LMTL_skew", "IPS_mean","IPS_vc","IPS_skew",
          "STLM_mean","STLM_vc","STLM_skew", "DBLM_mean","DBLM_vc","DBLM_skew", "POELM_mean","POELM_vc","POELM_skew",
          "IPC_mean","IPC_vc","IPC_skew","Ratio_UBLB","Pct_int","Pct_nonint","Min_nonint","Max_nonint","q25_nonint","q50_nonint","q75_nonint")
  df <- data.frame(matrix(ncol = length(cols), nrow = 0))
  colnames(df) <- cols
  
  for(i in 1:length(datasets)){
    dataset<- unlist(strsplit(datasets[i],"/"))[length(unlist(strsplit(datasets[i],"/")))]
    row<-c(dataset)
    
    data <- read_TSPLIB(datasets[i])
    row <- c(row, n_of_cities(data))
    row <- c(row, tour_length(data))

    print("feat_15_23")
    feat_15_23 <- ls_feature_15_23(solver_path, datasets[i])
    row <- c(row, feat_15_23)
    
    print("feat_24_32")
    feat_24_32 <- ls_feature_24_32(solver_path, datasets[i])
    
    feat_24_32_items<-c()
    for(item in 1:9){
      feat_24_32_items <- c(feat_24_32_items, feat_24_32[[item]])
    }
    row<-c(row, feat_24_32_items)
    
    print("feat_33_43")
    feat_33_43 <- bc_features(solver_path, datasets[i])
    row<-c(row, feat_33_43[1:8], feat_33_43[[9]], feat_33_43[[10]], feat_33_43[[11]])
    
    
    print(row)
    df <- rbind(df, row)
  } 
  colnames(df) <- cols
  write.csv(df, file = "demo.csv", row.names = FALSE)
  
}

demo("../concorde/TSP")
























 
# 
# library(tidyverse)
# library(corrplot)
# library(TSP)
# 
# df <- read.csv("Cleaned spreadsheet - instance_feature_set.csv", header = T)
# cols<-c("dataset","No_of_Cities","tour_length","TC_mean","TC_vc","TC_skew", "LMTL_mean","LMTL_vc","LMTL_skew", "IPS_mean","IPS_vc","IPS_skew",
#         "STLM_mean","STLM_vc","STLM_skew", "DBLM_mean","DBLM_vc","DBLM_skew", "POELM_mean","POELM_vc","POELM_skew",
#         "IPC_mean","IPC_vc","IPC_skew","Ratio_UBLB","Pct_int","Pct_nonint","Min_nonint","Max_nonint","q25_nonint","q50_nonint","q75_nonint")
# colnames(df) <- cols
# 
# summary(df)
# write.csv(df, file="./stats_collection/final_csv.csv", row.names = F)
# 
# 
# df$IPC_mean <- as.numeric(df$IPC_mean)
# 
# str(df)
# 
# cor_cols <- df %>%
#   select(No_of_Cities, tour_length, TC_mean, LMTL_mean,STLM_mean, DBLM_mean, POELM_mean, Ratio_UBLB, Pct_int)
# 
# 
# correlation_matrix <- cor(cor_cols, method = "spearman")
# 
# # Convert the correlation matrix to a data frame
# correlation_df <- as.data.frame(correlation_matrix)
# 
# # Create a correlation plot using corrplot
# corrplot(
#   cor(cor_cols, method = "spearman"),
#   method = "square",
#   type = "upper",
#   tl.col = "black",
#   tl.cex = 1,
#   col = colorRampPalette(c("purple", "dark green"))(200)
# )
# 
# panel.cor2= function(x,y, digits =2, prefix = " ", cex.cor, ...){
#   usr <- par("usr")
#   on.exit(par(usr))
#   par(usr = c(0,1,0,1))
#   r <- abs(cor(x,y, method="spearman"))
#   txt <- format(r, digits=digits)
#   txt <- paste(prefix, txt, sep=" ")
#   if(missing(cex.cor)) cex.cor<- 0.8/strwidth(txt)
#   text(0.5,0.5, txt, cex = 2, col="blue")
# }
# panel.hist2 = function(x, ...){
#   usr <- par("usr")
#   on.exit(par(usr))
#   par(usr=c(usr[1:2], 0,1.5))
#   h <- hist(x,plot=F)
#   breaks <- h$breaks
#   nB <- length(breaks)
#   y <- h$counts
#   y <- y/max(y)
#   rect(breaks[-nB],0, breaks[-1], y, col="cyan", ...)
# }
# pairs(cor_cols, panel=function(x,y, ...){
#   points(x,y, ...)
#   abline(lm(y ~ x), col="red")
# }, pch = 19, cex.labels = 1.5, cex.axis=2, upper.panel = panel.cor2, diag.panel = panel.hist2)
# 
# 
# generate_random_datasets = function(n=5){
#   city_sizes <- c(200,300,400,500,600,700,800,900,1000)
#   df<-data.frame(
#     instance = character(0),
#     no_of_cities = numeric(0),
#     nearest_sol = numeric(0),
#     farthest_sol = numeric(0)
#   )
#   for(i in 1:n){
#     city_size <- sample(city_sizes, size=1)
#     dataset <- salesperson::generateRandomNetwork(i,name= paste("instance",i,sep = "_"))
#     #print(getNearestNeighbourFeatureSet(dataset, include.costs = T))
#     dataset.distance_matix<- TSP(dist(dataset$coordinate))
#     nearest_solution <- solve_TSP(dataset.distance_matix, method = "nearest_insertion")
#     farthest_solution <- solve_TSP(dataset.distance_matix, method = "farthest_insertion")
# 
#     df<- rbind(df, c(paste("./stats_collection/instance",i,city_size,".tsp",sep = "_"), city_size, nearest_solution, farthest_solution))
#     salesperson::exportToTSPlibFormat(dataset, paste("./stats_collection/instance",i,city_size,".tsp",sep = "_"),paste("instance",i,city_size,sep = "_"))
#   }
#   write.csv(df, "./stats_collection/randomdataset.csv",row.names = FALSE)
# }
# 
# generate_random_datasets(20)
# 
# 
