path= "home/aryman/Desktop/R_Seminar/salesperson"
setwd(path)
getwd()
# 

library('salesperson')
testing <- salesperson::generateRandomNetwork(1000,name="Aryman_Testing")
testing
salesperson::exportToTSPlibFormat(testing, "../Aryman_testing.tsp","Aryman_testing")

#result <- system2("timeout", args= c("2s" ,"../concorde/TSP/concorde", "-v" ,"-x" ,"../dataset/att532.tsp"), stdout = TRUE, stderr = TRUE)
result <- run.concorde_linkern(solver_path = "../concorde/TSP", method = "concorde", instance_path = "../dataset/att532.tsp", args = "-x -v", concorde_runtime = 2)
result
str(result)

cuts<-c()
lps<-c()
filtered_result <- trimws(result[grep("^\\s*Add", result)])
#filtered_result
#filtered_result <- filter(filtered_result, trimws)
for(temp in filtered_result ){
  cut <-unlist(strsplit(temp,","))[1]
  lp  <-unlist(strsplit(temp,","))[2]
  
  #print(cut)
  #print(lp)
  cut_temp<- unlist(strsplit(cut, " "))[c(2,3)] 
  if(cut_temp[1] == ""){
    cuts<-c(cuts ,as.numeric(cut_temp[2]) )
  }
  else{
    uts<-c(cuts ,as.numeric(cut_temp[1]) )
  }
  lps<- c(lps, as.numeric(unlist(strsplit(trimws(lp), " "))[2]))
  cat( tail(cuts,n=1), "<---->", tail(lps, n=1), "\n" )
  #cat(cuts, " ----  ",lps,"\n")
}

print(length(lps))
print(length(cuts))

data <- data.frame(Cuts = cuts, LPs = lps)
#writeLines(result, "../2second_concorde.txt")
