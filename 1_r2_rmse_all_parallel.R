###########################################
##### models built ###### #################
##### NATURE COM PAPER SUBMISSION CODE ####
###########################################
### AUTHOR: DBPR #############
###########################################

library('foreach')
library('doParallel')
library('parallel')
library('doSNOW')

#setwd("C:/Users/dXXXXX/Documents/Purdue Projects/Monthly Analysis/Models 1/Bart_select")
setwd("C:/Users/DXXXXX/Documents/Purdue/Monthly Analysis/Models 1/Bart_select")

source("03A_bart_package_cross_validation.R")
source("03B_bart_package_vimp.R")
load("climate_states_s.RData")
load("select_index.RData")
load("parameters_sel.RData")
load("parameters_air.RData")

# create the cluster workers
cl <- makeCluster(detectCores() -1 ,outfile="")
registerDoSNOW(cl)
clusterEvalQ(cl, library(bartMachine))
clusterEvalQ(cl, options(java.parameters = "-Xmx4000m"))
clusterEvalQ(cl, set_bart_machine_num_cores(4))

bart_pl <- function (x, y, parR) {
  
  bart <- k_fold_cv(x, y, num_trees=parR[1], q=parR[2], nu=parR[3], k=parR[4],k_folds = 10, serialize = T)
  
  results <- cbind(bart$PseudoRsq, bart$rmse)
  
  return(results)
}

results.air <- data.frame(numeric(48), numeric(48))
results.sel <- data.frame(numeric(48), numeric(48))
wilcox.r2 <- data.frame(numeric(48))
wilcox.rmse <- data.frame(numeric(48))

for (i in 1:48){
  
  sel <- foreach(j = 1:20,.combine=rbind) %dopar%
  {
    
    selected <- bart_pl(climate.states.s[[i]][,c(4,df.s[[i]])][,-1],climate.states.s[[i]][,c(4,df.s[[i]])][,1], par.sel[i,])
    air <- bart_pl(as.data.frame(climate.states.s[[i]][,c(4,5)][,-1]),climate.states.s[[i]][,c(4,5)][,1], results[i,])
    
    list(selected,air)
  }
  
  print('-----///////------')
  print(i)
  df.full <- do.call(rbind.data.frame, sel)
  
  results.sel[i,] <- cbind(as.data.frame(mean(df.full[1:20,1])), as.data.frame(mean(df.full[1:20,2])))
  results.air[i,] <- cbind(as.data.frame(mean(df.full[21:40,1])), as.data.frame(mean(df.full[21:40,2])))
  
  wilcox.r2[i,] <- wilcox.test(df.full[1:20,1], df.full[21:40,1])$p.value
  wilcox.rmse[i,] <- wilcox.test(df.full[1:20,2], df.full[21:40,2])$p.value
  
  
  print(results.sel)
  print(results.air)
  
}

names(results.sel) <- c('R2_sel', 'RMSE_sel')
names(results.air) <- c('R2_air', 'RMSE_air')

save(results.air, file = "results_air.RData")
save(results.sel, file = "results_sel.RData")



write.csv(results.sel,file="R2_RMSE_sel_final.csv")
write.csv(results.air,file="R2_RMSE_air_final.csv")

write.csv(cbind(wilcox.r2, wilcox.rmse), file = "wilcox.csv")

