###########################################
##### QUANTILE ERRORS #####################
##### NATURE COM PAPER SUBMISSION CODE ####
###########################################
### AUTHOR: DBPR #############
###########################################

#setwd("C:/Users/DXXXXX/Documents/Purdue/Month_Climate/Linear Regression")
#setwd("C:/Users/XXXXXXXX/Documents/Purdue Projects/Month_Climate/Linear Regression")

# loading the data

yhat.air <- read.table("y_hatTS_Air.csv", header = T, sep = ",", stringsAsFactors = F)
yhat.sel <- read.table("y_hatTS_sel.csv", header = T, sep = ",", stringsAsFactors = F)
y.obs <- read.table("y_obsTS.csv", header = T, sep = ",", stringsAsFactors = F)


time <- as.data.frame(yhat.air[,1], stringsAsFactors = F)
colnames(time) <- "Time"


linear.df <- function(time,air, selected, obs){
  
  data <- cbind(time,air, selected, obs)
  data <- data[order(data$obs),]
  data$error_air <- data$obs - data$air
  data$error_sel <- data$obs - data$sel
  
  return(data)
}

library(ggplot2)
library(scales)
library(reshape2)
library(zoo)


#mse_air_all <- list()
#rmse_sel_all <- list()
#rmse_air_55 <- list()
#rmse_sel_55 <- list()
#rmse_air_75 <- list()
#rmse_sel_75 <- list()
#rmse_air_90 <- list()
#rmse_sel_90 <- list()

rmse <- data.frame(numeric(48), numeric(48), numeric(48), numeric(48), numeric(48), numeric(48), numeric(48), numeric(48))
colnames(rmse) <- c("Air_all", "Sel_all", "Air_55", "Sel_55", "Air_75", "Sel_75", "Air_90", "Sel_90")


for (i in 1:48) {
  
  state.name <- colnames(yhat.air)[i+1]
  
  df <- linear.df(time, yhat.air[,i+1], yhat.sel[,i+1], y.obs[,i+1])
  
  df.air <- as.data.frame(cbind(df$obs, df$error_air))
  df.sel <- as.data.frame(cbind(df$obs, df$error_sel))
  
  colnames(df.air) <- c("energy_sales_obs", "error_air")
  colnames(df.sel) <- c("energy_sales_obs", "error_sel")
  
  
  df.air <- df.air[order(df.air$energy_sales_obs),]
  df.sel <- df.sel[order(df.sel$energy_sales_obs),]
  
  # saving the datafile by state
  filename <- paste0("errors_ordered", state.name, ".csv")
  write.csv(cbind(df.air, df.sel), file = filename, row.names = F)
  
  # errors
  # change from the other code we here just want a csv to plot in tableau and not a linear regression plot
  
  # error all
  rmse[i,1] <- sqrt(sum(df.air$error_air^2)/nrow(df.air))
  rmse[i,2] <- sqrt(sum(df.sel$error_sel^2)/nrow(df.sel))
  
  # error 55
  rmse[i,3] <- sqrt(sum(df.air$error_air[54:108]^2)/length(df.air$error_air[54:108]))
  rmse[i,4] <- sqrt(sum(df.sel$error_sel[54:108]^2)/length(df.sel$error_sel[54:108]))
  
  # error 75
  rmse[i,5] <- sqrt(sum(df.air$error_air[81:108]^2)/length(df.air$error_air[81:108]))
  rmse[i,6] <- sqrt(sum(df.sel$error_sel[81:108]^2)/length(df.sel$error_sel[81:108]))

  
  # error 90
  rmse[i,7] <- sqrt(sum(df.air$error_air[97:108]^2)/length(df.air$error_air[97:108]))
  rmse[i,8] <- sqrt(sum(df.sel$error_sel[97:108]^2)/length(df.sel$error_sel[97:108]))
  
}

write.csv(rmse, file = "RMSE_all.csv", row.names = F)


