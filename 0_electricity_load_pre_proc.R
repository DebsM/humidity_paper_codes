###########################################
##### LOAD PRE PROCESSING #################
##### NATURE COM PAPER SUBMISSION CODE ####
###########################################
### AUTHOR: DBPR #############
###########################################


library(dplyr)


#setwd("C:/Users/dXXXXX/Documents/Purdue Projects/Climate sensitivity/ClimateData")

####### Electricity Sales Data read from .csv file
####### Pick out data for all states from 2008 to 2016.
####### ================================================
electricity = read.csv("sales_revenue.csv", stringsAsFactors = F, skip = 2)
electricity <- subset(electricity, Year <2017, select=c(Year, Month, State, Megawatthours) )
#electricity <- electricity[which(electricity$Year == 2008 & electricity$Month == 10)[1]:nrow(electricity),]
electricity <- electricity[!(electricity$State=="AK"|electricity$State=="HI"|electricity$State=="DC"),] # remove states not present in predictor data
##electricity <- electricity[!(electricity$Year == 2013 & electricity$Month == 9),] # 201310 month missing in predictor data
#monthElec<- paste0(as.character(electricity$Year), as.character(sprintf("%02d", electricity$Month)))

electricity.summer <- electricity %>% 
  filter(Month %in% c(5:8))

###### Sales Vector
salesR <- as.numeric(gsub(",","",electricity$Megawatthours)) * 1e3 # change to kWh

salesR.summer <- as.numeric(gsub(",","",electricity.summer$Megawatthours)) * 1e3 # change to kWh
#salesC <- as.numeric(gsub(",","",electricity$Megawatthours.1))* 1e3 # change to kWh




###### Convert a state's aggregate electricity sales into percapita electricity sales
###### Monthly population data for the years 2008 to 2016 is processed in seperate R fiile
######=================================================================================
#wdir <- ("C:/Users/dXXXX/Documents/Purdue Projects/Climate sensitivity/ClimateData")
#source("02A_PopulationDataProcessing.R")

#both in alphabetical order of states; year; and month
salesR <- salesR/pop

salesR.summer <- salesR.summer/pop.summer
#salesC <- salesC/pop
#setwd(wdir)

###### Store non detrended i.e. raw per capita sales data for plotting purposes
salesR_raw <- salesR; #salesC_raw <- salesC
stCode <- "CA"; #"ND" or "WA" "IN" "NY" <- Choose any state to check the detrending accuracy
#y_salesR <- aggregate(data.frame(salesR,salesC), by = list(monthElec), FUN = mean)$salesR
y_salesR <- salesR[electricity$State==stCode];#y_salesC <- salesC[electricity$State==stCode];




###### Deterending the Sales data. Specifically, the response varaiable: Per capita sales.
###### Detrending methodology as explained in the paper: Sailor and Munoz(1997)
######==========================================================================
yearly <- aggregate(salesR, by=list(electricity$State, electricity$Year), sum) # sales in each year for every state
annualmean <- aggregate(yearly[3], yearly[2], mean)
#yearly[yearly$Group.2 == 2008,3] <- yearly[yearly$Group.2 == 2009,3] # 2008 had entries for only 3 months. so copy annualsum of 2009 (and hence we would have copied adjFactor of 2009 in subsequent steps).
adjfactor_yearlist <-  split( yearly[,3]/rep(annualmean[,2], length.out = nrow(yearly)) , yearly[,2] ) # Every year's adjustment factor of 49 states fall into one list element.
adjfactor_yearlist <- lapply(adjfactor_yearlist, FUN = rep, 12) # Every year's adjustment factor of 49 states, repreated 12 times. First 49 elements inside first list element has 2008 Jan's adjFactor for 49 states.
adjfactor <- Reduce(c, adjfactor_yearlist) #unlist(adjfactor_yearlist) #unlist returns named vector  # Combines all months data for all 49 states into single vector
#adjfactor <- adjfactor[-(1:441)]# sales data starts from oct'08. So, remove jan'08 to sep'08 data.

salesR <- salesR/adjfactor

setwd("C:/Users/dmaiasil/Documents/Purdue Projects/Monthly Analysis/Pop Data")
save(salesR, file="salesResiAdjusted.RData")


yearly.s <- aggregate(salesR.summer, by=list(electricity.summer$State, electricity.summer$Year), sum) # sales in each year for every state
annualmean.s <- aggregate(yearly.s[3], yearly.s[2], mean)
#yearly[yearly$Group.2 == 2008,3] <- yearly[yearly$Group.2 == 2009,3] # 2008 had entries for only 3 months. so copy annualsum of 2009 (and hence we would have copied adjFactor of 2009 in subsequent steps).
adjfactor_yearlist.s <-  split( yearly.s[,3]/rep(annualmean.s[,2], length.out = nrow(yearly.s)) , yearly.s[,2] ) # Every year's adjustment factor of 49 states fall into one list element.
adjfactor_yearlist.s <- lapply(adjfactor_yearlist.s, FUN = rep, 4) # Every year's adjustment factor of 49 states, repreated 12 times. First 49 elements inside first list element has 2008 Jan's adjFactor for 49 states.
adjfactor.s <- Reduce(c, adjfactor_yearlist.s) #unlist(adjfactor_yearlist) #unlist returns named vector  # Combines all months data for all 49 states into single vector
#adjfactor <- adjfactor[-(1:441)]# sales data starts from oct'08. So, remove jan'08 to sep'08 data.

salesR.summer <- salesR.summer/adjfactor.s

setwd("C:/Users/dmaiasil/Documents/Purdue Projects/Monthly Analysis/Pop Data")
save(salesR.summer, file="salesResiAdjustedSummer.RData")

#yearly <- aggregate(salesC, by=list(electricity$State, electricity$Year), sum) # sales in each year for every state
#annualmean <- aggregate(yearly[!(yearly$Group.2 == 2008),3], by=list(yearly[!(yearly$Group.2 == 2008),1]), mean)
#yearly[yearly$Group.2 == 2008,3] <- yearly[yearly$Group.2 == 2009,3] # 2008 had entries for only 3 months. so copy annualsum of 2009 (and hence we would have copied adjFactor of 2009 in subsequent steps).
#adjfactor_yearlist <-  split( yearly[,3]/rep(annualmean[,2], length.out = nrow(yearly)) , yearly[,2] ) # Every year's adjustment factor of 49 states fall into one list element.
#adjfactor_yearlist <- lapply(adjfactor_yearlist, FUN = rep, 12) # Every year's adjustment factor of 49 states, repreated 12 times. First 49 elements inside first list element has 2008 Jan's adjFactor for 49 states.
#adjfactor <- Reduce(c, adjfactor_yearlist) #unlist(adjfactor_yearlist) #unlist returns named vector  # Combine all months data for all 49 states into single vector
#adjfactor <- adjfactor[-(1:441)]# sales data starts from oct'08. So, remove jan'08 to sep'08 data.

#salesC <- salesC/adjfactor




###### Plot sales vector (for a state) before and after detrending to check if the detrending is correct
###### =================================================================================================
#y_salesR_d <- aggregate(data.frame(salesR,salesC), by = list(monthElec), FUN = mean)$salesR
y_salesR_d <- salesR[electricity$State==stCode];#y_salesC_d <- salesC[electricity$State==stCode];

par(mar=c(5,4,4,4)+0.1);plot(x=1:length(unique(monthElec)), y=y_salesR, type = "l", xlab ="Time: 1 to 99 months", ylab="Per capita Electricity Sales (kWh)", main =paste0("Residential sales for", stCode))
par(new=T)
plot(x=1:length(unique(monthElec)), y=y_salesR_d,  type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", col="red")
axis(side=4, ylim=range(y_salesR_d), col="red",col.axis="red") ;mtext("Per capita Electricity Sales (kWh)", side=4, line=3, col="red")  
legend("topleft",  legend=c("Raw", "Detrended"), lty=c(1,1), col=c("black", "red"), text.col=c("black","red"))

par(mar=c(5,4,4,4)+0.1);plot(x=1:length(unique(monthElec)), y=y_salesC, type = "l", xlab ="Time: 1 to 99 months", ylab="Per capita Electricity Sales (kWh)", main =paste0("Commercial sales for", stCode))
par(new=T)
plot(x=1:length(unique(monthElec)), y=y_salesC_d,  type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "", col="red")
axis(side=4, ylim=range(y_salesC_d), col="red",col.axis="red") ;mtext("Per capita Electricity Sales (kWh)", side=4, line=3, col="red")  
legend("topleft",  legend=c("Raw", "Detrended"), lty=c(1,1), col=c("black", "red"), text.col=c("black","red"))





###### Adjust the sales vector to have same yearly&monthly data points as climatic variables
###### Data for the month of 201309 (i.e. Sepetember2013) is not available for climatic variables
###### Possible reason: Data was not archived due to government shutdown on that month/year
###### ======================================================================================
warning("201309 Sales data is removed since 201309 (i.e. September-2013) data is missing in predictors")
salesR <- salesR[!(electricity$Year == 2013 & electricity$Month == 9)] # 201310 month missing in predictor data. So remove same month data.
salesC <- salesC[!(electricity$Year == 2013 & electricity$Month == 9)] # 201310 month missing in predictor data. So remove same month data.
monthElec <- monthElec[!(electricity$Year == 2013 & electricity$Month == 9)]
salesR_raw <- salesR_raw[!(electricity$Year == 2013 & electricity$Month == 9)]
salesC_raw <- salesC_raw[!(electricity$Year == 2013 & electricity$Month == 9)]
electricity <- electricity[!(electricity$Year == 2013 & electricity$Month == 9),]


save(list=c("salesR","salesC","monthElec","electricity"), file="02_Processed_InputData_ElectricitySales.RDATA")
