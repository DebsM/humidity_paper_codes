
###########################################
##### population data PRE PROCESSING ######
##### NATURE COM PAPER SUBMISSION CODE ####
###########################################
### AUTHOR: DBPR #############
###########################################

library(dplyr)
library(cdlTools)
library(reshape2)
library(stringr)

#setwd("C:/Users/dmaiasil/Documents/Purdue Projects/Monthly Analysis/Pop Data")


##### Run 02_Process_InputData_ElectricitySales.R before executing this code

##### Goal: Create a population vector in the same format as electricity sales vector
#####       i.e. 1st 49 entries must correspond to population data for the month 200810 (2008 October)
#####            Next 49 entries correspond to the month 200811 
#####            ....
#####            Last 49 entries correspond to the month 201612
#####       Therfore, POP vector contains 49 states * 99 months = 4851 data points


##### Pop from 1996 to 1999

pop90 <- read.table("stch-icen1990.txt", sep="", header=F)
pop90$V2 = substr(pop90$V2,1,nchar(pop90$V2)-3)
colnames(pop90) <- c("Year","FIPS","Age Group","Race-Sex","Ethnic origin","POP")
#we need to sum all the pop from the same state
pop90 <- subset(pop90, select=c("Year","FIPS","POP"))
pop90$FIPS <- as.numeric(pop90$FIPS)
pop90 <- aggregate(POP ~ FIPS, pop90, sum)
colnames(pop90)[2] <- "POPESTIMATE1990"

###### write function to optimize #####
pop91 <- read.table("stch-icen1991.txt", sep="", header=F)
pop91$V2 = substr(pop91$V2,1,nchar(pop91$V2)-3)
colnames(pop91) <- c("Year","FIPS","Age Group","Race-Sex","Ethnic origin","POP")
pop91 <- subset(pop91, select=c("Year","FIPS","POP"))
pop91$FIPS <- as.numeric(pop91$FIPS)
pop91 <- aggregate(POP ~ FIPS, pop91, sum)
colnames(pop91)[2] <- "POPESTIMATE1991"

pop92 <- read.table("stch-icen1992.txt", sep="", header=F)
pop92$V2 = substr(pop92$V2,1,nchar(pop92$V2)-3)
colnames(pop92) <- c("Year","FIPS","Age Group","Race-Sex","Ethnic origin","POP")
pop92 <- subset(pop92, select=c("Year","FIPS","POP"))
pop92$FIPS <- as.numeric(pop92$FIPS)
pop92 <- aggregate(POP ~ FIPS, pop92, sum)
colnames(pop92)[2] <- "POPESTIMATE1992"

pop93 <- read.table("stch-icen1993.txt", sep="", header=F)
pop93$V2 = substr(pop93$V2,1,nchar(pop93$V2)-3)
colnames(pop93) <- c("Year","FIPS","Age Group","Race-Sex","Ethnic origin","POP")
pop93 <- subset(pop93, select=c("Year","FIPS","POP"))
pop93$FIPS <- as.numeric(pop93$FIPS)
pop93 <- aggregate(POP ~ FIPS, pop93, sum)
colnames(pop93)[2] <- "POPESTIMATE1993"

pop94 <- read.table("stch-icen1994.txt", sep="", header=F)
pop94$V2 = substr(pop94$V2,1,nchar(pop94$V2)-3)
colnames(pop94) <- c("Year","FIPS","Age Group","Race-Sex","Ethnic origin","POP")
pop94 <- subset(pop94, select=c("Year","FIPS","POP"))
pop94$FIPS <- as.numeric(pop94$FIPS)
pop94 <- aggregate(POP ~ FIPS, pop94, sum)
colnames(pop94)[2] <- "POPESTIMATE1994"

pop95 <- read.table("stch-icen1995.txt", sep="", header=F)
pop95$V2 = substr(pop95$V2,1,nchar(pop95$V2)-3)
colnames(pop95) <- c("Year","FIPS","Age Group","Race-Sex","Ethnic origin","POP")
pop95 <- subset(pop95, select=c("Year","FIPS","POP"))
pop95$FIPS <- as.numeric(pop95$FIPS)
pop95 <- aggregate(POP ~ FIPS, pop95, sum)
colnames(pop95)[2] <- "POPESTIMATE1995"

pop96 <- read.table("stch-icen1996.txt", sep="", header=F)
pop96$V2 = substr(pop96$V2,1,nchar(pop96$V2)-3)
colnames(pop96) <- c("Year","FIPS","Age Group","Race-Sex","Ethnic origin","POP")
pop96 <- subset(pop96, select=c("Year","FIPS","POP"))
pop96$FIPS <- as.numeric(pop96$FIPS)
pop96 <- aggregate(POP ~ FIPS, pop96, sum)
colnames(pop96)[2] <- "POPESTIMATE1996"

pop97 <- read.table("stch-icen1997.txt", sep="", header=F)
pop97$V2 = substr(pop97$V2,1,nchar(pop97$V2)-3)
colnames(pop97) <- c("Year","FIPS","Age Group","Race-Sex","Ethnic origin","POP")
pop97 <- subset(pop97, select=c("Year","FIPS","POP"))
pop97$FIPS <- as.numeric(pop97$FIPS)
pop97 <- aggregate(POP ~ FIPS, pop97, sum)
colnames(pop97)[2] <- "POPESTIMATE1997"

pop98 <- read.table("stch-icen1998.txt", sep="", header=F)
pop98$V2 = substr(pop98$V2,1,nchar(pop98$V2)-3)
colnames(pop98) <- c("Year","FIPS","Age Group","Race-Sex","Ethnic origin","POP")
pop98 <- subset(pop98, select=c("Year","FIPS","POP"))
pop98$FIPS <- as.numeric(pop98$FIPS)
pop98 <- aggregate(POP ~ FIPS, pop98, sum)
colnames(pop98)[2] <- "POPESTIMATE1998"

pop99 <- read.table("stch-icen1999.txt", sep="", header=F)
pop99$V2 = substr(pop99$V2,1,nchar(pop99$V2)-3)
colnames(pop99) <- c("Year","FIPS","Age Group","Race-Sex","Ethnic origin","POP")
pop99 <- subset(pop99, select=c("Year","FIPS","POP"))
pop99$FIPS <- as.numeric(pop99$FIPS)
pop99 <- aggregate(POP ~ FIPS, pop99, sum)
colnames(pop99)[2] <- "POPESTIMATE1999"


pop9099 <- cbind(pop90,pop91[-1],pop92[-1],pop93[-1],pop94[-1],pop95[-1],pop96[-1],pop97[-1],pop98[-1],pop99[-1])


pop9099$FIPS <- fips(pop9099$FIPS, to = "Name")
colnames(pop9099)[1] <- "NAME"

#removing DC, Alaska and Hawaii
pop9099 <- pop9099[c(-9,-2,-12),]
pop9099[7,1] <- "Delaware"

######## Population data from 2011 to 2016 for all states into a data frame
pop1016 <- read.csv(file = "02A_nst-est2016-alldata.csv", header=TRUE, stringsAsFactors = F)
pop1016 <- subset(pop1016, STATE >= 1, select=c(NAME,POPESTIMATE2011:POPESTIMATE2016))
pop1016 <- pop1016[-52,] #Remove: Puerto Rico

##### Population data from 2000 to 2009 for all states into a data frame. 
pop0010 <- read.csv(file = "02A_st-est2000-2010_int-agesex.csv", header=TRUE, stringsAsFactors = F)
pop0010 <- subset(pop0010, STATE >= 1 & (SEX ==0 & AGE ==999), select=c(NAME,POPESTIMATE2000:POPESTIMATE2010))

##### Combine 2000-10 and 2011-2016 data frames
##### Used 2010 data from 2000 to 2010 series to be consistent with the data used in 8states model
popul <- cbind(pop0010[,-12], pop1016[,-1])
popul <- popul[!(popul$NAME=="Alaska"|popul$NAME=="Hawaii"|popul$NAME == "District of Columbia"),]#Remove States that we don't consider in our analysis

popul <- inner_join(popul, pop9099)

sNames <- read.csv("02A_StateNameCodes.csv", header = TRUE, stringsAsFactors = FALSE)
sNames <- sNames[,c(3,4)]

colnames(sNames) <- c("NAME", "State")

popul <- inner_join(popul, sNames)

popul <- popul[,c(29,2:28)]

popul <- popul[,c(1,19:28,2:18)]

popul <- popul[rep(seq_len(nrow(popul)), each=12),]


popul$Months <- rep(c(1:12),48)
popul <- popul[,c(1,29,2:28)]
popul <- popul[,-2]



popul.f <- melt(popul, by=State)
popul.f$Month <- rep(1:12,48*27)


popul.f$variable <- str_sub(popul.f$variable, 12)
popul.f$variable <- as.numeric(popul.f$variable)

colnames(popul.f) <- c("State", "Year", "Pop", "Month")

popul.f <- arrange(popul.f, popul.f$Year, popul.f$Month, popul.f$State) 

colnames(popul.f) <- c("State", "Year", "Pop", "Month")

pop.summer <- popul.f %>% 
  filter(Month %in% c(5:8))

pop.summer <- pop.summer$Pop

pop <- popul.f$Pop




### too few inputs to interpolate the data - we will repeat the July pop for all year
inter <- popul
inter$Month <- 7
months.complete <- rep(c(1,2,3,4,5,6,8,9,10,11,12),1296)
states.complete <- rep(inter$State, 297)
to.complete <- data.frame(states.complete,months.complete)
name.col <- c("POPESTIMATE2000", "POPESTIMATE2001", "POPESTIMATE2002", "POPESTIMATE2003", "POPESTIMATE2004", "POPESTIMATE2005", "POPESTIMATE2006",
"POPESTIMATE2007", "POPESTIMATE2008", "POPESTIMATE2009", "POPESTIMATE2010", "POPESTIMATE2011", "POPESTIMATE2012", "POPESTIMATE2013", "POPESTIMATE2014", "POPESTIMATE2015",
"POPESTIMATE2016", "POPESTIMATE1990", "POPESTIMATE1991", "POPESTIMATE1992", "POPESTIMATE1993", "POPESTIMATE1994", "POPESTIMATE1995", "POPESTIMATE1996", "POPESTIMATE1997",
"POPESTIMATE1998", "POPESTIMATE1999")
to.complete[ , name.col] <- NA
colnames(to.complete)[c(1,2)] <- c("State", "Month")
inter <- rbind(to.complete,inter)
inter <- arrange(inter, inter$State, inter$Month) 
library(zoo)
inter.s <- split(inter, f=inter$State)
na.approx(inter.s$AL)
