
#Run this R-script first

###############
### DATASET ###
###############

rm(list=ls())

setwd("D:/Dropbox/3_PLSC504/Paper/Survival/")

library(foreign)

#House data
House <- read.dta("Data/berry_fowler/House.dta") #House <- read.table("Data/berry_fowler/House.tab", sep="\t", header=TRUE)
#remove unneeded years from election data
House <- subset(House, year%in%c(1984,1986,1988,1990,1992,1994,1996,1998,2000,2002,2004,2006,2008,2010))

#Election data
Election <- read.dta("Data/incumbency_advantage/JOPrepfile1.dta")
#remove unneeded years from election data
Election <- subset(Election, year%in%c(1984,1986,1988,1990,1992,1994,1996,1998,2000,2002,2004,2006,2008,2010))
Election <- subset(Election, select=c(year,state,stcd,inc,dv,dvp,dpres,midterm,cexpx,iexpx,incwin,south))

#create year_state_district variable in House dataset
library(reshape2) #for colsplit
House$district <- colsplit(House$id, pattern = "_", names = c("1","2","3","4"))[,2]
House$district<- sprintf("%02d", House$district)
House$state_name <- state.name[match(House$state,state.abb)]
House$state_num <- as.numeric(as.factor(House$state_name))
House$yearstcd <- as.numeric(paste(House$year, House$state_num, House$district, sep=""))

#create year_state_district variable in Election dataset
Election$yearstcd <- as.numeric(paste(Election$year, Election$stcd, sep=""))

## Merge
House <- merge(House, Election, by = "yearstcd")
#House2 <- House2[,c(1:5,190:206)]

rm(Election)


######################
### new variables  ###
######################

##
House$year_elected <- House$year.x-2


##incumbent/challenger spending
House$iexpx2 <- House$iexpx/10000
House$cexpx2 <- House$cexpx/10000
House$iexpx2[is.na(House$iexpx)==T] <- 0
House$cexpx2[is.na(House$iexpx)==T] <- 0
###House$iexpx <- log(House$iexpx+1)
###House$cexpx <- log(House$cexpx+1)
###House$iexpxadv <- House$iexpx/House$cexpx
####House$iexpxadv[is.na(House$iexpxadv)==T] <- 1
###House$iexpxadv[House$iexpxadv==Inf] <- 1
###House <- House[complete.cases(House$iexpxadv),]

#unopposed
House$unopposed <- 0
House$unopposed[is.na(House$cexpx)==T] <- 1

#survive election?
House$dead <- 0
House$dead[House$incwin==0] <- 1

#retired
House$retired <- 0
House$retired[House$inc%in%c(2:10)] <- 1

#retired & defeated combined
House$dead2 <- House$dead + House$retired

#1982 = year 0
House$year.alt.elected <- House$year_elected-1982
House$year.alt <- House$year.x-1982

#election cycles instead of years
House$el1 <- House$year.alt.elected/2
House$el2 <- House$year.alt/2

#committees
#1 = member
#2 = subcommittee chair
#3 = ranking member
#4 = chairman

#I am simply creating an aggregate measure here. 
#The more committes a member is in/the higher he ranks...
#...the higher the measure
House$compower <- apply(House[,67:89],1,sum,na.rm = TRUE)
#plot(density(apply(House[,67:89],1,sum,na.rm = TRUE)))

#committee chairmanship, 1 if member has at least one
House$comchair <- 0
for (i in 1:nrow(House)){
  House$comchair[i][4%in%House[i,67:89]] <- 1
}

#log population
House$log_pop <- log(House$pop)

#save(file="Data/House.Rdata", House)
