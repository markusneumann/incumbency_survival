
### !!! Run '504_Markus_Paper_Data2.R' first !!!

##############
###Analysis###
##############

##R version 3.3.2 (2016-10-31) -- "Sincere Pumpkin Patch"

#Be careful when running all of this code, the models with Firth correction take a while

#Credit for the code of the survival plots and the discrete-time stuff goes to Chris Zorn ;-)

#rm(list=ls())

set.seed(123)

#the working directory needs to contain the House.Rdata file created in 504_Markus_Paper_Data2.R
setwd("D:/Dropbox/3_PLSC504/Paper/Survival/Data")
load(file="House.Rdata")

library(survival)
#Create survival object
survival.H <- Surv(House$el1, House$el2, House$dead)

#Cox regression
m1H <- coxph(survival.H ~ dem + dpres + dem*dpres + majority + log_pop + log_faads + compower + comchair + iexpx2 + cexpx2 + iexpx2*cexpx2 + south,
             data = House, method="breslow")
m2H <- coxph(survival.H ~ dem + dpres + dem*dpres + majority + log_pop + log_faads + compower + comchair + log_faads*comchair + iexpx2 + cexpx2 + iexpx2*cexpx2 + south,
             data = House, method="breslow")
#summary(m1H)
#summary(m2H)

##################
#Firth correction#
##################

#Cox regression with Firth's penalized likelihood - takes a long time to run
library(coxphf)
m1Hf <- coxphf(survival.H ~ dem + dpres + dem*dpres + majority + log_pop + log_faads + compower + comchair + log_faads*compower + iexpx2 + cexpx2 + iexpx2*cexpx2 + south + unopposed, data = House)
#m1Hf

#################
#Competing risks#
#################

#defeated
survival.Hd <- Surv(House$el1, House$el2, House$dead)
#retired
survival.Hr <- Surv(House$el1, House$el2, House$retired)
#combined
survival.Hc <- Surv(House$el1, House$el2, House$dead2)

#Plot survival curves
#pdf("../Plots/figure1.pdf",6,5)
plot(survfit(survival.Hd~1),mark.time=F,lwd=c(3,1,1),
     lty=c(3,3,3),col=c("blue","blue","blue"),
     xlab="Time (in election cycles)",ylab="Survival",main="Figure 1: Survival curves")
par(new=TRUE)
plot(survfit(survival.Hr~1),mark.time=F,lwd=c(3,1,1),
     lty=c(3,3,3),col=c("red","red","red"))
par(new=TRUE)
plot(survfit(survival.Hc~1),mark.time=F,lwd=c(3,1,1),
     lty=c(1,1,1),col=c("black","black","black"))
legend("bottomleft",bty="n",inset=0.02,lty=c(3,3,1),lwd=c(3,3,3),
       col=c("blue","red","black"),c("Defeat","Retirement","Combined"))
#dev.off()


#regular cox
m1Hc <- coxph(survival.Hc ~ dem + dpres + dem*dpres + majority + log_pop + log_faads + compower + comchair + log_faads*compower + iexpx2 + cexpx2 + iexpx2*cexpx2 + south, data = House, method="breslow")
m1Hd <- coxph(survival.Hd ~ dem + dpres + dem*dpres + majority + log_pop + log_faads + compower + comchair + log_faads*compower + iexpx2 + cexpx2 + iexpx2*cexpx2 + south, data = House, method="breslow")
m1Hr <- coxph(survival.Hr ~ dem + dpres + dem*dpres + majority + log_pop + log_faads + compower + comchair + log_faads*compower + iexpx2 + cexpx2 + iexpx2*cexpx2 + south, data = House, method="breslow")
#summary(m1Hc)
#summary(m1Hd)
#summary(m1Hr)

#firth correction
m1Hfc <- coxphf(survival.Hc ~ dem + dpres + dem*dpres + majority + log_pop + log_faads + compower + comchair + log_faads*compower + iexpx2 + cexpx2 + iexpx2*cexpx2 + south + unopposed, data = House)
m1Hfd <- coxphf(survival.Hd ~ dem + dpres + dem*dpres + majority + log_pop + log_faads + compower + comchair + log_faads*compower + iexpx2 + cexpx2 + iexpx2*cexpx2 + south + unopposed, data = House)
m1Hfr <- coxphf(survival.Hr ~ dem + dpres + dem*dpres + majority + log_pop + log_faads + compower + comchair + log_faads*compower + iexpx2 + cexpx2 + iexpx2*cexpx2 + south + unopposed, data = House)
#m1Hfc
#m1Hfd
#m1Hfr


###############
#Discrete-time#
###############

## no time variable / "flat" hazard
House.logit <- glm(dead ~ dem + dpres + dem*dpres + majority + log_pop + log_faads + compower + comchair + log_faads*compower + iexpx2 + cexpx2 + iexpx2*cexpx2 + south,
                   data=House, na.action=na.exclude, family="binomial")

## linear trend
House$duration <- House$el2
House.trend <- glm(dead ~ dem + dpres + dem*dpres + majority + log_pop + log_faads + compower + comchair + log_faads*compower + iexpx2 + cexpx2 + iexpx2*cexpx2 + south
                 + duration, data=House, na.action=na.exclude, family="binomial")

## fourth-order polynomial trend
House$d2 <- House$duration^2*0.1
House$d3 <- House$duration^3*0.01
House$d4 <- House$duration^4*0.001
House.P4 <- glm(dead ~ dem + dpres + dem*dpres + majority + log_pop + log_faads + compower + comchair + log_faads*compower + iexpx2 + cexpx2 + iexpx2*cexpx2 + south
              + duration + d2 + d3 + d4, data=House, na.action=na.exclude, family="binomial")

## time dummies
House.dummy<-glm(dead ~ dem + dpres + dem*dpres + majority + log_pop + log_faads + compower + comchair + log_faads*compower + iexpx2 + cexpx2 + iexpx2*cexpx2 + south
                 + as.factor(duration), data=House, na.action=na.exclude, family="binomial")

# Predicted probabilities:
Xhats<-as.data.frame(t(c(median(House$dem), mean(House$dpres), median(House$majority),
                         mean(House$log_pop), mean(House$log_faads),
                         mean(House$compower), median(House$comchair), mean(House$iexpx2), mean(House$cexpx2),
                         median(House$south))))
Xhats<-Xhats[rep(1:nrow(Xhats),each=max(House$duration)),]
Xhats$duration<-1:max(House$duration)        
Xhats$d2<-Xhats$duration^2*0.1
Xhats$d3<-Xhats$duration^3*0.01
Xhats$d4<-Xhats$duration^4*0.001
colnames(Xhats)<-c("dem","dpres","majority","log_pop","log_faads",
                   "compower","comchair","iexpx2","cexpx2","south","duration","d2","d3","d4")

Hat.logit<-predict(House.logit,Xhats,type="response")
Hat.trend<-predict(House.trend,Xhats,type="response")
Hat.P4<-predict(House.P4,Xhats,type="response")
Hat.dummy<-predict(House.dummy,Xhats,type="response")

# Plot:
#pdf("../Plots/figure2.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Xhats$duration,Hat.logit,ylim=c(0,0.1),t="l",lwd=3,col="black",
     xlab="Time (in election cycles)",
     ylab="Predicted Probability", 
     main="Figure 2: Discrete-time: probability of being defeated")
lines(Xhats$duration,Hat.trend,lwd=3,lty=2,col="blue")
lines(Xhats$duration,Hat.P4,lwd=3,lty=3,col="green")
lines(Xhats$duration,Hat.dummy,lwd=3,lty=4,col="red")
legend("topright",inset=0.05,bty="n",
       c("No Duration Dependence","Linear Dependence",
         "Fourth-Degree Polynomial","Duration Dummies"),lty=c(1,2,3,4),
       lwd=c(3,3,3,3),col=c("black","blue","green","red"))
#dev.off()

##########
#COMBINED

## no time variable / "flat" hazard
House.logit2 <- glm(dead2 ~ dem + dpres + dem*dpres + majority + log_pop + log_faads + compower + comchair + log_faads*compower + iexpx2 + cexpx2 + iexpx2*cexpx2 + south,
                   data=House, na.action=na.exclude, family="binomial")

## linear trend
House$duration <- House$el2
House.trend2 <- glm(dead2 ~ dem + dpres + dem*dpres + majority + log_pop + log_faads + compower + comchair + log_faads*compower + iexpx2 + cexpx2 + iexpx2*cexpx2 + south
                   + duration, data=House, na.action=na.exclude, family="binomial")

## fourth-order polynomial trend
House$d2 <- House$duration^2*0.1
House$d3 <- House$duration^3*0.01
House$d4 <- House$duration^4*0.001
House.P42 <- glm(dead2 ~ dem + dpres + dem*dpres + majority + log_pop + log_faads + compower + comchair + log_faads*compower + iexpx2 + cexpx2 + iexpx2*cexpx2 + south
                + duration + d2 + d3 + d4, data=House, na.action=na.exclude, family="binomial")

## time dummies
House.dummy2<-glm(dead2 ~ dem + dpres + dem*dpres + majority + log_pop + log_faads + compower + comchair + log_faads*compower + iexpx2 + cexpx2 + iexpx2*cexpx2 + south
                 + as.factor(duration), data=House, na.action=na.exclude, family="binomial")

# Predicted probabilities:
Xhats<-as.data.frame(t(c(median(House$dem), mean(House$dpres), median(House$majority),
                         mean(House$log_pop), mean(House$log_faads),
                         mean(House$compower), median(House$comchair), mean(House$iexpx2), mean(House$cexpx2),
                         median(House$south))))
Xhats<-Xhats[rep(1:nrow(Xhats),each=max(House$duration)),]
Xhats$duration<-1:max(House$duration)        
Xhats$d2<-Xhats$duration^2*0.1
Xhats$d3<-Xhats$duration^3*0.01
Xhats$d4<-Xhats$duration^4*0.001
colnames(Xhats)<-c("dem","dpres","majority","log_pop","log_faads",
                   "compower","comchair","iexpx2","cexpx2","south","duration","d2","d3","d4")

Hat.logit2<-predict(House.logit2,Xhats,type="response")
Hat.trend2<-predict(House.trend2,Xhats,type="response")
Hat.P42<-predict(House.P42,Xhats,type="response")
Hat.dummy2<-predict(House.dummy2,Xhats,type="response")

# Plot:
#pdf("../Plots/appendix_figure2.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Xhats$duration,Hat.logit2,ylim=c(0,0.3),t="l",lwd=3,col="black",
     xlab="Time (in election cycles)",
     ylab="Predicted Probability", 
     main="Figure 4: Discrete-time: probability of defeat or retirement")
lines(Xhats$duration,Hat.trend2,lwd=3,lty=2,col="blue")
lines(Xhats$duration,Hat.P42,lwd=3,lty=3,col="green")
lines(Xhats$duration,Hat.dummy2,lwd=3,lty=4,col="red")
legend("topright",inset=0.05,bty="n",
       c("No Duration Dependence","Linear Dependence",
         "Fourth-Degree Polynomial","Duration Dummies"),lty=c(1,2,3,4),
       lwd=c(3,3,3,3),col=c("black","blue","green","red"))
#dev.off()



##################################
#discrete-time + firth correction#
##################################

library(logistf)
## no duration dependence
House.logit.f <- glm(dead2 ~ dem + dpres + dem*dpres + majority + log_pop + log_faads + compower + comchair + log_faads*compower + iexpx2 + cexpx2 + iexpx2*cexpx2 + south + unopposed,
                     data=House,na.action=na.exclude,family="binomial")

## linear trend
House.trend.f <- glm(dead2 ~ dem + dpres + dem*dpres + majority + log_pop + log_faads + compower + comchair + log_faads*compower + iexpx2 + cexpx2 + iexpx2*cexpx2 + south + unopposed
                     +duration,data=House,na.action=na.exclude,family="binomial")

## fourth-order polynomial trend
House.P4.f <- glm(dead2 ~ dem + dpres + dem*dpres + majority + log_pop + log_faads + compower + comchair + log_faads*compower + iexpx2 + cexpx2 + iexpx2*cexpx2 + south + unopposed
                  +duration+d2+d3+d4,data=House,na.action=na.exclude,
                  family="binomial")

## time dummies
House.dummy.f <- logistf(dead2 ~ dem + dpres + dem*dpres + majority + log_pop + log_faads + compower + comchair + log_faads*compower + iexpx2 + cexpx2 + iexpx2*cexpx2 + south + unopposed
                         +as.factor(duration),data=House,na.action=na.exclude,
                         family="binomial")


####################################################
#discrete-time + competing risks + firth correction#
####################################################

#multinomial

#DOESNT WORK RELIABLY
#ADDING CERTAIN VARIABLES BLOWS IT UP
#...also takes forever

#library(pmlr)
#House$threecat <- factor(House$threecat)
#House.MNL.f <- pmlr(threecat~ dem + majority + dpres + log_faads + south + unopposed + as.factor(duration), data=House)

#summary(House.MNL.f)

#House$threecat <- 0
#House$threecat[House$dead] <- 1
#House$threecat[House$retired] <- 2

#House$lnT <- log(House$seniority)
#scotus.MNL <- multinom(threecat~dem + majority + lnT + dpres + log_faads + log(pop) + south, data=House,na.action=na.omit)
#summary(scotus.MNL)


##############################################################
# regression tables + density plot for campaign expenditures #
##############################################################

## Campaign expenditures density plot for incumbent/challenger
require(reshape2)
require(ggplot2)
library(cowplot)
theme_set(theme_bw())
ggdat <- melt(data.frame(House$iexpx,House$cexpx))
levels(ggdat$variable) <- c("Challenger", "Incumbent")
ggdat$value <- ggdat$value/100000

m1 <- ggplot(ggdat, aes(x=value, color=variable))
m1 <- m1 + geom_density(fill=NA, size=.2, alpha=0.1)
m1 <- m1 + labs(y="Density", title="Figure 3: Campaign expenditures", x="Expenditures (in 100,000)") + xlim(0,50) + theme(legend.position="bottom")
m1

ggsave(file="../Plots/appendix_figure1.pdf", plot = m1, width = 6, height = 5)


## Stargazer tables

library(stargazer)

#regular cox
stargazer(m1H, m2H, single.row=T, no.space=F, digits = 2, keep.stat="N", dep.var.labels="Defeat", title="Cox proportional hazards regression model",
          covariate.labels = c("Democrat", "Presidential Vote", "Majority", "Log. Population", "FAADS",
                               "Committee Index", "Committee Chair", "Incumbent Expend.", "Challenger Expend.",
                               "South", "Dem. * Pres. Vote", "FAADS*Committee", "Inc. * Chall. Expend."))

#cox + firth correction -- DOES NOT WORK; have to do manually
#stargazer(m1Hf)

#competing risks
stargazer(m1Hc,m1Hd,m1Hr, single.row=T, no.space=F, digits = 2, keep.stat="N", dep.var.labels=c("Combined","Defeat","Retirement"),
          title="Cox proportional hazards regression model - defeat and retirement",
          covariate.labels = c("Democrat", "Presidential Vote", "Majority", "Log. Population", "FAADS",
                               "Committee Index", "Committee Chair", "Incumbent Expend.", "Challenger Expend.",
                               "South", "Dem. * Pres. Vote", "FAADS*Committee", "Inc. * Chall. Expend."))

#competing risks + firth correction -- DOES NOT WORK; have to do manually
#stargazer(m1Hfc,m1Hfd,m1Hfr)

#discrete-time
stargazer(House.logit,House.trend,House.P4,House.dummy, no.space=T, digits = 2, keep.stat="N", omit = "as.factor",
          dep.var.labels="Defeat", title="Discrete-time survival models",
          covariate.labels = c("Democrat", "Presidential Vote", "Majority", "Log. Population", "FAADS",
                               "Committee Index", "Committee Chair", "Incumbent Expend.", "Challenger Expend.",
                               "South", "Duration", "d2", "d3", "d4", "Dem. * Pres. Vote", "FAADS*Committee",
                               "Inc. * Chall. Expend.", "Constant"))

#discrete-time competing
stargazer(House.logit2,House.trend2,House.P42,House.dummy2, no.space=T, digits = 2, keep.stat="N", omit = "as.factor",
          dep.var.labels="Defeat", title="Discrete-time survival models - defeat and retirement",
          covariate.labels = c("Democrat", "Presidential Vote", "Majority", "Log. Population", "FAADS",
                               "Committee Index", "Committee Chair", "Incumbent Expend.", "Challenger Expend.",
                               "South", "Duration", "d2", "d3", "d4", "Dem. * Pres. Vote", "FAADS*Committee",
                               "Inc. * Chall. Expend.", "Constant"))

#discrete-time + firth correction -- DOES NOT WORK; have to do manually
#stargazer(House.logit.f,House.trend.f,House.P4.f,House.dummy.f)

#save.image("D:/Dropbox/3_PLSC504/Paper/Survival/saved_results_environment.RData")