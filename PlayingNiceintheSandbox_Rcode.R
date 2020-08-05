rm(list=ls())
library(haven)
library(lme4)
library(nlme)
library(stargazer)
library(rts)
library(viridis)
library(ggpubr)
library(broman)
library(plyr)
require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
require(foreach)
library(dplyr)  # Data frame manipulation
library(readr)  # Read CSVs nicely
library(broom)  # Convert models to data frames
library(data.table)
library(car)
options(scipen=999)

setwd("working directory")
load(file = "prehandling.data.RData")


#------------------------------------------------------------------------------------------------------------------
# Data handling
#------------------------------------------------------------------------------------------------------------------

#create a variable containing session id 
prehandling.data$sessionid = with(prehandling.data, 
                                    ifelse(session == "181031_1246",1, 
                                    ifelse(session == "181031_1524", 2, 
                                    ifelse(session == "181101_0928", 3, 
                                    ifelse(session == "181101_1526", 4,
                                    ifelse(session == "181112_1240", 5, 
                                    ifelse(session == "181112_1615", 6,
                                    ifelse(session == "181113_1437", 7,
                                    ifelse(session == "190402_1049", 8,
                                    ifelse(session =="190403_1037", 9,
                                    ifelse(session == "190405_1113", 10, 
                                    ifelse(session == "190408_1233", 11, 
                                    ifelse(session == "190410_1022", 12, 
                                    ifelse(session == "191015_0930", 13, 
                                    ifelse(session == "191015_1244", 14, 
                                    ifelse(session == "191016_0931", 15,
                                    ifelse(session == "191016_1236", 16, 
                                    ifelse(session == "191021_1013", 17, 0))))))))))))))))))
table(prehandling.data$sessionid)

#identify separate parts of the experiment
prehandling.data$part = with(prehandling.data, 
                               ifelse(treatment == 1, 1,
                               ifelse(treatment == 2, 2,
                               ifelse(treatment == 3, 3, 
                               ifelse(treatment == 4 & Period == 1, 4, 
                               ifelse(treatment == 4 & Period == 2, 5, 
                               ifelse(treatment == 5, 6,
                               ifelse(treatment == 6, 7,
                               ifelse(treatment == 7, 8, 0)))))))))
table(prehandling.data$part)

#Create variable identifying the group in main part of the experiment (Fishing Game)
prehandling.data$maingroup = with(prehandling.data, 
                                ifelse(treatment ==5, prehandling.data$Group, NA)) 
setDT(prehandling.data)[, maingroup:= maingroup[!is.na(maingroup)][1L] , by = .(session, Subject)]
table(prehandling.data$maingroup)

#Generate unique group id for over all sessions
prehandling.data$groupid = (10*prehandling.data$sessionid)+ (prehandling.data$maingroup)
table(prehandling.data$groupid)

#Generate unique subject id for over all sessions
prehandling.data$subjectid = (1000*prehandling.data$sessionid)+(100*prehandling.data$maingroup+prehandling.data$Subject)
table(prehandling.data$subjectid)

# Delete failed session group from data (Session 3, group 4 due to Z-tree setup mistake)
prehandling.data <- prehandling.data[which(prehandling.data$groupid != 34),]
table(prehandling.data$groupid)

#Correcting group treatment for session 3, group 5 due to session 3 Z-Tree setup mistake
prehandling.data$Both[which(prehandling.data$groupid == 35)]
prehandling.data$Both = with(prehandling.data, ifelse(prehandling.data$groupid == 35,0, prehandling.data$Both))
prehandling.data$Both[which(prehandling.data$groupid == 35)]
table(prehandling.data$Both)

#Correcting group treatment in session 191016_1236 (session 16) group 1 and group 2 due to Z-Tree error
prehandling.data$Nohet[which(prehandling.data$sessionid ==16 & prehandling.data$Group <3)]
prehandling.data$Nohet = with(prehandling.data, ifelse(prehandling.data$sessionid ==16 & Group <3,0, prehandling.data$Nohet))
prehandling.data$Nohet[which(prehandling.data$sessionid ==16 & prehandling.data$Group <3)]
table(prehandling.data$Nohet)

#Generate treatment conditional factor variable
prehandling.data$condition = with(prehandling.data, 
                                    ifelse(Ecohet ==1, 1,
                                    ifelse(Sochet ==1, 2, 
                                    ifelse(Both == 1, 3, 
                                    ifelse(Nohet ==1, 4,0)))))
table(prehandling.data$condition)
prehandling.data$condition = with(prehandling.data, ifelse(prehandling.data$groupid ==35,1, prehandling.data$condition))


#----------------------------------------------------------------------------------------------------
# Data handling: control variables
#----------------------------------------------------------------------------------------------------

# Make variable for sex
prehandling.data$Sex = with(prehandling.data, 
                         ifelse(Sex == "Male", 0, 
                         ifelse(Sex == "Female", 1, NA)))
table(prehandling.data$Sex)

# Make group mean of sex
tempdat <- ddply( prehandling.data, "groupid", function(x) 
  data.frame( meansex= mean(x$Sex, na.rm = TRUE ) ) )
table(tempdat$meansex)
prehandling.data <- merge( prehandling.data, tempdat )
table(prehandling.data$meansex)

#Make student variable
prehandling.data$Student = with(prehandling.data,
                             ifelse(Student == "No", 0,
                             ifelse(Student == "Yes", 1, NA)))
table(prehandling.data$Student)

#Make group mean of student
tempdat <- ddply( prehandling.data, "groupid", function(x) 
  data.frame( meanstudent= mean(x$Student, na.rm = TRUE ) ) )
table(tempdat$meanstudent)
prehandling.data <- merge( prehandling.data, tempdat )
table(prehandling.data$meanstudent)

#Make Game Theory Experience variable
prehandling.data$Gametheory = with(prehandling.data,
                                  ifelse(Gametheory == "No", 0,
                                         ifelse(Gametheory == "Yes", 1, NA)))
table(prehandling.data$Gametheory)

#Make group mean of game theory experience
tempdat <- ddply( prehandling.data, "groupid", function(x) 
  data.frame( meanGT= mean(x$Gametheory, na.rm = TRUE ) ) )
table(tempdat$meanGT)
prehandling.data <- merge( prehandling.data, tempdat )
table(prehandling.data$meanGT)

#Make group mean for number of friends/aquaintances in session
tempdat <- ddply( prehandling.data, "groupid", function(x) 
  data.frame( meanfriends= mean(x$Friends, na.rm = TRUE ) ) )
table(tempdat$meanfriends)
prehandling.data <- merge( prehandling.data, tempdat)
table(prehandling.data$meanfriends)

#Make group mean of age
tempdat <- ddply( prehandling.data, "groupid", function(x) 
  data.frame( meanage= mean(x$Age, na.rm = TRUE ) ) )
table(tempdat$meanage)
prehandling.data <- merge( prehandling.data, tempdat )
table(prehandling.data$meanage)

# make dummy for Netherlands sessions
prehandling.data$netherlands <- with(prehandling.data, 
                                       ifelse (session == "190402_1049" 
                                               | session == "190403_1037" 
                                               | session == "190405_1113" 
                                               | session == "190408_1233" 
                                               | session == "190410_1022", 1,0))
factor(prehandling.data$netherlands)
w = table(prehandling.data$netherlands)
t = as.data.frame(w)
t

# Variable for final profit / money paid to subject
prehandling.data$PaidMoney = with(prehandling.data,
                               ifelse(part == 8, MoneyToPay, NA))
table(prehandling.data$PaidMoney) 
setDT(prehandling.data)[, PaidMoney:= PaidMoney[!is.na(PaidMoney)][1L] , by = .(session, Subject)]
table(prehandling.data$PaidMoney) 


# Make variable for lagged sum of appropriation of others in group
# Sum appropriation others in t
prehandling.data$sumappothers = with(prehandling.data, 
                                  ifelse(treatment == 5 & player ==1, (App2 + App3 + App4), 
                                  ifelse(treatment == 5 & player ==2, (App1 + App3 + App4),
                                  ifelse(treatment == 5 & player ==3, (App1 + App2 + App4),
                                  ifelse(treatment == 5 & player ==4, (App1 + App2 + App3), NA)))))
table(prehandling.data$sumappothers)

#lagged sum appropriation others
prehandling.data = data.table(prehandling.data[order(session, treatment, Group, Period, player),])
col = c("sumappothers")
anscol = paste("lag1", col, sep="_")
prehandling.data[, (anscol) := data.table::shift(.SD, 4, NA, "lag"), .SDcols = col, by = "groupid"]
prehandling.data$sumappothers_lag1 = with(prehandling.data,
                                       ifelse(treatment == 5, prehandling.data$lag1_sumappothers, NA))
table(prehandling.data$sumappothers_lag1)


#------------------------------------------------------------------------------------------------------
# Data Handling: Investment Game variables
#------------------------------------------------------------------------------------------------------

#General trust (stage 1 of Investment Game)
table(prehandling.data$trusting)
prehandling.data$trust_general = with(prehandling.data,
                                        ifelse(treatment ==1, (trusting/10), NA))
table(prehandling.data$trust_general)
setDT(prehandling.data)[, trust_general:= trust_general[!is.na(trust_general)][1L] , by = .(session, Subject)]
table(prehandling.data$trust_general)


#Ingroup Trust (stage 2 of Investment Game)
prehandling.data$ingrouptrust = with(prehandling.data,
                                        ifelse(part ==4, (trusting/10), NA))
table(prehandling.data$ingrouptrust)
setDT(prehandling.data)[, ingrouptrust:= ingrouptrust[!is.na(ingrouptrust)][1L] , by = .(session, Subject)]
table(prehandling.data$ingrouptrust)

#Outgroup Trust (stage 3 of Investment Game)
prehandling.data$outgrouptrust = with(prehandling.data,
                                       ifelse(part ==5, (trusting/10), NA))
table(prehandling.data$outgrouptrust)
setDT(prehandling.data)[, outgrouptrust:= outgrouptrust[!is.na(outgrouptrust)][1L] , by = .(session, Subject)]
table(prehandling.data$outgrouptrust)

# Mean trust per group: general trust 
tempdat <- ddply( prehandling.data, "groupid", function(x) 
  data.frame( meantrust_general= mean(x$trust_general, na.rm = TRUE ) ) )
table(tempdat$meantrust_general)
prehandling.data <- merge( prehandling.data, tempdat )
table(prehandling.data$meantrust_general)

#Mean trust per group: ingroup trust 
tempdat <- ddply( prehandling.data, "groupid", function(x) 
  data.frame( meantrust_ingroup= mean(x$ingrouptrust, na.rm = TRUE ) ) )
table(tempdat$meantrust_ingroup)
prehandling.data <- merge( prehandling.data, tempdat )
table(prehandling.data$meantrust_ingroup)

#Mean trust per group: outgroup trust 
tempdat <- ddply( prehandling.data, "groupid", function(x) 
  data.frame( meantrust_outgroup= mean(x$outgrouptrust, na.rm = TRUE ) ) )
table(tempdat$meantrust_outgroup)
prehandling.data <- merge( prehandling.data, tempdat )
table(prehandling.data$meantrust_outgroup)

# Generate trustworthiness variable
prehandling.data$trustworth3 = prehandling.data$honouring3/13
prehandling.data$trustworth6 = prehandling.data$honouring6/16
prehandling.data$trustworth9 = prehandling.data$honouring9/19
prehandling.data$trustworth12 = prehandling.data$honouring12/22
prehandling.data$trustworth15 = prehandling.data$honouring15/25
prehandling.data$trustworth18 = prehandling.data$honouring18/28
prehandling.data$trustworth21 = prehandling.data$honouring21/31
prehandling.data$trustworth24 = prehandling.data$honouring24/34
prehandling.data$trustworth27 = prehandling.data$honouring27/37
prehandling.data$trustworth30 = prehandling.data$honouring30/40

prehandling.data$trustworthiness = (prehandling.data$trustworth3 + prehandling.data$trustworth6+ prehandling.data$trustworth9+
                                          prehandling.data$trustworth12+ prehandling.data$trustworth15+ prehandling.data$trustworth18+
                                          prehandling.data$trustworth21+ prehandling.data$trustworth24+
                                          prehandling.data$trustworth27+ prehandling.data$trustworth30)/10
table(prehandling.data$trustworthiness)

# General trustworthiness
prehandling.data$trustworth_general = with(prehandling.data,
                                        ifelse(treatment ==1, trustworthiness, NA))
table(prehandling.data$trustworth_general)
setDT(prehandling.data)[, trustworth_general:= trustworth_general[!is.na(trustworth_general)][1L] , by = .(session, Subject)]
table(prehandling.data$trustworth_general)

#Ingroup trustworthiness
prehandling.data$ingrouptrustworth = with(prehandling.data,
                                             ifelse(part == 4, trustworthiness, NA))
table(prehandling.data$ingrouptrustworth)
setDT(prehandling.data)[, ingrouptrustworth:= ingrouptrustworth[!is.na(ingrouptrustworth)][1L] , by = .(session, Subject)]
table(prehandling.data$ingrouptrustworth)

#Outgroup trustworthiness 
prehandling.data$outgrouptrustworth = with(prehandling.data,
                                            ifelse(part == 5, trustworthiness, NA))
table(prehandling.data$outgrouptrustworth)
setDT(prehandling.data)[, outgrouptrustworth:= outgrouptrustworth[!is.na(outgrouptrustworth)][1L] , by = .(session, Subject)]
table(prehandling.data$outgrouptrustworth)


#------------------------------------------------------------------------------------------------------------------
# Delete rows and columns that are not relevant for analyses
#------------------------------------------------------------------------------------------------------------------

prehandling.data = prehandling.data[which(prehandling.data$treatment ==5),]
prehandling.data = prehandling.data[,-which (grepl("honoured", names(prehandling.data))), with = FALSE]
prehandling.data = prehandling.data[,-which (grepl("honouring", names(prehandling.data))), with = FALSE]
prehandling.data = prehandling.data[,-which (grepl("Time", names(prehandling.data))), with = FALSE]
prehandling.data = prehandling.data[,-which (grepl("Option", names(prehandling.data))), with = FALSE]
prehandling.data = prehandling.data[,-which (grepl("Kancount", names(prehandling.data))), with = FALSE]
prehandling.data = prehandling.data[,-which (grepl("Correct", names(prehandling.data))), with = FALSE]
prehandling.data = prehandling.data[,-which (grepl("randomnumber", names(prehandling.data))), with = FALSE]
prehandling.data = prehandling.data[,-which (grepl("randomorder", names(prehandling.data))), with = FALSE]
prehandling.data = prehandling.data[,-which (grepl("kancount", names(prehandling.data))), with = FALSE]
prehandling.data = prehandling.data[,-which (grepl("TotalCorrect", names(prehandling.data))), with = FALSE]
prehandling.data = prehandling.data[,-which (grepl("DGprofit", names(prehandling.data))), with = FALSE]
prehandling.data = prehandling.data[,-which (grepl("trustprofit", names(prehandling.data))), with = FALSE]
prehandling.data = prehandling.data[,-which (grepl("MedianOrder", names(prehandling.data))), with = FALSE]
prehandling.data = prehandling.data[,-which (grepl("placetrust", names(prehandling.data))), with = FALSE]
prehandling.data = prehandling.data[,-which (grepl("trustplayer", names(prehandling.data))), with = FALSE]
prehandling.data = prehandling.data[,-which(names(prehandling.data) == "notplaced"), with = FALSE]
prehandling.data = prehandling.data[,-which(names(prehandling.data) == "Trustor"), with = FALSE]
prehandling.data = prehandling.data[,-which(names(prehandling.data) == "Trustee"), with = FALSE]
prehandling.data = prehandling.data[,-which(names(prehandling.data) == "Busy"), with = FALSE]
prehandling.data = prehandling.data[,-which(names(prehandling.data) == "practiseprofit"), with = FALSE]
prehandling.data = prehandling.data[,-which(names(prehandling.data) == "Kanorder"), with = FALSE]
prehandling.data = prehandling.data[,-which(names(prehandling.data) == "Kleeorder"), with = FALSE]
prehandling.data = prehandling.data[,-which(names(prehandling.data) == "Kanrandom"), with = FALSE]
prehandling.data = prehandling.data[,-which(names(prehandling.data) == "Kleerandom"), with = FALSE]
prehandling.data = prehandling.data[,-which(names(prehandling.data) == "KKorder"), with = FALSE]
prehandling.data = prehandling.data[,-which(names(prehandling.data) == "honoured_player2"), with = FALSE]
prehandling.data = prehandling.data[,-which(names(prehandling.data) == "DGrandom"), with = FALSE]
prehandling.data = prehandling.data[,-which(names(prehandling.data) == "LeaveStage"), with = FALSE]
prehandling.data = prehandling.data[,-which(names(prehandling.data) == "client"), with = FALSE]







#------------------------------------------------------------------------------------------------------------------
# Rename data, Save data & Load data
#------------------------------------------------------------------------------------------------------------------
working.data = prehandling.data
setwd("working directory") #set working directory
save(working.data,file = "working.data.RData",compress=TRUE)

setwd("working directory") # set working directory
load(file = "working.data.RData")

#------------------------------------------------------------------------------------------------------------------
# Graphs
#------------------------------------------------------------------------------------------------------------------

par(mfrow = c(1,1))
# GRAPH1: Average resource size per treatment over time
# Make mean resource size per treatment per period 
temp <- as.data.table(working.data)
#Ecohet
temp_meanresourceEcohet = 
  temp[,lapply(.SD,
               
               function(x){
                 mean(x[which(Ecohet==1)])
               }
  ),by = c("Period"),.SD = c("showResource")]
names(temp_meanresourceEcohet)[which(names(temp_meanresourceEcohet)=="showResource")] = "meanresourceEcohet"
working.data = merge(working.data,temp_meanresourceEcohet,by = "Period")


#Sochet
temp_meanresourceSochet = 
  temp[,lapply(.SD,
               
               function(x){
                 mean(x[which(Sochet==1)])
               }
  ),by = c("Period"),.SD = c("showResource")]
names(temp_meanresourceSochet)[which(names(temp_meanresourceSochet)=="showResource")] = "meanresourceSochet"
working.data = merge(working.data,temp_meanresourceSochet,by = "Period")


#Both
temp_meanresourceBoth = 
  temp[,lapply(.SD,
               
               function(x){
                 mean(x[which(Both==1)])
               }
  ),by = c("Period"),.SD = c("showResource")]
names(temp_meanresourceBoth)[which(names(temp_meanresourceBoth)=="showResource")] = "meanresourceBoth"
working.data = merge(working.data,temp_meanresourceBoth,by = "Period")


#Nohet
temp_meanresourceNohet = 
  temp[,lapply(.SD,
               
               function(x){
                 mean(x[which(Nohet==1)])
               }
  ),by = c("Period"),.SD = c("showResource")]
names(temp_meanresourceNohet)[which(names(temp_meanresourceNohet)=="showResource")] = "meanresourceNohet"
working.data = merge(working.data,temp_meanresourceNohet,by = "Period")


#Lineplot
newdata <- working.data[order(working.data$Period, 
                                   working.data$meanresourceEcohet, 
                                   working.data$meanresourceSochet,
                                   working.data$meanresourceBoth,
                                   working.data$meanresourceNohet),]


plot(x = newdata$Period,
     y = newdata$meanAppEcohet,
     pch = NA, 
     xlim = c(1,40),
     ylim = c(250, 600),
     xlab = 'Period',
     xaxt ="n",
     ylab= 'Resource size',
     cex.lab = 1,
     cex.axis = 1,
     bty="n")
axis(side = 1,at = seq(0,40,by=5),labels = seq(0,40,by=5), cex.axis = 1)

temp_5 <- loess(formula = meanresourceNohet ~ Period,data = newdata)
temp_6 <- loess(formula = meanresourceEcohet ~ Period,data = newdata)
temp_7 <- loess(formula = meanresourceSochet ~ Period,data = newdata)
temp_8 <- loess(formula = meanresourceBoth ~ Period,data = newdata)

lines(newdata$Period,temp_5$fitted,col = 'black', lty = 'solid', lwd = '2')
lines(newdata$Period,temp_6$fitted,col = 'black', lty = 'longdash', lwd = '2')
lines(newdata$Period,temp_7$fitted,col = 'black', lty = 'dotted', lwd = '2')
lines(newdata$Period,temp_8$fitted,col = 'black', lty = 'dotdash', lwd = '2')



legend("bottomleft",bty = "n",
       legend = c("NH","EH","SH","EHSH"),
       col = c('black', 'black', 'black', 'black'),
       lty = c('solid', 'longdash', 'dotted', 'dotdash'), lwd = 2)
dev.off()



# GRAPH 2: Average appropriation effort per treatment over time
# Make mean appropriation effort per treatment per period 
par(mfrow = c(1,1))
temp <- as.data.table(working.data)
#Ecohet
temp_meanAppEcohet = 
  temp[,lapply(.SD,
               
               function(x){
                 mean(x[which(Ecohet==1)])
               }
  ),by = c("Period"),.SD = c("Appropriation")]
names(temp_meanAppEcohet)[which(names(temp_meanAppEcohet)=="Appropriation")] = "meanAppEcohet"
working.data = merge(working.data,temp_meanAppEcohet,by = "Period")


#Sochet
temp_meanAppSochet = 
  temp[,lapply(.SD,
               
               function(x){
                 mean(x[which(Sochet==1)])
               }
  ),by = c("Period"),.SD = c("Appropriation")]
names(temp_meanAppSochet)[which(names(temp_meanAppSochet)=="Appropriation")] = "meanAppSochet"
working.data = merge(working.data,temp_meanAppSochet,by = "Period")


#Both
temp_meanAppBoth = 
  temp[,lapply(.SD,
               
               function(x){
                 mean(x[which(Both==1)])
               }
  ),by = c("Period"),.SD = c("Appropriation")]
names(temp_meanAppBoth)[which(names(temp_meanAppBoth)=="Appropriation")] = "meanAppBoth"
working.data = merge(working.data,temp_meanAppBoth,by = "Period")


#Nohet
temp_meanAppNohet = 
  temp[,lapply(.SD,
               
               function(x){
                 mean(x[which(Nohet==1)])
               }
  ),by = c("Period"),.SD = c("Appropriation")]
names(temp_meanAppNohet)[which(names(temp_meanAppNohet)=="Appropriation")] = "meanAppNohet"
working.data = merge(working.data,temp_meanAppNohet,by = "Period")



#Lineplot
newdata <- working.data[order(working.data$Period, 
                              working.data$meanAppEcohet, 
                              working.data$meanAppSochet,
                              working.data$meanAppBoth,
                              working.data$meanAppNohet),]


plot(x = newdata$Period,
     y = newdata$meanAppEcohet,
     pch = NA, 
     xlim = c(1,40),
     ylim = c(25, 45),
     xlab = 'Period',
     xaxt ="n",
     ylab= 'Appropriation',
     cex.lab = 1,
     cex.axis = 1,
     bty="n")
axis(side = 1,at = seq(0,40,by=5),labels = seq(0,40,by=5), cex.axis = 1)

temp_1 <- loess(formula = meanAppNohet ~ Period,data = newdata)
temp_2 <- loess(formula = meanAppEcohet ~ Period,data = newdata)
temp_3 <- loess(formula = meanAppSochet ~ Period,data = newdata)
temp_4 <- loess(formula = meanAppBoth ~ Period,data = newdata)

lines(newdata$Period,temp_1$fitted,col = 'black', lty = 'solid', lwd = '2')
lines(newdata$Period,temp_2$fitted,col = 'black', lty = 'longdash', lwd = '2')
lines(newdata$Period,temp_3$fitted,col = 'black', lty = 'dotted', lwd = '2')
lines(newdata$Period,temp_4$fitted,col = 'black', lty = 'dotdash', lwd = '2')



legend("bottomleft",bty = "n",
       legend = c("NH","EH","SH","EHSH"),
       col = c('black', 'black', 'black', 'black'),
       lty = c('solid', 'longdash', 'dotted', 'dotdash'), lwd = 2)
dev.off()

# GRAPH 3:Appropriation behaviour of high and low endowed players in EH and EHSH

#Make variables for average appropriation for high and low endowed players in EH anf EHSH
par(mfrow = c(1,1))
temp <- as.data.table(working.data)
#Ecohet High
temp_highAppEcohet = 
  temp[,lapply(.SD,
               
               function(x){
                 mean(x[which(Ecohet==1 & endowment == 60)])
               }
  ),by = c("Period"),.SD = c("Appropriation")]
names(temp_highAppEcohet)[which(names(temp_highAppEcohet)=="Appropriation")] = "highAppEcohet"
working.data = merge(working.data,temp_highAppEcohet,by = "Period")


#Ecohet Low
temp_lowAppEcohet = 
  temp[,lapply(.SD,
               
               function(x){
                 mean(x[which(Ecohet==1 & endowment == 40)])
               }
  ),by = c("Period"),.SD = c("Appropriation")]
names(temp_lowAppEcohet)[which(names(temp_lowAppEcohet)=="Appropriation")] = "lowAppEcohet"
working.data = merge(working.data,temp_lowAppEcohet,by = "Period")

#Both High
temp_highAppBoth = 
  temp[,lapply(.SD,
               
               function(x){
                 mean(x[which(Both==1 & endowment == 60)])
               }
  ),by = c("Period"),.SD = c("Appropriation")]
names(temp_highAppBoth)[which(names(temp_highAppBoth)=="Appropriation")] = "highAppBoth"
working.data = merge(working.data,temp_highAppBoth,by = "Period")


#Both Low
temp_lowAppBoth = 
  temp[,lapply(.SD,
               
               function(x){
                 mean(x[which(Both==1 & endowment == 40)])
               }
  ),by = c("Period"),.SD = c("Appropriation")]
names(temp_lowAppBoth)[which(names(temp_lowAppBoth)=="Appropriation")] = "lowAppBoth"
working.data = merge(working.data,temp_lowAppBoth,by = "Period")


#Lineplot 
newdata <- working.data[order(working.data$Period, 
                              working.data$highAppEcohet, 
                              working.data$lowAppEcohet,
                              working.data$highAppBoth,
                              working.data$lowAppBoth),]


plot(x = newdata$Period,
     y = newdata$highAppEcohet,
     pch = NA, 
     xlim = c(1,40),
     ylim = c(25, 45),
     xlab = 'Period',
     xaxt ="n",
     ylab= 'Appropriation',
     cex.lab = 1,
     cex.axis = 1,
     bty="n")
axis(side = 1,at = seq(0,40,by=5),labels = seq(0,40,by=5), cex.axis = 1)

temp_1 <- loess(formula = highAppEcohet ~ Period,data = newdata)
temp_2 <- loess(formula = lowAppEcohet ~ Period,data = newdata)
temp_3 <- loess(formula = highAppBoth ~ Period,data = newdata)
temp_4 <- loess(formula = lowAppBoth ~ Period,data = newdata)

lines(newdata$Period,temp_1$fitted,col = 'black', lty = 'solid', lwd = '2')
lines(newdata$Period,temp_2$fitted,col = 'black', lty = 'dotted', lwd = '2')
lines(newdata$Period,temp_3$fitted,col = 'black', lty = 'longdash', lwd = '2')
lines(newdata$Period,temp_4$fitted,col = 'black', lty = 'dotdash', lwd = '2')



legend("bottomleft",bty = "n",
       legend = c("EH high","EH low","EHSH high","EHSH low"),
       col = c('black', 'black', 'black', 'black'),
       lty = c('solid', 'dotted', 'longdash', 'dotdash'), lwd = 2)
dev.off()

#------------------------------------------------------------------------------------------------------------------
# Boxplots
#------------------------------------------------------------------------------------------------------------------

#Make factor variable for treatments
working.data$condition.fac <- factor(working.data$condition,
                                          levels = c(1, 2, 3, 4), 
                                          labels = c("Economic Heterogeneity", "Sociocutural Heterogeneity", "Economic & Sociocultural", "Homogeneity (Control)")
)


# BOXPLOT 1: Trust in other players by treatment
par(mfrow = c(1,1))
boxplot(working.data$trust80~working.data$condition.fac, ylab = "", xlab = "", names = c("EH", "SH", "EHSH", "NH"), cex.axis = 0.8)
dev.off()

#BOXPLOT 2: Subjective trustworthiness of other players by treatment
par(mfrow = c(1,1))
boxplot(working.data$trust13~working.data$condition.fac, ylab = "", xlab = "", names = c("EH", "SH", "EHSH", "NH"), cex.axis = 0.8)
dev.off()

#BOXPLOT 3: Investment Game trusting behaviour towards general, ingroup and outgroup member
# boxplot for trusting behaviour
par(mfrow = c(1, 3))
par(cex.axis=1.5)
par(cex.lab=2)
boxplot(working.data$trust_general, ylab = "", xlab = "General", cex.lab = 1.5)
boxplot(working.data$ingrouptrust, ylab = "", xlab = "Ingroup", cex.lab = 1.5)
boxplot(working.data$outgrouptrust, ylab = "", xlab = "Outgroup", cex.lab = 1.5)
dev.off()


#BOXPLOT 4: Investment Game trustworthy behaviour towards general, inrgoup and outgroup member
par(mfrow = c(1, 3))
par(cex.axis=1.5)
par(cex.lab=2)
boxplot(working.data$trustworth_general, ylab = " ", xlab = "General", cex.lab = 1.5)
boxplot(working.data$ingrouptrustworth, ylab = "", xlab = "Ingroup", cex.lab = 1.5)
boxplot(working.data$outgrouptrustworth, ylab = "", xlab = "Outgroup", cex.lab = 1.5)
dev.off()

par(mfrow = c(1,1))



#----------------------------------------------------------------------------------------------
#Graph: group performance by treatment
#----------------------------------------------------------------------------------------------

par(mfrow = c(2, 2))
color_transparent <- adjustcolor('black', alpha.f = 0.3)

#Ecohet graph
ecodata = working.data[which(working.data$Ecohet ==1),]

xvalues <- split(ecodata$Period, ecodata$groupid)
yvalues <- split(ecodata$showResource, ecodata$groupid)
plot(1:max(unlist(xvalues)),ylim=(c(0,max(unlist(yvalues)))),type="n", xlab = 'Period',ylab= 'Resource size', main = 'EH')
mapply(lines,xvalues,yvalues, col = color_transparent, lwd = 2)



#Sochet graph
socdata = working.data[which(working.data$Sochet ==1),]

xvalues <- split(socdata$Period, socdata$groupid)
yvalues <- split(socdata$showResource, socdata$groupid)
plot(1:max(unlist(xvalues)),ylim=(c(0,max(unlist(yvalues)))),type="n", xlab = 'Period',ylab= 'Resource size', main = 'SH')
mapply(lines,xvalues,yvalues, col = color_transparent, lwd = 2)


#Both graph
bothdata = working.data[which(working.data$Both ==1),]

xvalues <- split(bothdata$Period, bothdata$groupid)
yvalues <- split(bothdata$showResource, bothdata$groupid)
plot(1:max(unlist(xvalues)),ylim=(c(0,max(unlist(yvalues)))),type="n", xlab = 'Period',ylab= 'Resource size', main = 'EHSH')
mapply(lines,xvalues,yvalues, col = color_transparent, lwd = 2)

#Nohet graph
nohetdata = working.data[which(working.data$Nohet ==1),]

xvalues <- split(nohetdata$Period, nohetdata$groupid)
yvalues <- split(nohetdata$showResource, nohetdata$groupid)
plot(1:max(unlist(xvalues)),ylim=(c(0,max(unlist(yvalues)))),type="n", xlab = 'Period',ylab= 'Resource size', main = 'NH')
mapply(lines,xvalues,yvalues, col = color_transparent, lwd = 2)

dev.off()

#-------------------------------------------------------------------------------------------------
# example graph of decreasing marginal returns under overappropriation
#-------------------------------------------------------------------------------------------------

sumapp = 0:200  # max total app is 200
app = 0:50      # app from 0 to 50
E = 50          # endowment = 50
R0 = 600

# Graph for resource size under different total appropriations 
iter = 0
Rt_star_list = list()
U_star_list = list()
cand_param_list = list()
while(iter < 10){
  cand_param = list(sumapp = seq(120,200,10)[iter+1],
                    app = seq(30,50,2.5)[iter+1])
  
  U_star = c()
  Rt_star = c()
  Rt = 600
  
  for(t in 1:40){
    Rtmin = Rt
    Rt = min(600, 1.25*(Rtmin - (Rtmin / R0)*cand_param$sumapp))
    Rt_star = c(Rt_star,Rt)
    
    U = ((4/R0)*Rtmin)*cand_param$app + (E - cand_param$app)
    U_star = c(U_star,U)
    
  }
  
  U_star = cumsum(U_star)
  
  Rt_star_list = append(Rt_star_list,list(Rt_star))
  U_star_list = append(U_star_list,list(U_star))
  
  cand_param_list = append(cand_param_list, list(cand_param))
  iter = iter + 1
  
}


par(mfrow=c(3,3))
for(i in 1:9){
  plot(1:40,Rt_star_list[[i]], ylim = c(0, 600), ylab = "Resource Size", xlab = "Period")
  legend("bottomleft",
         bty="n", cex = 0.85,
         legend = paste("sum app:",round(as.numeric(as.character(unlist(cand_param_list[[i]]$sumapp))))))
}
dev.off()


par(mfrow=c(3,3))
for(i in 1:9){
  plot(1:40,U_star_list[[i]], ylim = c(0,max(unlist(U_star_list),na.rm=TRUE)), ylab = "Cumulative Utility", xlab = "Period")
  legend("topleft",
         bty="n", cex = 0.95,
         legend = paste("sum app:",round(as.numeric(as.character(unlist(cand_param_list[[i]]$sumapp))))))
}
dev.off()





#---------------------------------------------------------------------------------------------------
# RESOURCE SIZE TREATMENT EFFECTS OVER TIME WITH LINEAR + SQUARED PERIOD TERM 
#---------------------------------------------------------------------------------------------------

#create variable for quadratic term of Period
working.data$Period2 = working.data$Period^2 

#creating data frame 
simple = data.frame(t_1 = c(1, 0, 0, 0), 
                    t_2 = c(0, 1, 0, 0), 
                    t_3 = c(0, 0, 1, 0))

# Treatment effects over time
pred3 <- lme(fixed = showResource ~Ecohet*Period+ Sochet*Period + Both*Period + Ecohet*Period2 + Sochet*Period2 + Both*Period2 + 
               meantrust_general*Period  + meanage + meanfriends + meanGT + meanstudent + meansex +  netherlands, 
             random = ~1|groupid, data = working.data[which(working.data$player==1),])
summary(pred3)
pred3$coefficients$fixed

coefs_temp = 
  pred3$coefficients$fixed[grepl("Intercept",names(pred3$coefficients$fixed))|
                             grepl("Period",names(pred3$coefficients$fixed))|
                             grepl("het",names(pred3$coefficients$fixed))|
                             grepl("Both",names(pred3$coefficients$fixed))]
names(coefs_temp)[grep("Intercept",names(coefs_temp))] = "Intercept"

names(simple) = c("Ecohet","Sochet","Both")

#making data frame over 40 periods
level_1 = data.frame()
for(Period in 1:40){
  level_1_temp = data.frame(Intercept = 1,Period = Period,Period2=Period^2, simple)
  level_1 = rbind(level_1, level_1_temp)
}
level_1 $`Ecohet:Period`=level_1 $Ecohet*level_1 $Period
level_1 $`Period:Sochet` =level_1 $Sochet*level_1 $Period 
level_1 $`Period:Both`=level_1 $Both*level_1 $Period

level_1 $`Ecohet:Period2`=level_1 $Ecohet*level_1 $Period2
level_1 $`Sochet:Period2` =level_1 $Sochet*level_1 $Period2 
level_1 $`Both:Period2`=level_1 $Both*level_1 $Period2


y_hat = as.matrix(level_1) %*% as.numeric(coefs_temp[names(level_1)])

par.mfrow=c(1,1)
plot(x = 1:40,
     y = y_hat[which(level_1$Ecohet==1)],
     pch = NA, 
     xlim = c(1,40),
     ylim = c(250,600),
     xlab = 'Period',
     xaxt ="n",
     ylab= 'Predicted Resource Size',
     cex.lab = 1,
     cex.axis = 1,
     #yaxt = "n",
     bty="n")
axis(side = 1,at = seq(0,40,by=5),labels = seq(0,40,by=5), cex.axis = 1)
#axis(side = 2, at = seq(4, 6.5, by=0.5), labels = seq(4, 6.5, by=0.5))

lines(1:40,y_hat[which(level_1$Ecohet==0 & level_1$Both==0 & level_1$Sochet==0)],col = 'black',lty = 'solid', lwd = 2)
lines(1:40,y_hat[which(level_1$Ecohet==1 & level_1$Both==0 & level_1$Sochet==0)],col = 'black',lty = 'longdash', lwd = 2 )
lines(1:40,y_hat[which(level_1$Ecohet==0 & level_1$Both==0 & level_1$Sochet==1)],col = 'black',lty = 'dotted', lwd = 2 )
lines(1:40,y_hat[which(level_1$Ecohet==0 & level_1$Both==1 & level_1$Sochet==0)],col = 'black',lty = 'dotdash', lwd = 2 )

legend("bottomleft",bty = "n",
       legend = c("NH","EH","SH","EHSH"),
       lty = c('solid', 'longdash', 'dotted', 'dotdash'), lwd = 2)
dev.off()

#-----------------------------------------------------------------------------------
# TREATMENT EFFECT OF APPROPRIATION EFFORT INCLUDING LINEAR + SQUARED PERIOD TERM 
#-----------------------------------------------------------------------------------

pred6 <- lme(fixed = Appropriation ~Ecohet*Period2+ Sochet*Period2 + Both*Period2 + Ecohet*Period+ Sochet*Period + 
               Both*Period +trust_general*Period + Age + Sex + Friends + Gametheory + showResource + sumappothers_lag1 +  
               + netherlands + meantrust_general  + meanage + meanfriends + meanGT + meanstudent , random = ~1|groupid/subjectid, 
             data = working.data[which(!is.na(working.data$Sex) 
                                       & !is.na(working.data$sumappothers_lag1) 
                                       & !is.na(working.data$sumappothers_lag1)),])
summary(pred6)
pred6$coefficients$fixed


coefs_temp = 
  pred6$coefficients$fixed[grepl("Intercept",names(pred6$coefficients$fixed))|
                             grepl("Period",names(pred6$coefficients$fixed))|
                             grepl("het",names(pred6$coefficients$fixed))|
                             grepl("Both",names(pred6$coefficients$fixed))]
names(coefs_temp)[grep("Intercept",names(coefs_temp))] = "Intercept"


names(simple) = c("Ecohet","Sochet","Both")

#making data frame over 40 periods
level_1 = data.frame()
for(Period in 1:40){
  level_1_temp = data.frame(Intercept = 1,Period = Period,Period2=Period^2, simple)
  level_1 = rbind(level_1, level_1_temp)
}
level_1 $`Ecohet:Period`=level_1 $Ecohet*level_1 $Period
level_1 $`Sochet:Period` =level_1 $Sochet*level_1 $Period 
level_1 $`Both:Period`=level_1 $Both*level_1 $Period

level_1 $`Ecohet:Period2`=level_1 $Ecohet*level_1 $Period2
level_1 $`Period2:Sochet` =level_1 $Sochet*level_1 $Period2 
level_1 $`Period2:Both`=level_1 $Both*level_1 $Period2


y_hat = as.matrix(level_1) %*% as.numeric(coefs_temp[names(level_1)])

par(mfrow = c(1,1))
plot(x = 1:40,
     y = y_hat[which(level_1$Ecohet==1)],
     pch = NA, 
     xlim = c(1,40),
     ylim = c(22,32),
     xlab = 'Period',
     xaxt ="n",
     ylab= 'Predicted Appropriation',
     cex.lab = 1,
     cex.axis = 1,
     #yaxt = "n",
     bty="n")
axis(side = 1,at = seq(0,60,by=5),labels = seq(0,60,by=5), cex.axis = 1)
#axis(side = 2, at = seq(4, 6.5, by=0.5), labels = seq(4, 6.5, by=0.5))

lines(1:40,y_hat[which(level_1$Ecohet==0 & level_1$Both==0 & level_1$Sochet==0)],col = 'black',lty = 'solid', lwd = 2)
lines(1:40,y_hat[which(level_1$Ecohet==1 & level_1$Both==0 & level_1$Sochet==0)],col = 'black',lty = 'longdash', lwd = 2 )
lines(1:40,y_hat[which(level_1$Ecohet==0 & level_1$Both==0 & level_1$Sochet==1)],col = 'black',lty = 'dotted' , lwd = 2)
lines(1:40,y_hat[which(level_1$Ecohet==0 & level_1$Both==1 & level_1$Sochet==0)],col = 'black',lty = 'dotdash' , lwd = 2)

legend("bottomleft",bty = "n",
       legend = c("NH","EH","SH","EHSH"),
       lty = c('solid', 'longdash', 'dotted', 'dotdash'), lwd = 2)
dev.off()

#-------------------------------------------------------------------------------------------------
# T-tests
#-------------------------------------------------------------------------------------------------
#T-test of general trust vs outgrouptrust
t.test(working.data[which(working.data$Period==1),]$trust_general, working.data[which(working.data$Period==1),]$outgrouptrust, paired = TRUE)

#T-test of ingrouptrust vs outgrouptrust
t.test(working.data[which(working.data$Period==1),]$ingrouptrust, working.data[which(working.data$Period==1),]$outgrouptrust, paired = TRUE)

#T-test of high endowed EH vs high endowed EHSH players assuming equal variance
working.data$highappEco = with(working.data, 
                          ifelse(Ecohet ==1 & endowment ==60, Appropriation, NA))
working.data$highappBoth = with(working.data,
                          ifelse(Both ==1 & endowment == 60, Appropriation, NA))
t.test(working.data$highappEco, working.data$highappBoth, var.equal=TRUE)



#-------------------------------------------------------------------------------------------------
# Resource Size models
#-------------------------------------------------------------------------------------------------

#Check that Klee/Kan has no effect 
Modela <- lme(fixed = showResource~ Klee, random = ~1|groupid, data = working.data)
summary(Modela)

#Model 0: intercept only
Model0 <- lme(fixed = showResource~1, random = ~1|groupid, data = working.data)
summary(Model0)

# ICC = 114.2081/114.2081+99.54602 = 0.534
# Higher values of ICC indicate that a greater share of the total variation in the outcome measure is associated
# with cluster membership; i.e., a relatively strong relationship among the scores for two individuals from the same cluster.

#Model 1: Intercept and treatment effects interacted by period 
Model1 <- lme(fixed = showResource~ Ecohet*Period + Sochet*Period + Both*Period , 
               random = ~1| groupid, data = working.data[which(working.data$player==1),])
summary(Model1)


#Model 2: Intercept and treatment effects by period + average trust + controls + game effects
Model2 <- lme(fixed = showResource ~Ecohet*Period + Sochet*Period + Both*Period + 
                 meantrust_general*Period + meanage + meanfriends + meanGT + meanstudent 
               + netherlands + meansex, 
               random = ~1|groupid, data = working.data[which(working.data$player==1),])
summary(Model2)


# Model 3: quadratic effect of period 
Model3 <- lme(fixed = showResource~Ecohet*Period + Sochet*Period + Both*Period 
              +Ecohet*Period2 + Sochet*Period2 + Both*Period2 + 
                meantrust_general*Period+ meanage + meanfriends + meanGT 
              + meanstudent + meansex+  netherlands + Period, 
              random = ~1|groupid, data = working.data[which(working.data$player==1),])
summary(Model3)


#-------------------------------------------------------------------------------------------------
# Appropriation effort models
#-------------------------------------------------------------------------------------------------

#Model 4: Check that Klee/Kan has no significant effect 
Model4 <- lme(fixed = Appropriation ~ Klee, random = ~1|groupid/subjectid, data = working.data)
summary(Model4)

#Model 5: Intercept only model
Model5 <- lme(fixed = Appropriation~1, random = ~1|groupid/subjectid, data = working.data)
summary(Model5)

# ICC = 6.537/(6.537+12.129) = 0.350

#Model 6: Intercept and treatment effects by period + interaction with Period
Model6 <- lme(fixed = Appropriation ~ Ecohet*Period + Sochet*Period + Both*Period, 
              random = ~1|groupid/subjectid, data = working.data)
summary(Model6)

#Model 7: Intercept and treatments + individual trust + controls + interaction with Period
Model7 <- lme(fixed = Appropriation~Ecohet*Period + Sochet*Period + Both*Period + 
                 trust_general*Period +  Age + Sex + Friends + Gametheory + 
                 showResource +sumappothers_lag1 + netherlands
               + meantrust_general  + meanage + meanfriends + meanGT + meanstudent +meansex
               , random = ~1|groupid/subjectid, 
                data = working.data[which(!is.na(working.data$Sex) 
                                          & !is.na(working.data$sumappothers_lag1)),])
summary(Model7)  

#Intercept and treatments + individual trust + controls with linear and squared term of Period
Model8<- lme(fixed = Appropriation ~Ecohet*Period2+ Sochet*Period2 + Both*Period2 + Ecohet*Period+ Sochet*Period + 
                Both*Period +trust_general*Period + Age + Sex + Friends + Gametheory + showResource + sumappothers_lag1 +  
                + netherlands + meantrust_general  + meanage + meanfriends + meanGT + meanstudent +meansex , random = ~1|groupid/subjectid, 
              data = working.data[which(!is.na(working.data$Sex) 
                                        & !is.na(working.data$sumappothers_lag1)),])
summary(Model8)


#--------------------------------------------------------------------------------------------------------------
# Controlling for max trust in Resource size models
#--------------------------------------------------------------------------------------------------------------

temp.df = working.data

#make variable of max trust within group
groupMax <- ddply( working.data, "groupid", function(x) 
  data.frame( maxTrust = max(x$trust_general) ) )
temp.df <- merge( temp.df, groupMax, by = "groupid" )

#regression with max of trust 
Model_t1 <- lme(fixed = showResource~Ecohet*Period + Sochet*Period + Both*Period 
                +Ecohet*Period2 + Sochet*Period2 + Both*Period2 + 
                  maxTrust + meantrust_general*Period+ meanage + meanfriends + meanGT 
                + meanstudent + meansex+  netherlands + Period, 
                random = ~1|groupid, data = temp.df[which(temp.df$player==1),])
summary(Model_t1)


#-------------------------------------------------------------------------------------------------
# TRUST MODELS
#-------------------------------------------------------------------------------------------------
#Model 9: Treatments and controls on reported trust. 
Model9 = polr(formula = as.factor(as.character(unlist(trust80))) ~ Ecohet + Sochet + Both + 
                PaidMoney + Age + Friends + Gametheory + 
                Sex + Student + netherlands, 
                data = working.data[which(working.data$Period == 40),])
summary(Model9)
# store table
(ctable <- coef(summary(Model9)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))
#Odds ratios
exp(coef(Model9))
# get odds ratio standard errors
get.or.se <- function(Model9) {
  broom::tidy(Model9) %>% 
    mutate(or = exp(estimate),
           var.diag = diag(vcov(Model9)),
           or.se = sqrt(or^2 * var.diag)) %>%
    dplyr::select(or.se) %>% unlist %>% unname
}
get.or.se(Model9)



#Model 10: Treatments and controls on reported trustworthiness of other players
Model10 = polr(formula = as.factor(as.character(unlist(trust13))) ~ Ecohet + Sochet + Both + 
                 PaidMoney + Age + Friends + Gametheory + 
                 Sex + Student + netherlands, 
                 data = working.data[which(working.data$Period == 40),])
summary(Model10)
# store table
(ctable <- coef(summary(Model10)))
# calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
# combined table
(ctable <- cbind(ctable, "p value" = p))
#Odds ratios
exp(coef(Model10))
#Standard errors
get.or.se <- function(Model10) {
  broom::tidy(Model10) %>% 
    mutate(or = exp(estimate),
           var.diag = diag(vcov(Model10)),
           or.se = sqrt(or^2 * var.diag)) %>%
    dplyr::select(or.se) %>% unlist %>% unname
}
get.or.se(Model10)



#--------------------------------------------------------------------------------------------------------
# Create Stargazer models for Latex
#--------------------------------------------------------------------------------------------------------

#ShowResource tables
stargazer(Model1, Model2, Model3)


#Appropriation effort tables
stargazer(Model6, Model7, Model8)

# Trust table
stargazer(Model9, Model10)

#-------------------------------------------------------------------------------------------------------
# Nonparametric tests
#---------------------------------------------------------------------------------------------------
# Kruskal wallis test: is there a difference in means between groups
#---------------------------------------------------------------------------------------------------
# Resource Size
aggdata <- aggregate(data.frame(showResource_mean=working.data$showResource), 
                     by=list(condition=working.data$condition,  Period=working.data$Period),
                     FUN= mean, na.rm=TRUE)
kruskal.test(showResource_mean ~ condition, data = aggdata)


# Appropriation Effort
aggdata <- aggregate(data.frame(Appropriation_mean=working.data$Appropriation), 
                     by=list(condition=working.data$condition, Period=working.data$Period),
                     FUN= mean, na.rm=TRUE)
kruskal.test(Appropriation_mean ~ condition, data = aggdata)

#---------------------------------------------------------------------------------------------------
# Mann whitney U test:  are 2 groups different
#---------------------------------------------------------------------------------------------------


# data for means of resource size per treatment per period
tempdata = working.data
tempdata = tempdata[, c("meanresourceEcohet", "meanresourceSochet", "meanresourceBoth", "meanresourceNohet", "Period")]
tempdata = unique(tempdata)

wilcox.test(tempdata$meanresourceEcohet[-1],tempdata$meanresourceSochet[-1],paired=FALSE) # EH vs SH = sig
wilcox.test(tempdata$meanresourceEcohet[-1],tempdata$meanresourceNohet[-1],paired=FALSE) # EH vs NH = sig
wilcox.test(tempdata$meanresourceEcohet[-1],tempdata$meanresourceBoth[-1],paired=FALSE) # EH vs EHSH = sig 
wilcox.test(tempdata$meanresourceSochet[-1],tempdata$meanresourceBoth[-1],paired=FALSE) # SH vs EHSH = not sig
wilcox.test(tempdata$meanresourceSochet[-1],tempdata$meanresourceNohet[-1],paired=FALSE) # SH vs NH = not sig 
wilcox.test(tempdata$meanresourceBoth[-1],tempdata$meanresourceNohet[-1],paired=FALSE) # EHSH vs NH = not sig 


# data for means of resource size per treatment per period
tempdata = working.data
tempdata = tempdata[, c("meanAppEcohet", "meanAppSochet", "meanAppBoth", "meanAppNohet", "Period")]
tempdata = unique(tempdata)

wilcox.test(tempdata$meanAppEcohet, tempdata$meanAppSochet, paired = FALSE) # EH vs SH = not sig 
wilcox.test(tempdata$meanAppEcohet, tempdata$meanAppBoth, paired = FALSE) # EH vs EHSH = not sig 
wilcox.test(tempdata$meanAppEcohet, tempdata$meanAppNohet, paired = FALSE) # EH vs NH = not sig 
wilcox.test(tempdata$meanAppSochet, tempdata$meanAppBoth, paired = FALSE) # SH vs EHSH = not sig 
wilcox.test(tempdata$meanAppSochet, tempdata$meanAppNohet, paired = FALSE) # SH vs NH  = not sig 
wilcox.test(tempdata$meanAppBoth, tempdata$meanAppNohet, paired = FALSE) # NH vs EHSH = not sig 



