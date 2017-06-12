#######################################################################################################################################################
########################################  Trump Approval and Mexican Peso #############################################################################
############################################### Elena Badillo Goicoechea, June 12 2017 ################################################################

setwd("C:/YOUR_PREFERRED_WORKING_DIRECTORY")

#Required libraries

library(data.table)
library(dplyr)
library(reshape2)
library(ggplot2)
library(doBy)
library(scales)
library(zoo)

options(scipen = 999) #disable exponential notation for plots

fileURL<-"http://elections.huffingtonpost.com/pollster/api/v2/questions/00c%20-Pres-45-Trump%20-%20Job%20Approval%20-%20National/poll-responses-clean.tsv"
download.file(fileURL,destfile="./polls17.tsv") #BEFORE 930am or after 7pm in BM

########################## Polling Data

# Full Polling Dataset (via HuffPost poll agregator)

poll<-read.table("./polls17.tsv", sep = '\t', header = TRUE)
surveyHouses<-data.frame(levels(poll$survey_house)) #names of all surveyers

#Subset (All Adults) *Rasmussen and PPP publish "Likely Voters" instead of Adults
pollsub <- subset(poll, poll$sample_subpopulation== "Adults" | poll$sample_subpopulation=="Likely Voters")

#Subset (Approval stats)
pollAp<-summaryBy(Approve ~ end_date, data=pollsub,
                 FUN = list(mean, max, min, median, sd))
#Subset (Disapproval stats)
pollDp<-summaryBy(Disapprove ~ end_date, data=pollsub,
                 FUN = list(mean, max, min, median, sd))

########################## FX Data (Mexican Peso)

## Download From Quandl (source: St. Louis Federal Reserve)

library(Quandl)

Quandl.api_key("_iMxF88U1h8YpgB95tCn")
peso <- Quandl("FED/RXI_N_B_MX",type = "raw",start_date=as.Date(pollDp$end_date[1]), end_date=as.Date(pollDp$end_date[nrow(pollDp)]))
peso<-peso[order(as.Date(peso$Date, format="%d/%m/%Y")),] #order by date min to max
colnames(peso)<-c("end_date", "peso")

# Merge peso (just business days) with the weekday-wise pollDp dataset*:

require(plyr)

pollDp<-data.table(pollDp)
pesoT<-data.table(peso)

pollDp<-join(pollDp, pesoT, by = "end_date", type = "left", match = "all")
pollDp<-data.frame(pollDp)

#Fill the missing values with the previous obs

require(zoo)
pollDp$peso[1]<-pollDp$peso[2] #Fill the first value (because it's missing here)
pollDp$peso<-data.frame(na.approx(pollDp$peso, na.rm = FALSE)) #linearly interpolate NA values (weekends) for FX data in roder to merge w/poll data*
pollDp$Ap<-pollAp$Approve.mean #Add Approval rating to pollDp
pollmean<-pollDp[,c(1,2,7,8)] #Subset columns of interest (we want MEAN disapproval rate)
colnames(pollmean) <- c("end_date","Disapprove","TCN","Approve")

pollmelt<-melt(pollmean[,-3], id="end_date")
colnames(pollmelt) <- c("Date","Rating", "Value")

############################ Plots

### Approval & Disapproval
pollmelt$Value<-as.numeric(pollmelt$Value)
pollmelt$Date<-as.Date(pollmelt$Date)

soft=0.2 #degree of smoothing
broad=0.15
g1 <- ggplot(pollmelt, aes(x = Date,y = Value, group=Rating, colour= Rating)) +
      geom_line(size=0.2, alpha=1)+
      geom_smooth(method="loess", span=soft)+ #loess=Polynomial Regression Fitting
      theme(axis.text.x =element_text(size  = 11,
                                    angle = 90,
                                    hjust = 1,                                
                                    vjust = 1))
g1

ggsave(filename="App_and_Disapp_DT.jpg", width=11, height=7) #save plot in jpg file (by default in the working dir)

### Disapproval vs. Mexican Peso

## Time series

#Normalize data to 1st observation-date (Jan 22 2017): for visualization purposes (original FX and Approval data is in very  different scales)
dateN<-"2017-01-22"
normR<-pollmeanT[pollmeanT$end_date == dateN,]
normR <- data.frame(normR[rep(row.names(normR), nrow(pollmeanT)),])

Norm<-data.frame(pollmeanT/normR)
Norm$end_date<-pollmeanT$end_date
colnames(Norm) <- c("end_date","Disapproves (index=1, 22/01/17)","TCN (index=1, 22/01/17)")

# Melt dataset to plot by factor
Normelt<-melt(Norm, id="end_date")
colnames(Normelt) <- c("Date","Var","Value")
Normelt$Value<-as.numeric(Normelt$Value)
Normelt$Date<-as.Date(Normelt$Date)
0.15
broad=0.02
g2<-ggplot(Normelt, aes(x = Date,y = Value, group=Var, colour= Var)) +
    geom_line(size=0.2, alpha=0.5)+
    geom_smooth(method="loess",span=soft, se = TRUE)+
    theme(axis.text.x =element_text(size  = 10,
                                    angle = 90,
                                    hjust = 1,                                
                                    vjust = 1))
g2

ggsave(filename="Mexican_Peso_vs_DT_ts.jpg", width=11, height=7)

## Scatterplot

g2<-ggplot(pollmean, aes(x=Disapprove, y=TCN))
g2<-g2+xlab("Trump Approval Rating (%)")
g2<-g2+ylab("Peso (MX/USD)")
g2<-g2+geom_point(size=4, color="red",alpha=0.2)
g2<-g2+geom_smooth(method="lm",  linetype="dashed",color="black", size=0.5,se = TRUE)
g2

ggsave(filename="Mexican_Peso_vs_DT_scatter.jpg", width=11, height=7)

as.Date(pollmean$end_date[nrow(pollmean)])  # Last observation date

######################################################################################################################################################
