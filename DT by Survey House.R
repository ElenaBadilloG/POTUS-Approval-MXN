#######################################################################################################################################################
########################################  Trump Approval by Survey House #############################################################################
############################################### Elena Badillo Goicoechea, June 21 2017 ################################################################
# setwd("C:/YOUR_PREFERRED_WORKING_DIRECTORY")

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
download.file(fileURL,destfile="./polls17.tsv")

# Full Polling Dataset (via HuffPost poll agregator)

poll<-read.table("./polls17.tsv", sep = '\t', header = TRUE)
surveyHouses<-data.frame(levels(poll$survey_house)) #names of all surveyers

#Subset (All Adults) *Rasmussen and PPP publish "Likely Voters" instead of Adults
pollsub <- subset(poll, poll$sample_subpopulation== "Adults" | poll$sample_subpopulation=="Likely Voters")

# Average Approval by Survey House

pollAp_sh<-summaryBy(Approve ~ survey_house, data=pollsub,
                     FUN = list(mean, max, min, median, sd)) #by survey house
shm<-pollAp_sh[order(pollAp_sh$Approve.mean),] #order by min to max mean
dmean<-shm[,1:2]
colnames(dmean)<-c("Survey_House", "Approval")
dmean$Survey_House <- factor(dmean$Survey_House, levels = dmean$Survey_House[order(dmean$Approval)]) #ordenar factores para obtener colores ordenados

# Bar plot
library(RColorBrewer)

n<-nrow(dmean)
colfunc <- colorRampPalette(c("blue", "red"))
#plot(rep(1,n),col=colfunc(n),pch=19,cex=3)
polpalette<-colfunc(n) #own palette  where blue+dem, red-dem

g4<-ggplot(dmean, aes(x = Survey_House, y = Approval, fill = Survey_House)) + 
    geom_bar(stat = "identity") +
    scale_fill_manual(values=polpalette)+
    xlab("Survey House") + ylab("Trump Approval (%, avg)")+
    coord_cartesian(ylim=c(30,50))+
    ggtitle("Avg. Trump Approval Rating by Survey House (%)")+
    theme(axis.text.x = element_text(size=10, angle=90), legend.position="none",
          axis.title = element_text(size=12),
          axis.text = element_text(size=12),
          plot.title = element_text(size=18))+
    annotate("text", x="AP-NORC", y=46, label= "less pro-Trump", colour="blue")+
    annotate("text", x="Zogby", y=46, label= "more pro-Trump", colour="red")
g4

ggsave(filename="Avg_App_by_Survey_House.jpg", width=11, height=7)
