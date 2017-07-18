#######################################################################################################################################################
########################################  Trump Approval by Survey House #############################################################################
############################################### Elena Badillo Goicoechea, June 21 2017 ################################################################
#setwd("C:/YOUR_PREFERRED_WORKING_DIRECTORY")
#Required libraries

rm(list = ls()) #remove all previous work

library(data.table)
library(plyr)
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
poll$survey_house<-as.factor(gsub("/", "_", poll$survey_house)) #replace / to avoid symbol confusion
poll$survey_house<-as.factor(gsub(" ", "_", poll$survey_house))
surveyHouses<-levels(poll$survey_house) #names of all surveyers

#Subset (All Adults) some publish "Likely Voters" or "Registered Voters" instead of Adults
pollsub <- subset(poll, poll$sample_subpopulation== "Adults" | poll$sample_subpopulation=="Likely Voters" | poll$sample_subpopulation=="Registered Voters") #"Adults - Democrat" , "Adults - Republican"

# 1.Average Approval by Survey House

pollAp_sh<-summaryBy(Approve ~ survey_house, data=pollsub, FUN = list(mean, max, min, median, sd)) #by survey house
shm<-pollAp_sh[order(pollAp_sh$Approve.mean),] #order by min to max mean
dmean<-shm[,1:2]
colnames(dmean)<-c("Survey_House", "Approval")
dmean$Survey_House <- factor(dmean$Survey_House, levels = dmean$Survey_House[order(dmean$Approval)]) #ordenar factores para obtener colores ordenados

# 2.Average no. obs per survey, by survey house

pollObs_sh<-summaryBy(observations ~ survey_house, data=pollsub, FUN = list(mean, max, min, median)) #by survey house
sho<-pollObs_sh[order(pollAp_sh$Approve.mean),] #order by min to max mean
dmean$obs<-sho[,2]
colnames(dmean)<-c("Survey_House", "Approval", "Obs")

#  Scatterplot: Approval vs. No.Obs
library(ggrepel) #auto label placement

dmean2<-dmean[-c(13,21,23),]
gb2<-ggplot(dmean2, aes(x=Approval, y=Obs))+
     xlab("Avg. Trump Approval Rating (%)") +ylab("Avg. Obs per Survey")+
    geom_point(size=4, color="red",alpha=0.2) +
    geom_text_repel(aes(label=Survey_House), size=3, color="grey32")+ coord_cartesian(ylim=c(250,2250))+
    ggtitle("Trump Approval vs. No. Observations per Sample, by Survey House (average)*")+
    theme(plot.title = element_text(family = "Trebuchet", size=15,colour = "lightskyblue4",face="bold"),
          panel.grid.major = element_line(colour = "white"), panel.grid.minor = element_line(colour = "white"),
          axis.text.x =element_text(size  = 10,angle = 90, hjust = 1,  vjust = 1),
          axis.text.y =element_text(size  = 11))+
    annotate("text", color="lightskyblue4",size=3, x=38.8, y=300, label= "* Excludes SurveyMonkey and Pew, whose average sample sizes are dispproportionately large.")+
    annotate("text", color="lightskyblue4",size=3, x=36.1, y=250, label= "Source: Huffington Post")+
    geom_smooth(method="lm",  linetype="dashed",color="black", size=0.5,se = FALSE)
gb2

ggsave(filename="Obs_vs_App_scatter.jpg", width=11, height=7)
as.Date(pollmean$end_date[nrow(pollmean)])  # Last observation date
