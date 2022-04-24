#Installing packages

library(MASS)
library(plotly)
library(ggplot2)
library(lattice)
library(tidyverse) #for data transformation
library(magrittr) #for piping
library(forcats) #for factor manipulation
library("devtools")
library("yarrr")
library(gganimate)
library(gapminder)
library(gifski)


library("rms")
library("sjPlot")#Interaction plots
library(sjmisc)
library(ggplot2)
library(MuMIn) # use to calculate the marginal and conditional R-Sq
library(lme4) #for glmm
library("lmerTest")
library(effects)#check for effect size
theme_set(theme_sjplot())


library("pROC")
library("psych")
library("rockchalk")
library(ROCR)
#Loading packages
library(rpart)
library(rpart.plot)
library(caret)
library("randomForest") #for categorical dependent variable
library("mobForest") #for random forest (eg. count data)
#library("REEMtree") #classification tree with random effect
library(dplyr)
library(tidyr)
library(reshape2)
library(reprtree) #for ploting random forest tree
library("gbm") #Gradient boosting machine
library("dismo")
library(xgboost)      # a faster implementation of gbm
library(pdp)          # model visualization
library(ggplot2)      # model visualization
library(lime)         # model visualization
library("factoextra")
library("cluster")
library("mclust") #model-based clustering
library("dbscan")#density-based clustering
library("clValid")#Validate Clustering methods
library("kohonen") #Self-organising maps  clustering
library(NbClust) #determining the optimal number of clusters
library (fpc)

library(mlbench)
library(e1071)

#Visualizing randomforest
library(igraph)
library("plyr")
library("lubridate")
library(dplyr)
library("ggraph")
library("rminer")

#Visualizing gbm (convert rf and ct to gbm)
library("partykit")
#library("h2o")
library("nnet")

#Create a custom color scale
library(RColorBrewer)
library(fishplot)

library(sp)  # vector data
library(raster)  # raster data
library(rgdal)  # input/output, projections

library("rgeos")  # geometry ops
library("spdep")  # spatial dependence
library(maptools)

#Importing data
Gt3_plot=read.csv(file="GT3_Plot_2.csv")
Gt_plot=read.csv(file="LA-Turn_Plot_2.csv")
Gbull_plot=read.csv(file="LA-Bull_plot_2.csv")

dim(Gbull_plot)

#Importing data
Gt3_plot=read.csv(file="GT3_Plot_2.csv")
Gt_plot=read.csv(file="LA-Turn_Plot_2.csv")
Gbull_plot=read.csv(file="LA-Bull_plot_2.csv")

Fish_strain_Gt3=factor(Gt3_plot$Pop_strain,levels=c(1,2,3), labels=c("UA", "LA","Orn"))
Fish_sex_Gt3=factor(Gt3_plot$SEX,levels=c(1,2), labels=c("Female", "Male"))

Gt3_plot$fish=Fish_strain_Gt3
Gt3_plot$Fish_sex=Fish_sex_Gt3
Gt3_plot$parasite_type=rep(1,dim(Gt3_plot)[1])


Fish_strain_Gt=factor(Gt_plot$Pop_strain,levels=c(1,2,3), labels=c("UA", "LA","Orn"))
Fish_sex_Gt=factor(Gt_plot$SEX,levels=c(1,2), labels=c("Female", "Male"))

Gt_plot$fish=Fish_strain_Gt
Gt_plot$Fish_sex=Fish_sex_Gt
Gt_plot$parasite_type=rep(2,dim(Gt_plot)[1])


Fish_strain_Gbull=factor(Gbull_plot$Pop_strain,levels=c(1,2,3), labels=c("UA", "LA","Orn"))
Fish_sex_Gbull=factor(Gbull_plot$SEX,levels=c(1,2), labels=c("Female", "Male"))

Gbull_plot$fish=Fish_strain_Gbull
Gbull_plot$Fish_sex=Fish_sex_Gbull
Gbull_plot$parasite_type=rep(3,dim(Gbull_plot)[1])

#Combining the three data set
Combined_data<-rbind(Gt3_plot,Gt_plot,Gbull_plot)
Combined_data$Pop_strain<-factor(Combined_data$Pop_strain,levels=c(1,2,3), labels=c("UA", "LA","Orn"))
Combined_data$parasite_type<-factor(Combined_data$parasite_type,levels=c(1,2,3),labels=c("Gt3","Gt","Gbull"))
Orn_plot=split(Combined_data,Combined_data$Pop_strain)$"Orn"
LA_plot=split(Combined_data,Combined_data$Pop_strain)$"LA"
UA_plot=split(Combined_data,Combined_data$Pop_strain)$"UA"

#Transforming the data structure for Time-based HeatMap
Parasite_transform=list()
Parasite_transform[[1]]<- ddply(Gt3_plot, c("Day", "Parasite","Bodypart"), summarise,
                    N    = length(Gt3_plot)
)

Parasite_transform[[2]]<- ddply(Gt_plot, c("Day", "Parasite","Bodypart"), summarise,
                    N    = length(Gt_plot)
)

Parasite_transform[[3]]<- ddply(Gbull_plot, c("Day", "Parasite","Bodypart"), summarise,
                    N    = length(Gbull_plot)
)


Parasite_transform[[4]]<- ddply(Orn_plot, c("Day", "Parasite","Bodypart"), summarise,
                    N    = length(Gt3_plot)
)

Parasite_transform[[5]]<- ddply(LA_plot, c("Day", "Parasite","Bodypart"), summarise,
                    N    = length(Gt_plot)
)

Parasite_transform[[6]]<- ddply(UA_plot, c("Day", "Parasite","Bodypart"), summarise,
                    N    = length(Gbull_plot)
)

Parasite_transform[[1]]$Bodypart= factor(Parasite_transform[[1]]$Bodypart,levels(Parasite_transform[[1]]$Bodypart)[c(7,4,8,1,2,6,5,3)])
Parasite_transform[[2]]$Bodypart= factor(Parasite_transform[[2]]$Bodypart,levels(Parasite_transform[[2]]$Bodypart)[c(7,4,8,1,2,6,5,3)])
Parasite_transform[[3]]$Bodypart= factor(Parasite_transform[[3]]$Bodypart,levels(Parasite_transform[[3]]$Bodypart)[c(7,4,8,1,2,6,5,3)])
Parasite_transform[[4]]$Bodypart= factor(Parasite_transform[[4]]$Bodypart,levels(Parasite_transform[[4]]$Bodypart)[c(7,4,8,1,2,6,5,3)])
Parasite_transform[[5]]$Bodypart= factor(Parasite_transform[[5]]$Bodypart,levels(Parasite_transform[[5]]$Bodypart)[c(7,4,8,1,2,6,5,3)])
Parasite_transform[[6]]$Bodypart= factor(Parasite_transform[[6]]$Bodypart,levels(Parasite_transform[[6]]$Bodypart)[c(7,4,8,1,2,6,5,3)])


#levels(Parasite_transform_Gt3$Bodypart)
#levels(Parasite_transform_Gt$Bodypart)
#levels(Parasite_transform_Gbull$Bodypart)

#Ornamental fish across parasite type
Orn_Gt3_data=split(Orn_plot,Orn_plot$parasite_type)$Gt3
Orn_Gt_data=split(Orn_plot,Orn_plot$parasite_type)$Gt
Orn_Gbull_data=split(Orn_plot,Orn_plot$parasite_type)$Gbull

#LA fish across parasite type
LA_Gt3_data=split(LA_plot,LA_plot$parasite_type)$Gt3
LA_Gt_data=split(LA_plot,LA_plot$parasite_type)$Gt
LA_Gbull_data=split(LA_plot,LA_plot$parasite_type)$Gbull

#UA fish across parasite type
UA_Gt3_data=split(UA_plot,UA_plot$parasite_type)$Gt3
UA_Gt_data=split(UA_plot,UA_plot$parasite_type)$Gt
UA_Gbull_data=split(UA_plot,UA_plot$parasite_type)$Gbull


#Splitting across time for Orn-Gt3
Orn_Gt3=list()
Orn_Gt3[[1]]=split(Orn_Gt3_data,Orn_Gt3_data$Day)$"1"
Orn_Gt3[[3]]=split(Orn_Gt3_data,Orn_Gt3_data$Day)$"3"
Orn_Gt3[[5]]=split(Orn_Gt3_data,Orn_Gt3_data$Day)$"5"
Orn_Gt3[[7]]=split(Orn_Gt3_data,Orn_Gt3_data$Day)$"7"
Orn_Gt3[[9]]=split(Orn_Gt3_data,Orn_Gt3_data$Day)$"9"
Orn_Gt3[[11]]=split(Orn_Gt3_data,Orn_Gt3_data$Day)$"11"
Orn_Gt3[[13]]=split(Orn_Gt3_data,Orn_Gt3_data$Day)$"13"
Orn_Gt3[[15]]=split(Orn_Gt3_data,Orn_Gt3_data$Day)$"15"
Orn_Gt3[[17]]=split(Orn_Gt3_data,Orn_Gt3_data$Day)$"17"

#Splitting across time for Orn-Gt
Orn_Gt=list()
Orn_Gt[[1]]=split(Orn_Gt_data,Orn_Gt_data$Day)$"1"
Orn_Gt[[3]]=split(Orn_Gt_data,Orn_Gt_data$Day)$"3"
Orn_Gt[[5]]=split(Orn_Gt_data,Orn_Gt_data$Day)$"5"
Orn_Gt[[7]]=split(Orn_Gt_data,Orn_Gt_data$Day)$"7"
Orn_Gt[[9]]=split(Orn_Gt_data,Orn_Gt_data$Day)$"9"
Orn_Gt[[11]]=split(Orn_Gt_data,Orn_Gt_data$Day)$"11"
Orn_Gt[[13]]=split(Orn_Gt_data,Orn_Gt_data$Day)$"13"
Orn_Gt[[15]]=split(Orn_Gt_data,Orn_Gt_data$Day)$"15"
Orn_Gt[[17]]=split(Orn_Gt_data,Orn_Gt_data$Day)$"17"


#Splitting across time for Orn-Gbull
Orn_Gbull=list()
Orn_Gbull[[1]]=split(Orn_Gbull_data,Orn_Gbull_data$Day)$"1"
Orn_Gbull[[3]]=split(Orn_Gbull_data,Orn_Gbull_data$Day)$"3"
Orn_Gbull[[5]]=split(Orn_Gbull_data,Orn_Gbull_data$Day)$"5"
Orn_Gbull[[7]]=split(Orn_Gbull_data,Orn_Gbull_data$Day)$"7"
Orn_Gbull[[9]]=split(Orn_Gbull_data,Orn_Gbull_data$Day)$"9"
Orn_Gbull[[11]]=split(Orn_Gbull_data,Orn_Gbull_data$Day)$"11"
Orn_Gbull[[13]]=split(Orn_Gbull_data,Orn_Gbull_data$Day)$"13"
Orn_Gbull[[15]]=split(Orn_Gbull_data,Orn_Gbull_data$Day)$"15"
Orn_Gbull[[17]]=split(Orn_Gbull_data,Orn_Gbull_data$Day)$"17"


#Splitting across time for LA-Gt3
LA_Gt3=list()
LA_Gt3[[1]]=split(LA_Gt3_data,LA_Gt3_data$Day)$"1"
LA_Gt3[[3]]=split(LA_Gt3_data,LA_Gt3_data$Day)$"3"
LA_Gt3[[5]]=split(LA_Gt3_data,LA_Gt3_data$Day)$"5"
LA_Gt3[[7]]=split(LA_Gt3_data,LA_Gt3_data$Day)$"7"
LA_Gt3[[9]]=split(LA_Gt3_data,LA_Gt3_data$Day)$"9"
LA_Gt3[[11]]=split(LA_Gt3_data,LA_Gt3_data$Day)$"11"
LA_Gt3[[13]]=split(LA_Gt3_data,LA_Gt3_data$Day)$"13"
LA_Gt3[[15]]=split(LA_Gt3_data,LA_Gt3_data$Day)$"15"
LA_Gt3[[17]]=split(LA_Gt3_data,LA_Gt3_data$Day)$"17"

#Splitting across time for LA-Gt
LA_Gt=list()
LA_Gt[[1]]=split(LA_Gt_data,LA_Gt_data$Day)$"1"
LA_Gt[[3]]=split(LA_Gt_data,LA_Gt_data$Day)$"3"
LA_Gt[[5]]=split(LA_Gt_data,LA_Gt_data$Day)$"5"
LA_Gt[[7]]=split(LA_Gt_data,LA_Gt_data$Day)$"7"
LA_Gt[[9]]=split(LA_Gt_data,LA_Gt_data$Day)$"9"
LA_Gt[[11]]=split(LA_Gt_data,LA_Gt_data$Day)$"11"
LA_Gt[[13]]=split(LA_Gt_data,LA_Gt_data$Day)$"13"
LA_Gt[[15]]=split(LA_Gt_data,LA_Gt_data$Day)$"15"
LA_Gt[[17]]=split(LA_Gt_data,LA_Gt_data$Day)$"17"


#Splitting across time for LA-Gbull
LA_Gbull=list()
LA_Gbull[[1]]=split(LA_Gbull_data,LA_Gbull_data$Day)$"1"
LA_Gbull[[3]]=split(LA_Gbull_data,LA_Gbull_data$Day)$"3"
LA_Gbull[[5]]=split(LA_Gbull_data,LA_Gbull_data$Day)$"5"
LA_Gbull[[7]]=split(LA_Gbull_data,LA_Gbull_data$Day)$"7"
LA_Gbull[[9]]=split(LA_Gbull_data,LA_Gbull_data$Day)$"9"
LA_Gbull[[11]]=split(LA_Gbull_data,LA_Gbull_data$Day)$"11"
LA_Gbull[[13]]=split(LA_Gbull_data,LA_Gbull_data$Day)$"13"
LA_Gbull[[15]]=split(LA_Gbull_data,LA_Gbull_data$Day)$"15"
LA_Gbull[[17]]=split(LA_Gbull_data,LA_Gbull_data$Day)$"17"


#Splitting across time for UA-Gt3
UA_Gt3=list()
UA_Gt3[[1]]=split(UA_Gt3_data,UA_Gt3_data$Day)$"1"
UA_Gt3[[3]]=split(UA_Gt3_data,UA_Gt3_data$Day)$"3"
UA_Gt3[[5]]=split(UA_Gt3_data,UA_Gt3_data$Day)$"5"
UA_Gt3[[7]]=split(UA_Gt3_data,UA_Gt3_data$Day)$"7"
UA_Gt3[[9]]=split(UA_Gt3_data,UA_Gt3_data$Day)$"9"
UA_Gt3[[11]]=split(UA_Gt3_data,UA_Gt3_data$Day)$"11"
UA_Gt3[[13]]=split(UA_Gt3_data,UA_Gt3_data$Day)$"13"
UA_Gt3[[15]]=split(UA_Gt3_data,UA_Gt3_data$Day)$"15"
UA_Gt3[[17]]=split(UA_Gt3_data,UA_Gt3_data$Day)$"17"

#Splitting across time for UA-Gt
UA_Gt=list()
UA_Gt[[1]]=split(UA_Gt_data,UA_Gt_data$Day)$"1"
UA_Gt[[3]]=split(UA_Gt_data,UA_Gt_data$Day)$"3"
UA_Gt[[5]]=split(UA_Gt_data,UA_Gt_data$Day)$"5"
UA_Gt[[7]]=split(UA_Gt_data,UA_Gt_data$Day)$"7"
UA_Gt[[9]]=split(UA_Gt_data,UA_Gt_data$Day)$"9"
UA_Gt[[11]]=split(UA_Gt_data,UA_Gt_data$Day)$"11"
UA_Gt[[13]]=split(UA_Gt_data,UA_Gt_data$Day)$"13"
UA_Gt[[15]]=split(UA_Gt_data,UA_Gt_data$Day)$"15"
UA_Gt[[17]]=split(UA_Gt_data,UA_Gt_data$Day)$"17"


#Splitting across time for UA-Gbull
UA_Gbull=list()
UA_Gbull[[1]]=split(UA_Gbull_data,UA_Gbull_data$Day)$"1"
UA_Gbull[[3]]=split(UA_Gbull_data,UA_Gbull_data$Day)$"3"
UA_Gbull[[5]]=split(UA_Gbull_data,UA_Gbull_data$Day)$"5"
UA_Gbull[[7]]=split(UA_Gbull_data,UA_Gbull_data$Day)$"7"
UA_Gbull[[9]]=split(UA_Gbull_data,UA_Gbull_data$Day)$"9"
UA_Gbull[[11]]=split(UA_Gbull_data,UA_Gbull_data$Day)$"11"
UA_Gbull[[13]]=split(UA_Gbull_data,UA_Gbull_data$Day)$"13"
UA_Gbull[[15]]=split(UA_Gbull_data,UA_Gbull_data$Day)$"15"
UA_Gbull[[17]]=split(UA_Gbull_data,UA_Gbull_data$Day)$"17"


seq_day=seq(1,17,by=2)
Trans_Orn_Gt3=list()
Trans_Orn_Gt=list()
Trans_Orn_Gbull=list()
Trans_LA_Gt3=list()
Trans_LA_Gt=list()
Trans_LA_Gbull=list()
Trans_UA_Gt3=list()
Trans_UA_Gt=list()
Trans_UA_Gbull=list()
for (i in seq_day)
    {
Trans_Orn_Gt3[[i]]<- ddply(Orn_Gt3[[i]], c("Day", "Parasite","Bodypart","ID"), summarise,
                    N    = length(Orn_Gt3))
Trans_Orn_Gt[[i]]<- ddply(Orn_Gt[[i]], c("Day", "Parasite","Bodypart","ID"), summarise,
                    N    = length(Orn_Gt))
Trans_Orn_Gbull[[i]]<- ddply(Orn_Gbull[[i]], c("Day", "Parasite","Bodypart","ID"), summarise,
                    N    = length(Orn_Gbull))
    
Trans_LA_Gt3[[i]]<- ddply(LA_Gt3[[i]], c("Day", "Parasite","Bodypart","ID"), summarise,
                    N    = length(LA_Gt3))
Trans_LA_Gt[[i]]<- ddply(LA_Gt[[i]], c("Day", "Parasite","Bodypart","ID"), summarise,
                    N    = length(LA_Gt))
Trans_LA_Gbull[[i]]<- ddply(LA_Gbull[[i]], c("Day", "Parasite","Bodypart","ID"), summarise,
                    N    = length(LA_Gbull))
    
Trans_UA_Gt3[[i]]<- ddply(UA_Gt3[[i]], c("Day", "Parasite","Bodypart","ID"), summarise,
                    N    = length(UA_Gt3))
Trans_UA_Gt[[i]]<- ddply(UA_Gt[[i]], c("Day", "Parasite","Bodypart","ID"), summarise,
                    N    = length(UA_Gt))
Trans_UA_Gbull[[i]]<- ddply(UA_Gbull[[i]], c("Day", "Parasite","Bodypart","ID"), summarise,
                    N    = length(UA_Gbull))
}

Bodyparts=list()
Guppy<-function(Data){
    #display.brewer.all(8,type="seq",colorblindFriendly=T)
Bodyparts[[1]]=split(Data[,-c(1,4)],Data$Bodypart)$"Tail"
Bodyparts[[2]]=split(Data[,-c(1,4)],Data$Bodypart)$"Anal"
Bodyparts[[3]]=split(Data[,-c(1,4)],Data$Bodypart)$"LB"
Bodyparts[[4]]=split(Data[,-c(1,4)],Data$Bodypart)$"UB"
Bodyparts[[5]]=split(Data[,-c(1,4)],Data$Bodypart)$"Pelvic"
Bodyparts[[6]]=split(Data[,-c(1,4)],Data$Bodypart)$"Pectoral"
Bodyparts[[7]]=split(Data[,-c(1,4)],Data$Bodypart)$"Dorsal"
Bodyparts[[8]]=split(Data[,-c(1,4)],Data$Bodypart)$"Head"

#Computing means across each of the bodyparts
    
fish_total=length(unique(Data$ID))
Tail=round(log((sum(Bodyparts[[1]]$Parasite)/fish_total)+1),0)
Anal=round(log((sum(Bodyparts[[2]]$Parasite)/fish_total)+1),0)
LB=round(log((sum(Bodyparts[[3]]$Parasite)/fish_total)+1),0)
UB=round(log((sum(Bodyparts[[4]]$Parasite)/fish_total)+1),0)
Pelvic=round(log((sum(Bodyparts[[5]]$Parasite)/fish_total)+1),0)
Pectoral=round(log((sum(Bodyparts[[6]]$Parasite)/fish_total)+1),0)
Dorsal=round(log((sum(Bodyparts[[7]]$Parasite)/fish_total)+1),0)
Head=round(log((sum(Bodyparts[[8]]$Parasite)/fish_total)+1),0) 

return(c(Tail,Anal,LB,UB,Pelvic,Pectoral,Dorsal,Head))
}


Gt3_Orn_heatmap=list()

for (i in seq_day){
 Gt3_Orn_heatmap[[i]]= Guppy(Trans_Orn_Gt3[[i]]) 
}

for (i in seq_day){
cat("log mean Parasite load of Orn fish for Gt3 at the eight bodyparts are for day:", "",i,"is","",Gt3_Orn_heatmap[[i]],"\n")
    }

#Binning colours
Gt3_Orn_bins=list()
myColors <- brewer.pal(8,"Greys")
breaks <- c(0,.5,1,1.5,2,2.5,3,3.5,5)
for (i in seq_day){
Gt3_Orn_bins[[i]]<-cut(Gt3_Orn_heatmap[[i]], breaks, len = 9,include.lowest = TRUE )
}


Gt_Orn_heatmap=list()

for (i in seq_day){
 Gt_Orn_heatmap[[i]]= Guppy(Trans_Orn_Gt[[i]]) 
}

for (i in seq_day){
cat("Log mean parasite load of Orn fish  for Gt at the eight bodyparts are for day:", "",i,"is","",Gt_Orn_heatmap[[i]],"\n")
    }

#Binning colours
Gt_Orn_bins=list()
myColors <- brewer.pal(8,"Greys")
breaks <- c(0,.5,1,1.5,2,2.5,3,3.5,5)
for (i in seq_day){
Gt_Orn_bins[[i]]<-cut(Gt_Orn_heatmap[[i]], breaks, len = 9,include.lowest = TRUE )
}


Gbull_Orn_heatmap=list()

for (i in seq_day){
 Gbull_Orn_heatmap[[i]]= Guppy(Trans_Orn_Gbull[[i]]) 
}

for (i in seq_day){
cat("Log mean parasite load of Orn fish for Gbull at the eight bodyparts are for day:", "",i,"is","",Gbull_Orn_heatmap[[i]],"\n")
    }

#Binning colours
Gbull_Orn_bins=list()
myColors <- brewer.pal(8,"Greys")
breaks <- c(0,.5,1,1.5,2,2.5,3,3.5,5)
for (i in seq_day){
Gbull_Orn_bins[[i]]<-cut(Gbull_Orn_heatmap[[i]], breaks, len = 9,include.lowest = TRUE )
}


Gt3_LA_heatmap=list()

for (i in seq_day){
 Gt3_LA_heatmap[[i]]= Guppy(Trans_LA_Gt3[[i]]) 
}

for (i in seq_day){
cat("Log mean parasite load of LA fish for Gt3 at the eight bodyparts are for day:", "",i,"is","",Gt3_LA_heatmap[[i]],"\n")
    }
#Binning colours
Gt3_LA_bins=list()
myColors <- brewer.pal(8,"Greys")
breaks <- c(0,.5,1,1.5,2,2.5,3,3.5,5)
for (i in seq_day){
Gt3_LA_bins[[i]]<-cut(Gt3_LA_heatmap[[i]], breaks, len = 9,include.lowest = TRUE )
}


Gt_LA_heatmap=list()

for (i in seq_day){
 Gt_LA_heatmap[[i]]= Guppy(Trans_LA_Gt[[i]]) 
}

for (i in seq_day){
cat("Log mean parasite load of LA fish  for Gt at the eight bodyparts are for day:", "",i,"is","",Gt_LA_heatmap[[i]],"\n")
    }
#Binning colours
Gt_LA_bins=list()
myColors <- brewer.pal(8,"Greys")
breaks <-c(0,.5,1,1.5,2,2.5,3,3.5,5)
for (i in seq_day){
Gt_LA_bins[[i]]<-cut(Gt_LA_heatmap[[i]], breaks, len = 9,include.lowest = TRUE )
}


Gbull_LA_heatmap=list()

for (i in seq_day){
 Gbull_LA_heatmap[[i]]= Guppy(Trans_LA_Gbull[[i]]) 
}

for (i in seq_day){
cat("Log mean parasite load of LA fish for Gbull at the eight bodyparts are for day:", "",i,"is","",Gbull_LA_heatmap[[i]],"\n")
    }
#Binning colours
Gbull_LA_bins=list()
myColors <- brewer.pal(8,"Greys")
breaks <-c(0,.5,1,1.5,2,2.5,3,3.5,5)
for (i in seq_day){
Gbull_LA_bins[[i]]<-cut(Gbull_LA_heatmap[[i]], breaks, len = 9,include.lowest = TRUE )
}


Gt3_UA_heatmap=list()

for (i in seq_day){
 Gt3_UA_heatmap[[i]]= Guppy(Trans_UA_Gt3[[i]]) 
}

for (i in seq_day){
cat("Log mean parasite load of UA fish for Gt3 at the eight bodyparts are for day:", "",i,"is","",Gt3_UA_heatmap[[i]],"\n")
    }
#Binning colours
Gt3_UA_bins=list()
myColors <- brewer.pal(8,"Greys")
breaks <- c(0,.5,1,1.5,2,2.5,3,3.5,5)
for (i in seq_day){
Gt3_UA_bins[[i]]<-cut(Gt3_UA_heatmap[[i]], breaks, len = 9,include.lowest = TRUE )
}


Gt_UA_heatmap=list()

for (i in seq_day){
 Gt_UA_heatmap[[i]]= Guppy(Trans_UA_Gt[[i]]) 
}

for (i in seq_day){
cat("Log mean parasite load of UA fish  for Gt at the eight bodyparts are for day:", "",i,"is","",Gt_UA_heatmap[[i]],"\n")
    }
#Binning colours
Gt_UA_bins=list()
myColors <- brewer.pal(8,"Greys")
breaks <- c(0,.5,1,1.5,2,2.5,3,3.5,5)
for (i in seq_day){
Gt_UA_bins[[i]]<-cut(Gt_UA_heatmap[[i]], breaks, len = 9,include.lowest = TRUE )
}


Gbull_UA_heatmap=list()

for (i in seq_day){
 Gbull_UA_heatmap[[i]]= Guppy(Trans_UA_Gbull[[i]]) 
}

for (i in seq_day){
cat("Log mean parasite load of UA fish for Gbull at the eight bodyparts are for day:", "",i,"is","",Gbull_UA_heatmap[[i]],"\n")
    }
#Binning colours
Gbull_UA_bins=list()
myColors <- brewer.pal(8,"Greys")
breaks <- c(0,.5,1,1.5,2,2.5,3,3.5,5)
for (i in seq_day){
Gbull_UA_bins[[i]]<-cut(Gbull_UA_heatmap[[i]], breaks, len = 9,include.lowest = TRUE )
}


library(officer)
library(Rcpp)
library(rvg)

par(mfrow=c(3,3),mar=c(0,0,0,0))

#Gt3-Orn
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_Orn_bins[[1]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_Orn_bins[[1]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[1]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[1]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_Orn_bins[[1]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_Orn_bins[[1]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_Orn_bins[[1]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_Orn_bins[[1]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 1)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)



#Gt3-Orn
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_Orn_bins[[3]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_Orn_bins[[3]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[3]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[3]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_Orn_bins[[3]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_Orn_bins[[3]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_Orn_bins[[3]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_Orn_bins[[3]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)



#Gt3-Orn
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_Orn_bins[[5]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_Orn_bins[[5]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[5]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[5]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_Orn_bins[[5]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_Orn_bins[[5]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_Orn_bins[[5]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_Orn_bins[[5]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 5)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)



#Gt3-Orn
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_Orn_bins[[7]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_Orn_bins[[7]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[7]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[7]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_Orn_bins[[7]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_Orn_bins[[7]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_Orn_bins[[7]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_Orn_bins[[7]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 7)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)



#Gt3-Orn
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_Orn_bins[[9]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_Orn_bins[[9]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[9]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[9]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_Orn_bins[[9]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_Orn_bins[[9]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_Orn_bins[[9]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_Orn_bins[[9]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 9)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


#Gt3-Orn
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_Orn_bins[[11]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_Orn_bins[[11]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[11]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[1]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_Orn_bins[[11]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_Orn_bins[[11]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_Orn_bins[[11]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_Orn_bins[[11]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 11)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)
#Gt3-Orn
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_Orn_bins[[13]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_Orn_bins[[13]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[13]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[13]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_Orn_bins[[13]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_Orn_bins[[13]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_Orn_bins[[13]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_Orn_bins[[13]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 13)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

#Gt3-Orn
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_Orn_bins[[15]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_Orn_bins[[15]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[15]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[15]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_Orn_bins[[15]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_Orn_bins[[15]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_Orn_bins[[15]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_Orn_bins[[15]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 15)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

#Gt3-Orn
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_Orn_bins[[17]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_Orn_bins[[17]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[17]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[17]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_Orn_bins[[17]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_Orn_bins[[17]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_Orn_bins[[17]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_Orn_bins[[17]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 17)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)




par(mfrow=c(3,3),mar=c(0,0,0,0))

#Gt3-Orn
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_Orn_bins[[1]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_Orn_bins[[1]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[1]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[1]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_Orn_bins[[1]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_Orn_bins[[1]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_Orn_bins[[1]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_Orn_bins[[1]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 1)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)



#Gt3-Orn
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_Orn_bins[[3]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_Orn_bins[[3]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[3]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[3]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_Orn_bins[[3]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_Orn_bins[[3]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_Orn_bins[[3]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_Orn_bins[[3]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)



#Gt3-Orn
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_Orn_bins[[5]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_Orn_bins[[5]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[5]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[5]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_Orn_bins[[5]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_Orn_bins[[5]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_Orn_bins[[5]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_Orn_bins[[5]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 5)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)



#Gt3-Orn
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_Orn_bins[[7]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_Orn_bins[[7]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[7]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[7]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_Orn_bins[[7]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_Orn_bins[[7]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_Orn_bins[[7]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_Orn_bins[[7]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 7)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)



#Gt3-Orn
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_Orn_bins[[9]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_Orn_bins[[9]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[9]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[9]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_Orn_bins[[9]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_Orn_bins[[9]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_Orn_bins[[9]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_Orn_bins[[9]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 9)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


#Gt3-Orn
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_Orn_bins[[11]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_Orn_bins[[11]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[11]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[1]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_Orn_bins[[11]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_Orn_bins[[11]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_Orn_bins[[11]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_Orn_bins[[11]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 11)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)
#Gt3-Orn
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_Orn_bins[[13]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_Orn_bins[[13]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[13]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[13]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_Orn_bins[[13]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_Orn_bins[[13]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_Orn_bins[[13]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_Orn_bins[[13]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 13)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

#Gt3-Orn
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_Orn_bins[[15]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_Orn_bins[[15]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[15]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[15]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_Orn_bins[[15]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_Orn_bins[[15]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_Orn_bins[[15]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_Orn_bins[[15]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 15)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

#Gt3-Orn
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_Orn_bins[[17]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_Orn_bins[[17]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[17]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[17]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_Orn_bins[[17]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_Orn_bins[[17]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_Orn_bins[[17]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_Orn_bins[[17]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 17)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)




Graph=function()
    
    
doc <- read_pptx()
doc <- add_slide(doc, 'Title and Content', 'Office Theme')
doc <- ph_with_vg(doc, code = Graph(Time=Time[[i]]),type = "body")

# Write the document to a file
print(doc, target = 'Gt3_fish.pptx')

par(mfrow=c(3,3),mar=c(0,0,0,0))
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_LA_bins[[1]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_LA_bins[[1]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[1]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[1]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_LA_bins[[1]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_LA_bins[[1]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_LA_bins[[1]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_LA_bins[[1]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 1)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_LA_bins[[3]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_LA_bins[[3]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[3]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[3]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_LA_bins[[3]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_LA_bins[[3]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_LA_bins[[3]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_LA_bins[[3]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_LA_bins[[5]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_LA_bins[[5]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[5]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[5]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_LA_bins[[5]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_LA_bins[[5]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_LA_bins[[5]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_LA_bins[[5]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 5)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)



plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_LA_bins[[7]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_LA_bins[[7]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[7]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[7]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_LA_bins[[7]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_LA_bins[[7]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_LA_bins[[7]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_LA_bins[[7]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 7)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_LA_bins[[9]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_LA_bins[[9]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[9]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[9]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_LA_bins[[9]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_LA_bins[[9]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_LA_bins[[9]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_LA_bins[[9]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 9)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_LA_bins[[11]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_LA_bins[[11]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[11]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[11]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_LA_bins[[11]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_LA_bins[[11]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_LA_bins[[11]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_LA_bins[[11]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 11)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_LA_bins[[13]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_LA_bins[[13]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[13]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[13]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_LA_bins[[13]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_LA_bins[[13]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_LA_bins[[13]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_LA_bins[[13]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 13)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_LA_bins[[15]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_LA_bins[[15]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[15]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[15]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_LA_bins[[15]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_LA_bins[[15]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_LA_bins[[15]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_LA_bins[[15]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 15)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_LA_bins[[17]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_LA_bins[[17]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[17]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[17]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_LA_bins[[17]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_LA_bins[[17]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_LA_bins[[17]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_LA_bins[[17]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 17)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


#Gt3-UA
par(mfrow=c(3,3),mar=c(0,0,0,0))
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_UA_bins[[1]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_UA_bins[[1]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[1]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[1]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_UA_bins[[1]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_UA_bins[[1]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_UA_bins[[1]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_UA_bins[[1]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 1)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)
   


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_UA_bins[[3]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_UA_bins[[3]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[3]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[3]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_UA_bins[[3]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_UA_bins[[3]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_UA_bins[[3]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_UA_bins[[3]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_UA_bins[[5]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_UA_bins[[5]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[5]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[5]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_UA_bins[[5]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_UA_bins[[5]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_UA_bins[[5]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_UA_bins[[5]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 5)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_UA_bins[[7]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_UA_bins[[7]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[7]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[7]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_UA_bins[[7]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_UA_bins[[7]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_UA_bins[[7]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_UA_bins[[7]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 7)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_UA_bins[[9]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_UA_bins[[9]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[9]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[9]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_UA_bins[[9]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_UA_bins[[9]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_UA_bins[[9]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_UA_bins[[9]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 9)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)
   

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_UA_bins[[11]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_UA_bins[[11]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[11]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[11]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_UA_bins[[11]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_UA_bins[[11]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_UA_bins[[11]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_UA_bins[[11]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 11)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_UA_bins[[13]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_UA_bins[[13]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[13]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[13]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_UA_bins[[13]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_UA_bins[[13]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_UA_bins[[13]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_UA_bins[[13]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 13)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_UA_bins[[15]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_UA_bins[[15]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[15]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[15]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_UA_bins[[15]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_UA_bins[[15]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_UA_bins[[15]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_UA_bins[[15]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 15)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_UA_bins[[17]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_UA_bins[[17]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[17]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[17]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_UA_bins[[17]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_UA_bins[[17]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_UA_bins[[17]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_UA_bins[[17]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 17)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)
   


#par(mfrow=c(3,3),mar=c(4,4,1,1))
#par(mar=c(4,4,1,1))


par(mfrow=c(3,3),mar=c(0,0,0,0))
#Gt-Orn
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_Orn_bins[[1]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_Orn_bins[[1]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[1]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[1]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_Orn_bins[[1]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_Orn_bins[[1]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_Orn_bins[[1]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_Orn_bins[[1]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 1)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)
   

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_Orn_bins[[3]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_Orn_bins[[3]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[3]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[3]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_Orn_bins[[3]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_Orn_bins[[3]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_Orn_bins[[3]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_Orn_bins[[3]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_Orn_bins[[5]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_Orn_bins[[5]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[5]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[5]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_Orn_bins[[5]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_Orn_bins[[5]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_Orn_bins[[5]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_Orn_bins[[5]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 5)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)



plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_Orn_bins[[7]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_Orn_bins[[7]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[7]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[7]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_Orn_bins[[7]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_Orn_bins[[7]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_Orn_bins[[7]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_Orn_bins[[7]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 7)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_Orn_bins[[9]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_Orn_bins[[9]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[9]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[9]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_Orn_bins[[9]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_Orn_bins[[9]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_Orn_bins[[9]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_Orn_bins[[9]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 9)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)
   
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_Orn_bins[[11]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_Orn_bins[[11]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[11]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[11]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_Orn_bins[[11]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_Orn_bins[[11]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_Orn_bins[[11]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_Orn_bins[[11]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 11)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_Orn_bins[[13]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_Orn_bins[[13]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[13]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[13]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_Orn_bins[[13]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_Orn_bins[[13]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_Orn_bins[[13]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_Orn_bins[[13]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 13)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_Orn_bins[[15]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_Orn_bins[[15]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[15]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[15]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_Orn_bins[[15]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_Orn_bins[[15]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_Orn_bins[[15]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_Orn_bins[[15]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 15)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)
   
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_Orn_bins[[17]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_Orn_bins[[17]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[17]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[17]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_Orn_bins[[17]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_Orn_bins[[17]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_Orn_bins[[17]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_Orn_bins[[17]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 17)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)
   



#Gt-LA
par(mfrow=c(3,3),mar=c(0,0,0,0))
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_LA_bins[[1]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_LA_bins[[1]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_LA_bins[[1]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_LA_bins[[1]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_LA_bins[[1]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_LA_bins[[1]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_LA_bins[[1]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_LA_bins[[1]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 1)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_LA_bins[[3]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_LA_bins[[3]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_LA_bins[[3]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_LA_bins[[3]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_LA_bins[[3]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_LA_bins[[3]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_LA_bins[[3]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_LA_bins[[3]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_LA_bins[[5]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_LA_bins[[5]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_LA_bins[[5]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_LA_bins[[5]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_LA_bins[[5]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_LA_bins[[5]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_LA_bins[[5]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_LA_bins[[5]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 5)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_LA_bins[[7]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_LA_bins[[7]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_LA_bins[[7]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_LA_bins[[7]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_LA_bins[[7]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_LA_bins[[7]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_LA_bins[[7]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_LA_bins[[7]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 7)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_LA_bins[[9]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_LA_bins[[9]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_LA_bins[[9]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_LA_bins[[9]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_LA_bins[[9]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_LA_bins[[9]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_LA_bins[[9]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_LA_bins[[9]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 9)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_LA_bins[[11]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_LA_bins[[11]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_LA_bins[[11]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_LA_bins[[11]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_LA_bins[[11]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_LA_bins[[11]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_LA_bins[[11]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_LA_bins[[11]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 11)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_LA_bins[[13]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_LA_bins[[13]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_LA_bins[[13]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_LA_bins[[13]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_LA_bins[[13]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_LA_bins[[13]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_LA_bins[[13]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_LA_bins[[13]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 13)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)
   

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_LA_bins[[15]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_LA_bins[[15]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_LA_bins[[15]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_LA_bins[[15]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_LA_bins[[15]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_LA_bins[[15]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_LA_bins[[15]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_LA_bins[[15]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 15)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_LA_bins[[17]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_LA_bins[[17]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_LA_bins[[17]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_LA_bins[[17]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_LA_bins[[17]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_LA_bins[[17]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_LA_bins[[17]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_LA_bins[[17]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 17)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   




#Gt-UA
par(mfrow=c(3,3),mar=c(0,0,0,0))
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_UA_bins[[1]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_UA_bins[[1]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_UA_bins[[1]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_UA_bins[[1]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_UA_bins[[1]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_UA_bins[[1]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_UA_bins[[1]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_UA_bins[[1]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 1)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)
   

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_UA_bins[[3]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_UA_bins[[3]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_UA_bins[[3]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_UA_bins[[3]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_UA_bins[[3]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_UA_bins[[3]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_UA_bins[[3]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_UA_bins[[3]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_UA_bins[[5]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_UA_bins[[5]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_UA_bins[[5]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_UA_bins[[5]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_UA_bins[[5]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_UA_bins[[5]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_UA_bins[[5]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_UA_bins[[5]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 5)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_UA_bins[[7]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_UA_bins[[7]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_UA_bins[[7]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_UA_bins[[7]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_UA_bins[[7]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_UA_bins[[7]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_UA_bins[[7]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_UA_bins[[7]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 7)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_UA_bins[[9]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_UA_bins[[9]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_UA_bins[[9]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_UA_bins[[9]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_UA_bins[[9]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_UA_bins[[9]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_UA_bins[[9]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_UA_bins[[9]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 9)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)
   

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_UA_bins[[11]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_UA_bins[[11]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_UA_bins[[11]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_UA_bins[[11]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_UA_bins[[11]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_UA_bins[[11]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_UA_bins[[11]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_UA_bins[[11]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 11)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)
   


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_UA_bins[[13]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_UA_bins[[13]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_UA_bins[[13]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_UA_bins[[13]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_UA_bins[[13]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_UA_bins[[13]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_UA_bins[[13]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_UA_bins[[13]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 13)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_UA_bins[[15]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_UA_bins[[15]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_UA_bins[[15]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_UA_bins[[15]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_UA_bins[[15]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_UA_bins[[15]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_UA_bins[[15]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_UA_bins[[15]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 15)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_UA_bins[[17]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_UA_bins[[17]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_UA_bins[[17]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_UA_bins[[17]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_UA_bins[[17]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_UA_bins[[17]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_UA_bins[[17]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_UA_bins[[17]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 17)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)



#Gbull-Orn
par(mfrow=c(3,3),mar=c(0,0,0,0))
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_Orn_bins[[1]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_Orn_bins[[1]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[1]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[1]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_Orn_bins[[1]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_Orn_bins[[1]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_Orn_bins[[1]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_Orn_bins[[1]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gb parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 1)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_Orn_bins[[3]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_Orn_bins[[3]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[3]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[3]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_Orn_bins[[3]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_Orn_bins[[3]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_Orn_bins[[3]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_Orn_bins[[3]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gb parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)
   


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_Orn_bins[[5]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_Orn_bins[[5]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[5]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[5]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_Orn_bins[[5]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_Orn_bins[[5]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_Orn_bins[[5]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_Orn_bins[[5]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gb parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 5)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_Orn_bins[[7]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_Orn_bins[[7]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[7]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[7]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_Orn_bins[[7]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_Orn_bins[[7]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_Orn_bins[[7]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_Orn_bins[[7]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gb parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 7)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)
   

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_Orn_bins[[9]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_Orn_bins[[9]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[9]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[9]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_Orn_bins[[9]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_Orn_bins[[9]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_Orn_bins[[9]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_Orn_bins[[9]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gb parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 9)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_Orn_bins[[11]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_Orn_bins[[11]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[11]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[11]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_Orn_bins[[11]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_Orn_bins[[11]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_Orn_bins[[11]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_Orn_bins[[11]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gb parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 11)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_Orn_bins[[13]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_Orn_bins[[13]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[13]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[13]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_Orn_bins[[13]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_Orn_bins[[13]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_Orn_bins[[13]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_Orn_bins[[13]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gb parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 13)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_Orn_bins[[15]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_Orn_bins[[15]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[15]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[15]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_Orn_bins[[15]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_Orn_bins[[15]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_Orn_bins[[15]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_Orn_bins[[15]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gb parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 15)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_Orn_bins[[17]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_Orn_bins[[17]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[17]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[17]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_Orn_bins[[17]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_Orn_bins[[17]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_Orn_bins[[17]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_Orn_bins[[17]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gb parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 17)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   


#Gbull-LA
par(mfrow=c(3,3),mar=c(0,0,0,0))
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_LA_bins[[1]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_LA_bins[[1]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[1]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[1]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_LA_bins[[1]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_LA_bins[[1]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_LA_bins[[1]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_LA_bins[[1]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 1)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)



plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_LA_bins[[3]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_LA_bins[[3]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[3]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[3]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_LA_bins[[3]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_LA_bins[[3]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_LA_bins[[3]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_LA_bins[[3]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_LA_bins[[5]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_LA_bins[[5]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[5]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[5]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_LA_bins[[5]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_LA_bins[[5]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_LA_bins[[5]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_LA_bins[[5]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 5)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_LA_bins[[7]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_LA_bins[[7]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[7]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[7]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_LA_bins[[7]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_LA_bins[[7]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_LA_bins[[7]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_LA_bins[[7]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 7)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_LA_bins[[9]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_LA_bins[[9]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[9]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[9]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_LA_bins[[9]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_LA_bins[[9]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_LA_bins[[9]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_LA_bins[[9]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 9)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_LA_bins[[11]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_LA_bins[[11]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[11]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[11]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_LA_bins[[11]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_LA_bins[[11]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_LA_bins[[11]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_LA_bins[[11]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 11)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_LA_bins[[13]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_LA_bins[[13]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[13]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[13]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_LA_bins[[13]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_LA_bins[[13]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_LA_bins[[13]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_LA_bins[[13]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 13)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_LA_bins[[15]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_LA_bins[[15]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[15]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[15]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_LA_bins[[15]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_LA_bins[[15]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_LA_bins[[15]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_LA_bins[[15]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 15)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_LA_bins[[17]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_LA_bins[[17]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[17]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[17]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_LA_bins[[17]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_LA_bins[[17]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_LA_bins[[17]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_LA_bins[[17]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 17)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)




#Gbull-
par(mfrow=c(3,3),mar=c(0,0,0,0))
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_UA_bins[[1]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_UA_bins[[1]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[1]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[1]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_UA_bins[[1]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_UA_bins[[1]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_UA_bins[[1]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_UA_bins[[1]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 1)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_UA_bins[[3]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_UA_bins[[3]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[3]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[3]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_UA_bins[[3]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_UA_bins[[3]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_UA_bins[[3]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_UA_bins[[3]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_UA_bins[[5]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_UA_bins[[5]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[5]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[5]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_UA_bins[[5]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_UA_bins[[5]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_UA_bins[[5]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_UA_bins[[5]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 5)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_UA_bins[[7]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_UA_bins[[7]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[7]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[7]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_UA_bins[[7]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_UA_bins[[7]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_UA_bins[[7]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_UA_bins[[7]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 7)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_UA_bins[[9]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_UA_bins[[9]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[9]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[9]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_UA_bins[[9]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_UA_bins[[9]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_UA_bins[[9]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_UA_bins[[9]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 9)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_UA_bins[[11]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_UA_bins[[11]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[11]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[11]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_UA_bins[[11]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_UA_bins[[11]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_UA_bins[[11]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_UA_bins[[11]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 11)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_UA_bins[[13]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_UA_bins[[13]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[13]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[13]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_UA_bins[[13]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_UA_bins[[13]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_UA_bins[[13]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_UA_bins[[13]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 13)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_UA_bins[[15]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_UA_bins[[15]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[15]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[15]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_UA_bins[[15]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_UA_bins[[15]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_UA_bins[[15]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_UA_bins[[15]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 15)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_UA_bins[[17]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_UA_bins[[17]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[17]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[17]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_UA_bins[[17]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_UA_bins[[17]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_UA_bins[[17]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_UA_bins[[17]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 17)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   


#Day 3
par(mfrow=c(3,3),mar=c(0,0,0,0))
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_Orn_bins[[3]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_Orn_bins[[3]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[3]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[3]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_Orn_bins[[3]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_Orn_bins[[3]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_Orn_bins[[3]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_Orn_bins[[3]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_LA_bins[[3]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_LA_bins[[3]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[3]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[3]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_LA_bins[[3]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_LA_bins[[3]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_LA_bins[[3]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_LA_bins[[3]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)



plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_UA_bins[[3]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_UA_bins[[3]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[3]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[3]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_UA_bins[[3]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_UA_bins[[3]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_UA_bins[[3]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_UA_bins[[3]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_Orn_bins[[3]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_Orn_bins[[3]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[3]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[3]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_Orn_bins[[3]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_Orn_bins[[3]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_Orn_bins[[3]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_Orn_bins[[3]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_LA_bins[[3]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_LA_bins[[3]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_LA_bins[[3]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_LA_bins[[3]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_LA_bins[[3]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_LA_bins[[3]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_LA_bins[[3]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_LA_bins[[3]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_UA_bins[[3]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_UA_bins[[3]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_UA_bins[[3]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_UA_bins[[3]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_UA_bins[[3]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_UA_bins[[3]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_UA_bins[[3]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_UA_bins[[3]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_Orn_bins[[3]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_Orn_bins[[3]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[3]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[3]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_Orn_bins[[3]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_Orn_bins[[3]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_Orn_bins[[3]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_Orn_bins[[3]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gb parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)
   
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_LA_bins[[3]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_LA_bins[[3]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[3]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[3]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_LA_bins[[3]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_LA_bins[[3]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_LA_bins[[3]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_LA_bins[[3]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_UA_bins[[3]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_UA_bins[[3]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[3]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[3]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_UA_bins[[3]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_UA_bins[[3]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_UA_bins[[3]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_UA_bins[[3]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


#plot.new()
#plot.new()
myColors <- brewer.pal(8,"Greys")
source("http://www.math.mcmaster.ca/bolker/R/misc/legendx.R")
#graph=function(){
#par(oma=c(2,0,0,0))
#plot.new()
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)

legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
#expression("T ">="5")
#text<-paste(expression("<=0.5"),"(0.5,1]","(1.0,1.5]","(1.5,2.0]","(2.0,2.5]","(2.5,3.0]","(3.0,3.5]",">3.5")
text <- c(expression(""<=0.5),">0.5",">1.0",">1.5",">2.0",">2.5",">3.0",expression("">=3.5))
#par(xpd=TRUE)
legend("topright",legend = text,fill=myColors,bty = "n",box.cex=c(4,4),y.intersp=3,horiz = F,cex=1)
    
#mtext("Log 
#Mean Intensity", side = 3,line=1, font=2, cex=1.2,at=0.85)






#doc <- read_pptx()
#doc <- add_slide(doc, 'Title and Content', 'Office Theme')
#doc <- ph_with_vg(doc, code = graph(),type = "body")

# Write the document to a file
#print(doc, target = 'Heatmap_legend.pptx')


#Day 3

Graph=function(i,width){
op=par(mfrow=c(3,3),mar=c(0,0,0,0),oma=c(0,0,3,0))

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=width,col=myColors[Gt3_Orn_bins[[i]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=width,
           col=myColors[Gt3_Orn_bins[[i]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=width,col=myColors[Gt3_Orn_bins[[i]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=width,col=myColors[Gt3_Orn_bins[[i]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=width,col=myColors[Gt3_Orn_bins[[i]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=width,col=myColors[Gt3_Orn_bins[[i]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,
               .3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=width,
             col=myColors[Gt3_Orn_bins[[i]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=width,
                 col=myColors[Gt3_Orn_bins[[i]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,
      .25+.4),border="black",lwd=width)

#text(0.5,.3,"Gt3 on OS fish",col="black",lwd=3,pch=19,font=4,cex=2)
#text(0.5,.3," Ornamental fish ",col="black",lwd=3,pch=19,font=4,pos =4,,cex = 1.1)

#text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=width,col=myColors[Gt3_LA_bins[[i]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=width,
           col=myColors[Gt3_LA_bins[[i]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=width,col=myColors[Gt3_LA_bins[[i]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=width,col=myColors[Gt3_LA_bins[[i]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=width,col=myColors[Gt3_LA_bins[[i]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=width,col=myColors[Gt3_LA_bins[[i]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+
               .4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=width,
             col=myColors[Gt3_LA_bins[[i]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=width,
                 col=myColors[Gt3_LA_bins[[i]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+
      .4,.25+.4),border="black",lwd=width)

#text(0.5,.3,"Gt3 on LA fish ",col="black",lwd=3,pch=19,font=4,cex=2)
#text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)



plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=width,col=myColors[Gt3_UA_bins[[i]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=width,
           col=myColors[Gt3_UA_bins[[i]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=width,col=myColors[Gt3_UA_bins[[i]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=width,col=myColors[Gt3_UA_bins[[i]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=width,col=myColors[Gt3_UA_bins[[i]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=width,col=myColors[Gt3_UA_bins[[i]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,
               .325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=width,
             col=myColors[Gt3_UA_bins[[i]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=width,
                 col=myColors[Gt3_UA_bins[[i]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+
      .4,.255+.4,.25+.4),border="black",lwd=width)

#text(0.5,.3,"Gt3 on UA fish ",col="black",lwd=3,pch=19,font=4,cex=2)
#text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)

   
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=width,col=myColors[Gt_Orn_bins[[i]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=width,
           col=myColors[Gt_Orn_bins[[i]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=width,col=myColors[Gt_Orn_bins[[i]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=width,col=myColors[Gt_Orn_bins[[i]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=width,col=myColors[Gt_Orn_bins[[i]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=width,col=myColors[Gt_Orn_bins[[i]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+
               .4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=width,
             col=myColors[Gt_Orn_bins[[i]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=width,
                 col=myColors[Gt_Orn_bins[[i]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+
      .4,.25+.4),border="black",lwd=width)

#text(0.5,.3,"Gt on OS fish ",col="black",lwd=3,pch=19,font=4,cex=2)
#text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)

   plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=width,col=myColors[Gt_LA_bins[[i]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=width,
           col=myColors[Gt_LA_bins[[i]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,
               .0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=width,col=myColors[Gt_LA_bins[[i]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=width,col=myColors[Gt_LA_bins[[i]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=width,col=myColors[Gt_LA_bins[[i]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=width,col=myColors[Gt_LA_bins[[i]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+
               .4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=width,
             col=myColors[Gt_LA_bins[[i]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=width,
                 col=myColors[Gt_LA_bins[[i]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4)
            ,border="black",lwd=width)


#text(0.5,.3,"Gt on LA fish ",col="black",lwd=3,pch=19,font=4,cex=2)
#text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=width,col=myColors[Gt_UA_bins[[i]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=width,
           col=myColors[Gt_UA_bins[[i]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=width,col=myColors[Gt_UA_bins[[i]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=width,col=myColors[Gt_UA_bins[[i]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=width,col=myColors[Gt_UA_bins[[i]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=width,col=myColors[Gt_UA_bins[[i]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,
               .325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=width,
             col=myColors[Gt_UA_bins[[i]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=width,
                 col=myColors[Gt_UA_bins[[i]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4)
            ,border="black",lwd=width)


#text(0.5,.3,"Gt on UA fish ",col="black",lwd=3,pch=19,font=4,cex=2)
#text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)

   plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=width,col=myColors[Gbull_Orn_bins[[i]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=width,
           col=myColors[Gbull_Orn_bins[[i]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=width,col=myColors[Gbull_Orn_bins[[i]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=width,col=myColors[Gbull_Orn_bins[[i]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=width,col=myColors[Gbull_Orn_bins[[i]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=width,col=myColors[Gbull_Orn_bins[[i]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+
               .4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=width,
             col=myColors[Gbull_Orn_bins[[i]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=width,
                 col=myColors[Gbull_Orn_bins[[i]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4)
            ,border="black",lwd=width)


#text(0.5,.3,"Gb on OS fish ",col="black",lwd=3,pch=19,font=4,cex=2)
#text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)
   
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=width,col=myColors[Gbull_LA_bins[[i]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=width,
           col=myColors[Gbull_LA_bins[[i]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=width,col=myColors[Gbull_LA_bins[[i]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=width,col=myColors[Gbull_LA_bins[[i]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=width,col=myColors[Gbull_LA_bins[[i]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=width,col=myColors[Gbull_LA_bins[[i]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,
               .3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=width,
             col=myColors[Gbull_LA_bins[[i]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=width,
                 col=myColors[Gbull_LA_bins[[i]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4)
            ,border="black",lwd=width)

#text(0.5,.3,"Gb on LA fish ",col="black",lwd=3,pch=19,font=4,cex=2)
#text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=width,col=myColors[Gbull_UA_bins[[i]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=width,
           col=myColors[Gbull_UA_bins[[i]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=width,col=myColors[Gbull_UA_bins[[i]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=width,col=myColors[Gbull_UA_bins[[i]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=width,col=myColors[Gbull_UA_bins[[i]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=width,col=myColors[Gbull_UA_bins[[i]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,
               .325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=width,
             col=myColors[Gbull_UA_bins[[i]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=width,
                 col=myColors[Gbull_UA_bins[[i]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),
            border="black",lwd=width)

#text(0.5,.3,"Gb on UA fish ",col="black",lwd=3,pch=19,font=4,cex=2)
#text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)




par(op) # Leave the last plot
mtext("Day 17", side = 3,line=2, font=2, cex=3,at =.5)
op <- par(usr=c(0,1,0,1), # Reset the coordinates
          xpd=NA)         # Allow plotting outside the plot region
}

Graph(i=17,width=2)
#doc <- read_pptx()
#doc <- add_slide(doc, 'Title and Content', 'Office Theme')
#doc <- ph_with_vg(doc, code = Graph(i=11,width=2),type = "body")

#Write the document to a file
#print(doc, target = 'Heatmap_Day15.pptx')


#Day 3
i=3
op=par(mfrow=c(3,3),mar=c(0,0,0,0),oma=c(0,0,3,0))

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_Orn_bins[[i]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_Orn_bins[[i]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[i]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[i]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_Orn_bins[[i]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_Orn_bins[[i]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_Orn_bins[[i]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_Orn_bins[[i]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 on OS fish",col="black",lwd=3,pch=19,font=4,cex=2)
#text(0.5,.3," Ornamental fish ",col="black",lwd=3,pch=19,font=4,pos =4,,cex = 1.1)

#text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_LA_bins[[i]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_LA_bins[[i]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[i]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[i]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_LA_bins[[i]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_LA_bins[[i]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_LA_bins[[i]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_LA_bins[[i]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 on LA fish ",col="black",lwd=3,pch=19,font=4,cex=2)
#text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)



plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_UA_bins[[i]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_UA_bins[[i]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[i]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[i]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_UA_bins[[i]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_UA_bins[[i]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_UA_bins[[i]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_UA_bins[[i]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 on UA fish ",col="black",lwd=3,pch=19,font=4,cex=2)
#text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)

   
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_Orn_bins[[i]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_Orn_bins[[i]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[i]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[i]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_Orn_bins[[i]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_Orn_bins[[i]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_Orn_bins[[i]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_Orn_bins[[i]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt on OS fish ",col="black",lwd=3,pch=19,font=4,cex=2)
#text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)

   plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_LA_bins[[i]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_LA_bins[[i]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_LA_bins[[i]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_LA_bins[[i]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_LA_bins[[i]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_LA_bins[[i]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_LA_bins[[i]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_LA_bins[[i]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt on LA fish ",col="black",lwd=3,pch=19,font=4,cex=2)
#text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_UA_bins[[i]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_UA_bins[[i]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_UA_bins[[i]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_UA_bins[[i]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_UA_bins[[i]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_UA_bins[[i]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_UA_bins[[i]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_UA_bins[[i]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt on UA fish ",col="black",lwd=3,pch=19,font=4,cex=2)
#text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)

   plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_Orn_bins[[i]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_Orn_bins[[i]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[i]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[i]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_Orn_bins[[i]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_Orn_bins[[i]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_Orn_bins[[i]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_Orn_bins[[i]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gb on OS fish ",col="black",lwd=3,pch=19,font=4,cex=2)
#text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)
   
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_LA_bins[[i]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_LA_bins[[i]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[i]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[i]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_LA_bins[[i]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_LA_bins[[i]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_LA_bins[[i]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_LA_bins[[i]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb on LA fish ",col="black",lwd=3,pch=19,font=4,cex=2)
#text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_UA_bins[[i]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_UA_bins[[i]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[i]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[i]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_UA_bins[[i]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_UA_bins[[i]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_UA_bins[[i]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_UA_bins[[i]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb on UA fish ",col="black",lwd=3,pch=19,font=4,cex=2)
#text(0.5,.2,"(Day 3)",col="black",lwd=3,cex = .8,pos=3,font=2)




par(op) # Leave the last plot
mtext("Day 3", side = 3,line=2, font=2, cex=1.2,at =.8)
op <- par(usr=c(0,1,0,1), # Reset the coordinates
          xpd=NA)         # Allow plotting outside the plot region

legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
#par(xpd=TRUE)
legend(-.1,1.15,legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.99,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors, box.col=NA)


Graph=function()
    
    
doc <- read_pptx()
doc <- add_slide(doc, 'Title and Content', 'Office Theme')
doc <- ph_with_vg(doc, code = Graph(Time=Time[[i]]),type = "body")

# Write the document to a file
print(doc, target = 'Gt3_fish.pptx')

#Day 7
par(mfrow=c(3,3),mar=c(0,0,0,0))
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_Orn_bins[[7]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_Orn_bins[[7]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[7]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[7]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_Orn_bins[[7]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_Orn_bins[[7]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_Orn_bins[[7]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_Orn_bins[[7]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 7)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)



plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_LA_bins[[7]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_LA_bins[[7]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[7]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[7]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_LA_bins[[7]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_LA_bins[[7]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_LA_bins[[7]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_LA_bins[[7]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 7)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_UA_bins[[7]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_UA_bins[[7]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[7]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[7]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_UA_bins[[7]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_UA_bins[[7]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_UA_bins[[7]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_UA_bins[[7]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 7)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_Orn_bins[[7]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_Orn_bins[[7]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[7]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[7]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_Orn_bins[[7]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_Orn_bins[[7]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_Orn_bins[[7]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_Orn_bins[[7]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 7)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_LA_bins[[7]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_LA_bins[[7]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_LA_bins[[7]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_LA_bins[[7]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_LA_bins[[7]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_LA_bins[[7]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_LA_bins[[7]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_LA_bins[[7]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 7)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_UA_bins[[7]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_UA_bins[[7]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_UA_bins[[7]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_UA_bins[[7]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_UA_bins[[7]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_UA_bins[[7]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_UA_bins[[7]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_UA_bins[[7]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 7)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_Orn_bins[[7]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_Orn_bins[[7]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[7]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[7]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_Orn_bins[[7]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_Orn_bins[[7]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_Orn_bins[[7]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_Orn_bins[[7]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gb parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 7)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)
   
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_LA_bins[[7]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_LA_bins[[7]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[7]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[7]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_LA_bins[[7]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_LA_bins[[7]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_LA_bins[[7]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_LA_bins[[7]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 7)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_UA_bins[[7]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_UA_bins[[7]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[7]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[7]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_UA_bins[[7]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_UA_bins[[7]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_UA_bins[[7]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_UA_bins[[7]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 7)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


#Day 11
par(mfrow=c(3,3),mar=c(0,0,0,0))
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_Orn_bins[[11]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_Orn_bins[[11]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[11]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[1]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_Orn_bins[[11]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_Orn_bins[[11]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_Orn_bins[[11]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_Orn_bins[[11]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 11)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_LA_bins[[11]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_LA_bins[[11]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[11]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[11]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_LA_bins[[11]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_LA_bins[[11]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_LA_bins[[11]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_LA_bins[[11]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 11)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_UA_bins[[11]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_UA_bins[[11]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[11]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[11]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_UA_bins[[11]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_UA_bins[[11]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_UA_bins[[11]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_UA_bins[[11]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 11)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_Orn_bins[[11]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_Orn_bins[[11]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[11]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[11]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_Orn_bins[[11]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_Orn_bins[[11]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_Orn_bins[[11]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_Orn_bins[[11]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 11)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_LA_bins[[11]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_LA_bins[[11]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_LA_bins[[11]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_LA_bins[[11]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_LA_bins[[11]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_LA_bins[[11]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_LA_bins[[11]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_LA_bins[[11]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 11)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_UA_bins[[11]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_UA_bins[[11]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_UA_bins[[11]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_UA_bins[[11]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_UA_bins[[11]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_UA_bins[[11]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_UA_bins[[11]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_UA_bins[[11]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 11)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)
   
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_Orn_bins[[11]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_Orn_bins[[11]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[11]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[11]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_Orn_bins[[11]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_Orn_bins[[11]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_Orn_bins[[11]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_Orn_bins[[11]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gb parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 11)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_LA_bins[[11]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_LA_bins[[11]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[11]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[11]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_LA_bins[[11]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_LA_bins[[11]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_LA_bins[[11]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_LA_bins[[11]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 11)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_UA_bins[[11]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_UA_bins[[11]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[11]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[11]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_UA_bins[[11]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_UA_bins[[11]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_UA_bins[[11]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_UA_bins[[11]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 11)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   


#Day 15
par(mfrow=c(3,3),mar=c(0,0,0,0))
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_Orn_bins[[15]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_Orn_bins[[15]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[15]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_Orn_bins[[15]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_Orn_bins[[15]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_Orn_bins[[15]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_Orn_bins[[15]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_Orn_bins[[15]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 15)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)


plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_LA_bins[[15]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_LA_bins[[15]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[15]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_LA_bins[[15]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_LA_bins[[15]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_LA_bins[[15]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_LA_bins[[15]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_LA_bins[[15]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 15)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt3_UA_bins[[15]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt3_UA_bins[[15]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[15]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt3_UA_bins[[15]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt3_UA_bins[[15]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt3_UA_bins[[15]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt3_UA_bins[[15]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt3_UA_bins[[15]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt3 parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 15)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

   plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_Orn_bins[[15]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_Orn_bins[[15]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[15]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_Orn_bins[[15]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_Orn_bins[[15]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_Orn_bins[[15]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_Orn_bins[[15]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_Orn_bins[[15]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gt parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 15)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)
   
plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_LA_bins[[15]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_LA_bins[[15]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_LA_bins[[15]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_LA_bins[[15]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_LA_bins[[15]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_LA_bins[[15]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_LA_bins[[15]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_LA_bins[[15]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 15)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gt_UA_bins[[15]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gt_UA_bins[[15]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gt_UA_bins[[15]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gt_UA_bins[[15]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gt_UA_bins[[15]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gt_UA_bins[[15]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gt_UA_bins[[15]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gt_UA_bins[[15]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gt parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 15)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_Orn_bins[[15]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_Orn_bins[[15]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[15]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_Orn_bins[[15]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_Orn_bins[[15]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_Orn_bins[[15]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_Orn_bins[[15]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_Orn_bins[[15]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)


text(0.5,.3,"Gb parasites on Ornamental fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 15)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_LA_bins[[15]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_LA_bins[[15]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[15]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_LA_bins[[15]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_LA_bins[[15]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_LA_bins[[15]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_LA_bins[[15]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_LA_bins[[15]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on LA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 15)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)

plot.new()
Tail=polygon(c(.075,.1,.125,.175,.225,.25,.26,.275,.26,.25,.23,.227,.24,.25,.26,.275,.22,.20,.15,.1,.075,.05,.025,.018,.015,.015,.015,.03,.05,.075),
         c(.0576+.4,.05+.4,.0478+.4,.06+.4,.08+.4,.105+.4,.12+.4,.16+.4,.165+.4,.175+.4,.20+.4,.227+.4,.25+.4,.265+.4,.27+.4,
           .275+.4,.31+.4,.315+.4,.32+.4,.32+.4,.31+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.15+.4,.11+.4,.08+.4,.0576+.4),
             border="black",lwd=3,col=myColors[Gbull_UA_bins[[15]]][1])
LB=polygon(c(.3,.35,.4,.425,.45,.46,.475,.5,.52,.55,.575,.6,.61,.63,.653,.65,.62,.58,.55,.525,.5,.45,.4,.35,.3,
     0.275,.26,.25,.24,.227,.23,.25,.26,.275,        .3),
        c(.17+.4,.176+.4,.178+.4,.18+.4,.179+.4,.178+.4,.175+.4,.16+.4,.15+.4,.126+.4,.125+.4,.125+.4,.1255+.4,
             .1258+.4,.126+.4,.15+.4,.2+.4,.25+.4,.31+.4,.33+.4,.325+.4,.31+.4,.30+.4,.280+.4,.276+.4,
        .275+.4,.27+.4,.266+.4,.25+.4,.227+.4,.20+.4,.175+.4,.165+.4,.16+.4,.17+.4),
           border="black",lwd=3,
           col=myColors[Gbull_UA_bins[[15]]][3])
Anal=polygon(c(.46,.45,.425,.41,.408,.408,.41,.415,.427,.45,.46,.475,   .4755,  .472, .5,.51,.52,.52,.52,.5,.475,.46),
             c(.178+.4,.1765+.4,.175+.4,.16+.4,.15+.4,.13+.4,.124+.4,.1+.4,.0575+.4,.03+.4,.025+.4,.0245+.4, .024+.4, .023+.4,.045+.4,
               .057+.4,.1+.4,.126+.4,.15+.4,.16+.4,.175+.4,.178+.4)
             ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[15]]][2])
Pelvic=polygon(c(.6,.59,.575,.58,.6,.605,.615,.64,.65,.653,.63,.61,.605,.6),
              c(0.125+.4,.1256+.4,.11+.4,.1+.4,.055+.4,.054+.4,.058+.4,.09+.4,.11+.4,.126+.4,.1258+.4,.1255+.4,.124+.4,.125+.4)
               ,border="black",lwd=3,col=myColors[Gbull_UA_bins[[15]]][5])
Dorsal=polygon(c(.4,.38,.378,.379,.38,.4,.41,.428,.45,.46,.475,.5,.51,.525,.5, .45,.4),
              c(.3+.4,.325+.4,.33+.4,.35+.4,.36+.4,.378+.4,.381+.4,.385+.4,.384+.4,.383+.4,.375+.4,.36+.4,.35+.4,
                .33+.4,.325+.4,.31+.4,.30+.4),border="black",lwd=3,col=myColors[Gbull_UA_bins[[15]]][7])
UB=polygon(c(.653,.7,.725,.75,.77,.8,.83,.835,.84,.837,.8335,.83,.829,.830,.837,.8,.77,.725,.7,.65,.625,.6,.575,.55,.525,.55,.58,.62,.65,.653),
           c(.126+.4,.13+.4,.135+.4,.14+.4,.148+.4,.16+.4,.18+.4,.185+.4,.20+.4,.227+.4,.25+.4,.275+.4,.3+.4,.31+.4,.325+.4,.328+.4,.329+.4,.33+.4,.335+.4,.338+.4,.335+.4,.335+.4,.335+.4,.334+.4,.33+.4,
             .31+.4,.25+.4,.2+.4,.15+.4,.126+.4),
           border="black",lwd=3,col=myColors[Gbull_UA_bins[[15]]][4])
Head=polygon(c(.85,.86,.875,.9,.925,.93,.955,.96,.961,.955,.925,.875,.837,.830,.837,.829,.83,.8335,.837,.84,.845,.85),
             c(.18+.4,.185+.4,.19+.4,.2+.4,.22+.4,.23+.4,.27+.4,.28+.4,.282+.4,.30+.4,.31+.4,.325+.4,.325+.4,.31+.4,.325+.4,.3+.4,.275+.4,.25+.4,.227+.4,.2+.4,.185+.4,.18+.4),border="black",lwd=3,
             col=myColors[Gbull_UA_bins[[15]]][8])
Pectoral=polygon(c(.675,.7,.725,.75,.772, .774,   .775,.775,.76,.75,.725,.7,.675,.65,.645,.645,.65,.66,.675),
                  c(.175+.4,.178+.4,.18+.4,.2+.4,.220+.4, .223+.4,  .228+.4,.25+.4,.27+.4,.275+.4,.282+.4,.29+.4,
                    .295+.4,.27+.4,.25+.4,.227+.4,.2+.4,.18+.4,.175+.4),border="black",lwd=3,
                 col=myColors[Gbull_UA_bins[[15]]][6])
Eye=polygon(c(.8875,.9,.905,.907,.908,.9,.8875,.875,.86,.855 ,.855,.856,.86,.87,.8875),
    c(.25+.4,.26+.4,.265+.4,.275+.4,.28+.4,.3+.4,.31+.4,.308+.4,.30+.4,0.28+.4,.275+.4,.265+.4,.26+.4,.255+.4,.25+.4),border="black",lwd=3)

text(0.5,.3,"Gb parasites on UA fish ",col="black",lwd=3,pch=19,font=4)
text(0.5,.2,"(Day 15)",col="black",lwd=3,cex = .8,pos=3,font=2)
legend_order <- matrix(1:8,ncol=4,byrow = TRUE)
text <- c("0-","0.5-","1-","1.5-","2-","2.5-","3-",">3.5")
par(xpd=TRUE)
legend(x = "top",legend = text[legend_order],
       col=myColors[legend_order],ncol=4, lwd=3, cex=.65,bty = "o",title="Log Mean Intensity",
      lty = c(1,1,1,1,1,1,1,1)[legend_order],xpd=TRUE,pch=19,fill=myColors)



