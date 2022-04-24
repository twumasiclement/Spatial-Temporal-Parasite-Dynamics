options(repr.plot.width=8, repr.plot.height=8,repr.plot.res = 300) #Setting plot size

#Installing packages
library(dplyr)
library(MASS)
library(plotly)
library(ggplot2)
library(lattice)
library(tidyverse) #for data transformation
library(magrittr) #for piping
library(forcats) #for factor manipulation
#library("devtools")
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

#library(tidyr)
library(reshape2)
#library(reprtree) #for ploting random forest tree
library("gbm") #Gradient boosting machine
#library("dismo")
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
library("ggpubr")
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
#library(fishplot)

library(sp)  # vector data
#library(raster)  # raster data
library(rgdal)  # input/output, projections

library("rgeos")  # geometry ops
#library("spdep")  # spatial dependence
library(maptools)

#Multivariate analysis
library(mvtnorm)
#library(MVN)
#library("Hotelling")
#library("mvnormtest")
#library("heplots")
#library(lattice)
#library(Matrix)

#For Non-parametric pairwise comparison
library("PMCMRplus")

library(ggplot2)
library(gridExtra)
library(grid)
library(cowplot)
library(ggpattern)

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

#Function to transform data for multivariate analysis
Catogories=c("Gt3-Orn","Gt3-LA","Gt3-UA","Gt-Orn","Gt-LA","Gt-UA","Gb-Orn","Gb-LA","Gb-UA")
Time=seq(1,17,by=2)
Bodyparts=list()
Dataframe_Gyro=function(Data,Group_no,time_index){
Bodyparts[[1]]=split(Data[,-c(1,4)],Data$Bodypart)$"Tail"
Bodyparts[[2]]=split(Data[,-c(1,4)],Data$Bodypart)$"Anal"
Bodyparts[[3]]=split(Data[,-c(1,4)],Data$Bodypart)$"LB"
Bodyparts[[4]]=split(Data[,-c(1,4)],Data$Bodypart)$"UB"
Bodyparts[[5]]=split(Data[,-c(1,4)],Data$Bodypart)$"Pelvic"
Bodyparts[[6]]=split(Data[,-c(1,4)],Data$Bodypart)$"Pectoral"
Bodyparts[[7]]=split(Data[,-c(1,4)],Data$Bodypart)$"Dorsal"
Bodyparts[[8]]=split(Data[,-c(1,4)],Data$Bodypart)$"Head"
    
Tail=as.vector(Bodyparts[[1]]$Parasite)
Anal=as.vector(Bodyparts[[2]]$Parasite)
LB=as.vector(Bodyparts[[3]]$Parasite)
UB=as.vector(Bodyparts[[4]]$Parasite)
Pelvic=as.vector(Bodyparts[[5]]$Parasite)
Pectoral=as.vector(Bodyparts[[6]]$Parasite)
Dorsal=as.vector(Bodyparts[[7]]$Parasite)
Head=as.vector(Bodyparts[[8]]$Parasite) 
Category=rep(Catogories[Group_no],length(unique(Data$ID)))
time_variable=rep(time_index,length(unique(Data$ID)))
return(data.frame(Tail=Tail,Anal=Anal,LB=LB,UB=UB,Pelvic=Pelvic,Pectoral=Pectoral,Dorsal=Dorsal,Head=Head,
                 Group=Category,Day=time_variable)) 
}


#Function to combine transformed data
CombData_func=function(data1,data2,data3,data4,data5,data6,data7,data8,data9)
    {
    return(rbind(data1,data2,data3,data4,data5,data6,data7,data8,data9))
}

#Transformed data for each time point
OrnGt3=list();LAGt3=list();UAGt3=list();OrnGt=list();LAGt=list();UAGt=list();OrnGbull=list();LAGbull=list();UAGbull=list()

for (i in Time)
    {
OrnGt3[[i]]=Dataframe_Gyro(Data=Trans_Orn_Gt3[[i]],Group_no=1,time_index=i)
LAGt3[[i]]=Dataframe_Gyro(Data=Trans_LA_Gt3[[i]],Group_no=2,time_index=i)
UAGt3[[i]]=Dataframe_Gyro(Data=Trans_UA_Gt3[[i]],Group_no=3,time_index=i)
OrnGt[[i]]=Dataframe_Gyro(Data=Trans_Orn_Gt[[i]],Group_no=4,time_index=i)
LAGt[[i]]=Dataframe_Gyro(Data=Trans_LA_Gt[[i]],Group_no=5,time_index=i)
UAGt[[i]]=Dataframe_Gyro(Data=Trans_UA_Gt[[i]],Group_no=6,time_index=i)
OrnGbull[[i]]=Dataframe_Gyro(Data=Trans_Orn_Gbull[[i]],Group_no=7,time_index=i)
LAGbull[[i]]=Dataframe_Gyro(Data=Trans_LA_Gbull[[i]],Group_no=8,time_index=i)
UAGbull[[i]]=Dataframe_Gyro(Data=Trans_UA_Gbull[[i]],Group_no=9,time_index=i)
}

#Combining data
MultiData=list()
for (i in Time){
MultiData[[i]]=CombData_func(data1=OrnGt3[[i]],data2=LAGt3[[i]],data3=UAGt3[[i]],
                             data4=OrnGt[[i]],data5=LAGt[[i]],data6=UAGt[[i]],
                             data7=OrnGbull[[i]],data8=LAGbull[[i]],data9=UAGbull[[i]])
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
Tail=sum(Bodyparts[[1]]$Parasite)/fish_total
Anal=sum(Bodyparts[[2]]$Parasite)/fish_total
LB=sum(Bodyparts[[3]]$Parasite)/fish_total
UB=sum(Bodyparts[[4]]$Parasite)/fish_total
Pelvic=sum(Bodyparts[[5]]$Parasite)/fish_total
Pectoral=sum(Bodyparts[[6]]$Parasite)/fish_total
Dorsal=sum(Bodyparts[[7]]$Parasite)/fish_total
Head=sum(Bodyparts[[8]]$Parasite)/fish_total

return(c(Tail,Anal,LB,UB,Pelvic,Pectoral,Dorsal,Head))
}


Gt3_Orn_heatmap=list()

for (i in seq_day){
 Gt3_Orn_heatmap[[i]]= Guppy(Trans_Orn_Gt3[[i]]) 
}

for (i in seq_day){
cat("Mean Parasite load of Orn fish for Gt3 at the eight bodyparts are for day:", "",i,"is","",Gt3_Orn_heatmap[[i]],"\n")
    }

Gt_Orn_heatmap=list()

for (i in seq_day){
 Gt_Orn_heatmap[[i]]= Guppy(Trans_Orn_Gt[[i]]) 
}

for (i in seq_day){
cat("Mean parasite load of Orn fish  for Gt at the eight bodyparts are for day:", "",i,"is","",Gt_Orn_heatmap[[i]],"\n")
    }


Gbull_Orn_heatmap=list()

for (i in seq_day){
 Gbull_Orn_heatmap[[i]]= Guppy(Trans_Orn_Gbull[[i]]) 
}

for (i in seq_day){
cat("Mean parasite load of Orn fish for Gbull at the eight bodyparts are for day:", "",i,"is","",Gbull_Orn_heatmap[[i]],"\n")
    }

Gt3_LA_heatmap=list()

for (i in seq_day){
 Gt3_LA_heatmap[[i]]= Guppy(Trans_LA_Gt3[[i]]) 
}

for (i in seq_day){
cat("Mean parasite load of LA fish for Gt3 at the eight bodyparts are for day:", "",i,"is","",Gt3_LA_heatmap[[i]],"\n")
    }

Gt_LA_heatmap=list()

for (i in seq_day){
 Gt_LA_heatmap[[i]]= Guppy(Trans_LA_Gt[[i]]) 
}

for (i in seq_day){
cat("Mean parasite load of LA fish  for Gt at the eight bodyparts are for day:", "",i,"is","",Gt_LA_heatmap[[i]],"\n")
    }

Gbull_LA_heatmap=list()

for (i in seq_day){
 Gbull_LA_heatmap[[i]]= Guppy(Trans_LA_Gbull[[i]]) 
}

for (i in seq_day){
cat("Mean parasite load of LA fish for Gbull at the eight bodyparts are for day:", "",i,"is","",Gbull_LA_heatmap[[i]],"\n")
    }

Gt3_UA_heatmap=list()

for (i in seq_day){
 Gt3_UA_heatmap[[i]]= Guppy(Trans_UA_Gt3[[i]]) 
}

for (i in seq_day){
cat("Mean parasite load of UA fish for Gt3 at the eight bodyparts are for day:", "",i,"is","",Gt3_UA_heatmap[[i]],"\n")
    }

Gt_UA_heatmap=list()

for (i in seq_day){
 Gt_UA_heatmap[[i]]= Guppy(Trans_UA_Gt[[i]]) 
}

for (i in seq_day){
cat("Mean parasite load of UA fish  for Gt at the eight bodyparts are for day:", "",i,"is","",Gt_UA_heatmap[[i]],"\n")
    }

Gbull_UA_heatmap=list()

for (i in seq_day){
 Gbull_UA_heatmap[[i]]= Guppy(Trans_UA_Gbull[[i]]) 
}

for (i in seq_day){
cat("Mean parasite load of UA fish for Gbull at the eight bodyparts are for day:", "",i,"is","",Gbull_UA_heatmap[[i]],"\n")
    }

multkw<- function(group,y,simplify=FALSE){
 ### sort data by group ###
    o<-order(group)
    group<-group[o]
    y<-as.matrix(y[o,])
    n<-length(group)
    k<-dim(y)[2]   #k=p
    
    if (dim(y)[1] != n)
    return("number of observations not equal to length of group")
    groupls<-unique(group)
    g<-length(groupls) #number of groups (Number of fish-parasite combination)#
    groupind<-sapply(groupls,"==",group) #group indicator#
    ni<-colSums(groupind) #num of subj of each group (Number of fish in each group)#
    r<-apply(y,2,rank) #corresponding rank variable (Parasite at each bodyparts)#
    
    ### calculation of statistic ###
    r.ik<-t(groupind)%*%r*(1/ni)  #gxp, mean rank of kth variate in ith group#
    m<- (n+1)/2 #expected value of rik#
    u.ik<-t(r.ik-m)
    U<-as.vector(u.ik)
    V<-1/(n-1)*t(r-m)%*%(r-m) #pooled within-group cov matrix
    Vstar<-bdiag(lapply(1/ni,"*",V))
    W2<-as.numeric(t(U)%*%solve(Vstar)%*%U)
    
    ### return stat and p-value ###
   returnlist<-data.frame(statistic=W2,d.f.=k*(g-1),
   p.value=pchisq(W2,k*(g-1),lower.tail=F))
    
    if (simplify==TRUE) return (W2)
    else return (returnlist)
    }

day=seq(1,17,by=2)
MKW_results=list()
 for (i in day)
     {
MKW_results[[i]]=multkw(group=MultiData[[i]][,"Group"],y=MultiData[[i]][,1:8],simplify=F)
     }

#Day 1
MKW_results[[1]]

#Day 3
MKW_results[[3]]

#Day 5
MKW_results[[5]]

#Day 7
MKW_results[[7]]

#Day 9
MKW_results[[9]]

#Day 11
MKW_results[[11]]

#Day 13
MKW_results[[13]]

#Day 15
MKW_results[[15]]

#Day 17
MKW_results[[17]]

#Day 1
kruskal.test(Tail~ Group, data = MultiData[[1]])
kruskal.test(Anal~ Group, data = MultiData[[1]])
kruskal.test(LB~ Group, data = MultiData[[1]])
kruskal.test(UB~ Group, data = MultiData[[1]])
kruskal.test(Pelvic~ Group, data = MultiData[[1]])
kruskal.test(Pectoral~ Group, data = MultiData[[1]])
kruskal.test(Dorsal~ Group, data = MultiData[[1]])
kruskal.test(Head~ Group, data = MultiData[[1]])

#Day 3
kruskal.test(Tail~ Group, data = MultiData[[3]])
kruskal.test(Anal~ Group, data = MultiData[[3]])
kruskal.test(LB~ Group, data = MultiData[[3]])
kruskal.test(UB~ Group, data = MultiData[[3]])
kruskal.test(Pelvic~ Group, data = MultiData[[3]])
kruskal.test(Pectoral~ Group, data = MultiData[[3]])
kruskal.test(Dorsal~ Group, data = MultiData[[3]])
kruskal.test(Head~ Group, data = MultiData[[3]])

#Day 5
kruskal.test(Tail~ Group, data = MultiData[[5]])
kruskal.test(Anal~ Group, data = MultiData[[5]])
kruskal.test(LB~ Group, data = MultiData[[5]])
kruskal.test(UB~ Group, data = MultiData[[5]])
kruskal.test(Pelvic~ Group, data = MultiData[[5]])
kruskal.test(Pectoral~ Group, data = MultiData[[5]])
kruskal.test(Dorsal~ Group, data = MultiData[[5]])
kruskal.test(Head~ Group, data = MultiData[[5]])

#Day 7
kruskal.test(Tail~ Group, data = MultiData[[7]])
kruskal.test(Anal~ Group, data = MultiData[[7]])
kruskal.test(LB~ Group, data = MultiData[[7]])
kruskal.test(UB~ Group, data = MultiData[[7]])
kruskal.test(Pelvic~ Group, data = MultiData[[7]])
kruskal.test(Pectoral~ Group, data = MultiData[[7]])
kruskal.test(Dorsal~ Group, data = MultiData[[7]])
kruskal.test(Head~ Group, data = MultiData[[7]])

#Day 9
kruskal.test(Tail~ Group, data = MultiData[[9]])
kruskal.test(Anal~ Group, data = MultiData[[9]])
kruskal.test(LB~ Group, data = MultiData[[9]])
kruskal.test(UB~ Group, data = MultiData[[9]])
kruskal.test(Pelvic~ Group, data = MultiData[[9]])
kruskal.test(Pectoral~ Group, data = MultiData[[9]])
kruskal.test(Dorsal~ Group, data = MultiData[[9]])
kruskal.test(Head~ Group, data = MultiData[[9]])

#Day 11
kruskal.test(Tail~ Group, data = MultiData[[11]])
kruskal.test(Anal~ Group, data = MultiData[[11]])
kruskal.test(LB~ Group, data = MultiData[[11]])
kruskal.test(UB~ Group, data = MultiData[[11]])
kruskal.test(Pelvic~ Group, data = MultiData[[11]])
kruskal.test(Pectoral~ Group, data = MultiData[[11]])
kruskal.test(Dorsal~ Group, data = MultiData[[11]])
kruskal.test(Head~ Group, data = MultiData[[11]])

#Day 13
kruskal.test(Tail~ Group, data = MultiData[[13]])
kruskal.test(Anal~ Group, data = MultiData[[13]])
kruskal.test(LB~ Group, data = MultiData[[13]])
kruskal.test(UB~ Group, data = MultiData[[13]])
kruskal.test(Pelvic~ Group, data = MultiData[[13]])
kruskal.test(Pectoral~ Group, data = MultiData[[13]])
kruskal.test(Dorsal~ Group, data = MultiData[[13]])
kruskal.test(Head~ Group, data = MultiData[[13]])

#Day 15
kruskal.test(Tail~ Group, data = MultiData[[15]])
kruskal.test(Anal~ Group, data = MultiData[[15]])
kruskal.test(LB~ Group, data = MultiData[[15]])
kruskal.test(UB~ Group, data = MultiData[[15]])
kruskal.test(Pelvic~ Group, data = MultiData[[15]])
kruskal.test(Pectoral~ Group, data = MultiData[[15]])
kruskal.test(Dorsal~ Group, data = MultiData[[15]])
kruskal.test(Head~ Group, data = MultiData[[15]])

#Day 17
kruskal.test(Tail~ Group, data = MultiData[[17]])
kruskal.test(Anal~ Group, data = MultiData[[17]])
kruskal.test(LB~ Group, data = MultiData[[17]])
kruskal.test(UB~ Group, data = MultiData[[17]])
kruskal.test(Pelvic~ Group, data = MultiData[[17]])
kruskal.test(Pectoral~ Group, data = MultiData[[17]])
kruskal.test(Dorsal~ Group, data = MultiData[[17]])
kruskal.test(Head~ Group, data = MultiData[[17]])

lower_int<- function(x) {

    if(mean(x)==0) return(0)
    else{         
    return( abs(mean(x)-(sqrt(mean(x)/length(x)))))
             }
}
upper_int<- function(x){

    if(mean(x)==0) return(0)
    else{         
    return( abs(mean(x)+(sqrt(mean(x)/length(x)))))
             }
}

Combined_bodyparts=list()
Parasites_group=list()
Tail_region=list()
Head_region=list()
Lower_region=list()
Upper_region=list()

Tail_region_sd=list()
Head_region_sd=list()
Lower_region_sd=list()
Upper_region_sd=list()

Tail_region_lower=list()
Head_region_lower=list()
Lower_region_lower=list()
Upper_region_lower=list()

Tail_region_upper=list()
Head_region_upper=list()
Lower_region_upper=list()
Upper_region_upper=list()

for (i in seq_day){
MultiData[[i]]$Tail_region=MultiData[[i]][,1]
MultiData[[i]]$Lower_region=MultiData[[i]][,2]+MultiData[[i]][,3]+MultiData[[i]][,5]+MultiData[[i]][,7]
MultiData[[i]]$Upper_region=MultiData[[i]][,4]+MultiData[[i]][,6]
MultiData[[i]]$Head_region=MultiData[[i]][,8]
Combined_bodyparts[[i]]=MultiData[[i]][,c(11,12,13,14,9)]  #Combining data to only for body regions for Multivariate analysis
#Compute average parasites at the 4 regions
Tail_region[[i]]=as.vector(tapply(Combined_bodyparts[[i]]$Tail_region,Combined_bodyparts[[i]]$Group, mean)) 
Lower_region[[i]]=as.vector(tapply(Combined_bodyparts[[i]]$Lower_region,Combined_bodyparts[[i]]$Group, mean))
Upper_region[[i]]=as.vector(tapply(Combined_bodyparts[[i]]$Upper_region,Combined_bodyparts[[i]]$Group, mean))
Head_region[[i]]=as.vector(tapply(Combined_bodyparts[[i]]$Head_region,Combined_bodyparts[[i]]$Group, mean))
    
#Compute sd in parasites at the 4 regions
Tail_region_sd[[i]]=as.vector(tapply(Combined_bodyparts[[i]]$Tail_region,Combined_bodyparts[[i]]$Group, sd)) 
Lower_region_sd[[i]]=as.vector(tapply(Combined_bodyparts[[i]]$Lower_region,Combined_bodyparts[[i]]$Group, sd))
Upper_region_sd[[i]]=as.vector(tapply(Combined_bodyparts[[i]]$Upper_region,Combined_bodyparts[[i]]$Group, sd))
Head_region_sd[[i]]=as.vector(tapply(Combined_bodyparts[[i]]$Head_region,Combined_bodyparts[[i]]$Group, sd))
    
#Compute lower and upper confidence intervals at the 4 regions
Tail_region_lower[[i]]=as.vector(tapply(Combined_bodyparts[[i]]$Tail_region,Combined_bodyparts[[i]]$Group,lower_int)) 
Lower_region_lower[[i]]=as.vector(tapply(Combined_bodyparts[[i]]$Lower_region,Combined_bodyparts[[i]]$Group,lower_int))
Upper_region_lower[[i]]=as.vector(tapply(Combined_bodyparts[[i]]$Upper_region,Combined_bodyparts[[i]]$Group,lower_int))
Head_region_lower[[i]]=as.vector(tapply(Combined_bodyparts[[i]]$Head_region,Combined_bodyparts[[i]]$Group,lower_int))
    
Tail_region_upper[[i]]=as.vector(tapply(Combined_bodyparts[[i]]$Tail_region,Combined_bodyparts[[i]]$Group,upper_int)) 
Lower_region_upper[[i]]=as.vector(tapply(Combined_bodyparts[[i]]$Lower_region,Combined_bodyparts[[i]]$Group,upper_int))
Upper_region_upper[[i]]=as.vector(tapply(Combined_bodyparts[[i]]$Upper_region,Combined_bodyparts[[i]]$Group,upper_int))
Head_region_upper[[i]]=as.vector(tapply(Combined_bodyparts[[i]]$Head_region,Combined_bodyparts[[i]]$Group,upper_int))
    }



#Data for mean parasite across the four body regions and fish-parasite combinations
Means_data=list()
for (i in seq_day){
Means_data[[i]]=data.frame(
Combinations=rep(c("Gt3-Orn","Gt3-LA","Gt3-UA","Gt-Orn","Gt-LA","Gt-UA","Gbull-Orn","Gbull-LA","Gbull-UA"),4),
Fish_type=rep(c("OS","LA","UA","OS","LA","UA","OS","LA","UA"),4),
Para_species=rep(c("Gt3","Gt3","Gt3","Gt","Gt","Gt","Gb","Gb","Gb"),4),
Body_regions=c(rep("Tail",9),rep("Lower region",9),rep("Upper region",9),rep("Head",9)),
Parasite_means=c(Tail_region[[i]],Lower_region[[i]],Upper_region[[i]],Head_region[[i]]),
Parasite_sd=c(Tail_region_sd[[i]],Lower_region_sd[[i]],Upper_region_sd[[i]],Head_region_sd[[i]]),
Parasite_lower=c(Tail_region_lower[[i]],Lower_region_lower[[i]],Upper_region_lower[[i]],
                   Head_region_lower[[i]]),
Parasite_upper=c(Tail_region_upper[[i]],Lower_region_upper[[i]],Upper_region_upper[[i]],
                   Head_region_upper[[i]]),
Days=as.factor(rep(i,36))
    )
    }
Means_days=rbind(Means_data[[1]],Means_data[[3]],Means_data[[5]],Means_data[[7]],Means_data[[9]],
                 Means_data[[11]],Means_data[[13]],Means_data[[15]],Means_data[[17]])

head(Means_days,n=10)
#Means_days

Means_data[[1]]

ddply(Means_data[[1]], c("wool", "tension"), summarize, Mean = mean(breaks), SD = sd(breaks))

library(ggpubr)
#library(psych)
detach(package:dplyr)
library(plyr)
#detach(package:plyr)  
library(dplyr)

theme_set(
  theme_bw() +
    theme(legend.position = "top")
  )



p=list()
positions <- c("Tail","Lower region","Upper region","Head")
for (i in seq_day){
p[[i]]=ggplot(Means_data[[i]], aes(x=Fish_type, y=Parasite_means, fill=Body_regions)) + 
    geom_bar(aes(fill = factor(Body_regions, levels=positions)),position=position_dodge(), stat="identity")+
    labs(fill = "Body regions:",x="Fish stocks",y="Mean Intensity")+ theme(
  # Hide panel borders and remove grid lines
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.position = "none" , #hide legend
  # Change axis line
  axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()
  )+ ggtitle(paste("Day",i))+facet_wrap( ~ Para_species ) +scale_fill_manual(values=c("grey88","grey60","grey30","grey0"))
}

prow <- plot_grid( p[[3]] + theme(legend.position="none"),
           p[[7]] + theme(legend.position="none"),
           p[[11]] + theme(legend.position="none"),
           p[[15]] + theme(legend.position="none"),
           align = 'vh',
           hjust = -1,
           nrow = 2
           )

p_no_legend <- lapply(p, function(x) x + theme(legend.position = "none"))
legend <- cowplot::get_legend(p[[1]] + theme(legend.position = "top"))


p_grid <- cowplot::plot_grid(plotlist = p_no_legend, ncol = 2)

p <- plot_grid( prow, ncol=1,rel_heights = c(1, .2) )

#create common x and y labels

 y.grob <- textGrob("Mean Intensity", 
                   gp=gpar(fontface="bold", col="black", fontsize=15), rot=90)

x.grob <- textGrob("Fish stocks", 
                   gp=gpar(fontface="bold", col="black", fontsize=15))
                     
                      
                      
#add to plot

grid.arrange(arrangeGrob(p, top=legend,left = y.grob, bottom = x.grob))

theme_set(
  theme_bw() +
    theme(legend.position = "top")
  )



p=list()
positions <- c("Tail","Lower region","Upper region","Head")
for (i in seq_day){
p[[i]]=ggplot(Means_data[[i]], aes(x=Fish_type, y=Parasite_means, fill=Body_regions)) + 
    geom_bar(aes(fill = factor(Body_regions, levels=positions)),position=position_dodge(), stat="identity")+
    labs(fill = "Body regions:",x="Fish stocks",y="Mean Intensity")+ theme(
  # Hide panel borders and remove grid lines
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.position = "none" , #hide legend
  # Change axis line
  axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()
  )+ ggtitle(paste("Day",i))+facet_wrap( ~ Para_species ) 
}

prow <- plot_grid( p[[3]] + theme(legend.position="none"),
           p[[7]] + theme(legend.position="none"),
           p[[11]] + theme(legend.position="none"),
           p[[15]] + theme(legend.position="none"),
           align = 'vh',
           hjust = -1,
           nrow = 2
           )

p_no_legend <- lapply(p, function(x) x + theme(legend.position = "none"))
legend <- cowplot::get_legend(p[[1]] + theme(legend.position = "top"))


p_grid <- cowplot::plot_grid(plotlist = p_no_legend, ncol = 2)

p <- plot_grid( prow, ncol=1,rel_heights = c(1, .2) )

#create common x and y labels

 y.grob <- textGrob("Mean Intensity", 
                   gp=gpar(fontface="bold", col="black", fontsize=15), rot=90)

x.grob <- textGrob("Fish stocks", 
                   gp=gpar(fontface="bold", col="black", fontsize=15))
                     
                      
                      
#add to plot

grid.arrange(arrangeGrob(p, top=legend,left = y.grob, bottom = x.grob))

theme_set(
  theme_bw() +
    theme(legend.position = "top")
  )



p=list()
positions <- c("Tail","Lower region","Upper region","Head")
for (i in seq_day){
p[[i]]=ggplot(Means_data[[i]], aes(x=Fish_type, y=Parasite_means, fill=Body_regions)) + 
    geom_bar(aes(fill = factor(Body_regions, levels=positions)),position=position_dodge(), stat="identity")+ labs(fill = "Body regions:",x="Fish stocks",y="Mean Intensity")+ theme(
  # Hide panel borders and remove grid lines
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.position = "none" , #hide legend
  # Change axis line
  axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()
  )+ ggtitle(paste("Day",i))+facet_wrap( ~ Para_species ) 
}

prow <- plot_grid( p[[1]] + theme(legend.position="none"),
           p[[3]] + theme(legend.position="none"),
           p[[5]] + theme(legend.position="none"),
           p[[7]] + theme(legend.position="none"),
           align = 'vh',
           hjust = -1,
           nrow = 2
           )

p_no_legend <- lapply(p, function(x) x + theme(legend.position = "none"))
legend <- cowplot::get_legend(p[[1]] + theme(legend.position = "top"))


p_grid <- cowplot::plot_grid(plotlist = p_no_legend, ncol = 2)

p <- plot_grid( prow, ncol=1,rel_heights = c(1, .2) )

#create common x and y labels

 y.grob <- textGrob("Mean Intensity", 
                   gp=gpar(fontface="bold", col="black", fontsize=15), rot=90)

x.grob <- textGrob("Fish stocks", 
                   gp=gpar(fontface="bold", col="black", fontsize=15))
                     
                      
                      
#add to plot

grid.arrange(arrangeGrob(p, top=legend,left = y.grob, bottom = x.grob))

theme_set(
  theme_bw() +
    theme(legend.position = "top")
  )



p=list()
positions <- c("Tail","Lower region","Upper region","Head")
for (i in seq_day){
p[[i]]=ggplot(Means_data[[i]], aes(x=Fish_type, y=Parasite_means, fill=Body_regions)) + 
    geom_bar(aes(fill = factor(Body_regions, levels=positions)),position=position_dodge(), stat="identity")+ labs(fill = "Body regions:",x="Fish stocks",y="Mean Intensity")+ theme(
  # Hide panel borders and remove grid lines
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.position = "none" , #hide legend
  # Change axis line
  axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()
  )+ ggtitle(paste("Day",i))+facet_wrap( ~ Para_species )+scale_fill_manual(values=c("grey88","grey60","grey30","grey0"))
}

prow <- plot_grid( p[[1]] + theme(legend.position="none"),
           p[[3]] + theme(legend.position="none"),
           p[[5]] + theme(legend.position="none"),
           p[[7]] + theme(legend.position="none"),
           align = 'vh',
           hjust = -1,
           nrow = 2
           )

p_no_legend <- lapply(p, function(x) x + theme(legend.position = "none"))
legend <- cowplot::get_legend(p[[1]] + theme(legend.position = "top"))


p_grid <- cowplot::plot_grid(plotlist = p_no_legend, ncol = 2)

p <- plot_grid( prow, ncol=1,rel_heights = c(1, .2) )

#create common x and y labels

 y.grob <- textGrob("Mean Intensity", 
                   gp=gpar(fontface="bold", col="black", fontsize=15), rot=90)

x.grob <- textGrob("Fish stocks", 
                   gp=gpar(fontface="bold", col="black", fontsize=15))
                     
                      
                      
#add to plot

grid.arrange(arrangeGrob(p, top=legend,left = y.grob, bottom = x.grob))

theme_set(
  theme_bw() +
    theme(legend.position = "top")
  )



p=list()
positions <- c("Tail","Lower region","Upper region","Head")
for (i in seq_day){
p[[i]]=ggplot(Means_data[[i]], aes(x=Fish_type, y=Parasite_means, fill=Body_regions)) + 
    geom_bar(aes(fill = factor(Body_regions, levels=positions)),position=position_dodge(), stat="identity")+ labs(fill = "Body regions:",x="Fish stocks",y="Mean Intensity")+ theme(
  # Hide panel borders and remove grid lines
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.position = "none" , #hide legend
  # Change axis line
  axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()
  )+ ggtitle(paste("Day",i))+facet_wrap( ~ Para_species ) 
}

prow <- plot_grid( p[[9]] + theme(legend.position="none"),
           p[[11]] + theme(legend.position="none"),
           p[[13]] + theme(legend.position="none"),
           p[[15]] + theme(legend.position="none"),
           align = 'vh',
           hjust = -1,
           nrow = 2
           )



p_no_legend <- lapply(p, function(x) x + theme(legend.position = "none"))
                    
                      
legend <- cowplot::get_legend(p[[1]] + theme(legend.position = "top"))


p_grid <- cowplot::plot_grid(plotlist = p_no_legend, ncol = 2)

p <- plot_grid( prow, ncol=1,rel_heights = c(1, .2) )

#create common x and y labels

 y.grob <- textGrob("Mean Intensity", 
                   gp=gpar(fontface="bold", col="black", fontsize=15), rot=90)

x.grob <- textGrob("Fish stocks", 
                   gp=gpar(fontface="bold", col="black", fontsize=15))
                     
                      
                      
#add to plot

grid.arrange(arrangeGrob(p,top=legend,left = y.grob, bottom = x.grob))


theme_set(
  theme_bw() +
    theme(legend.position = "top")
  )



p=list()
positions <- c("Tail","Lower region","Upper region","Head")
for (i in seq_day){
p[[i]]=ggplot(Means_data[[i]], aes(x=Fish_type, y=Parasite_means, fill=Body_regions)) + 
    geom_bar(aes(fill = factor(Body_regions, levels=positions)),position=position_dodge(), stat="identity")+ labs(fill = "Body regions:",x="Fish stocks",y="Mean Intensity")+ theme(
  # Hide panel borders and remove grid lines
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.position = "none" , #hide legend
  # Change axis line
  axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()
  )+ ggtitle(paste("Day",i))+facet_wrap( ~ Para_species )+scale_fill_manual(values=c("grey88","grey60","grey30","grey0"))
}

prow <- plot_grid( p[[9]] + theme(legend.position="none"),
           p[[11]] + theme(legend.position="none"),
           p[[13]] + theme(legend.position="none"),
           p[[15]] + theme(legend.position="none"),
           align = 'vh',
           hjust = -1,
           nrow = 2
           )



p_no_legend <- lapply(p, function(x) x + theme(legend.position = "none"))
                    
                      
legend <- cowplot::get_legend(p[[1]] + theme(legend.position = "top"))


p_grid <- cowplot::plot_grid(plotlist = p_no_legend, ncol = 2)

p <- plot_grid( prow, ncol=1,rel_heights = c(1, .2) )

#create common x and y labels

 y.grob <- textGrob("Mean Intensity", 
                   gp=gpar(fontface="bold", col="black", fontsize=15), rot=90)

x.grob <- textGrob("Fish stocks", 
                   gp=gpar(fontface="bold", col="black", fontsize=15))
                     
                      
                      
#add to plot

grid.arrange(arrangeGrob(p,top=legend,left = y.grob, bottom = x.grob))


theme_set(
  theme_bw() +
    theme(legend.position = "top")
  )


p=list()
positions <- c("Tail","Lower region","Upper region","Head")
for (i in seq_day){
p[[i]]=ggplot(Means_data[[i]], aes(x=Fish_type, y=Parasite_means, fill=Body_regions)) + 
    geom_bar(aes(fill = factor(Body_regions, levels=positions)),position=position_dodge(), stat="identity")+ labs(fill = "Body regions:",x="Fish stocks",y="Mean Intensity")+ theme(
  # Hide panel borders and remove grid lines
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.position = "none" , #hide legend
  # Change axis line
  axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()
  )+ ggtitle(paste("Day",i))+facet_wrap( ~ Para_species ) 
}

prow <- plot_grid( p[[17]] + theme(legend.position="none"),
           align = 'vh',
           hjust = -1,
           nrow = 1
           )

p_no_legend <- lapply(p, function(x) x + theme(legend.position = "none"))
legend <- cowplot::get_legend(p[[1]] + theme(legend.position = "top"))


p_grid <- cowplot::plot_grid(plotlist = p_no_legend, ncol = 1)

p <- plot_grid( prow, ncol=1,rel_heights = c(1, .2) )

#create common x and y labels

 y.grob <- textGrob("Mean Intensity", 
                   gp=gpar(fontface="bold", col="black", fontsize=15), rot=90)

x.grob <- textGrob("Fish stocks", 
                   gp=gpar(fontface="bold", col="black", fontsize=15))
                     
                      
                      
#add to plot

grid.arrange(arrangeGrob(p,top=legend, left = y.grob, bottom = x.grob))

theme_set(
  theme_bw() +
    theme(legend.position = "top")
  )


p=list()
positions <- c("Tail","Lower region","Upper region","Head")
for (i in seq_day){
p[[i]]=ggplot(Means_data[[i]], aes(x=Fish_type, y=Parasite_means, fill=Body_regions)) + 
    geom_bar(aes(fill = factor(Body_regions, levels=positions)),position=position_dodge(), stat="identity")+ labs(fill = "Body regions:",x="Fish stocks",y="Mean Intensity")+ theme(
  # Hide panel borders and remove grid lines
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  legend.position = "none" , #hide legend
  # Change axis line
  axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()
  )+ ggtitle(paste("Day",i))+facet_wrap( ~ Para_species ) +scale_fill_manual(values=c("grey88","grey60","grey30","grey0"))
}

prow <- plot_grid( p[[17]] + theme(legend.position="none"),
           align = 'vh',
           hjust = -1,
           nrow = 1
           )

p_no_legend <- lapply(p, function(x) x + theme(legend.position = "none"))
legend <- cowplot::get_legend(p[[1]] + theme(legend.position = "top"))


p_grid <- cowplot::plot_grid(plotlist = p_no_legend, ncol = 1)

p <- plot_grid( prow, ncol=1,rel_heights = c(1, .2) )

#create common x and y labels

 y.grob <- textGrob("Mean Intensity", 
                   gp=gpar(fontface="bold", col="black", fontsize=15), rot=90)

x.grob <- textGrob("Fish stocks", 
                   gp=gpar(fontface="bold", col="black", fontsize=15))
                     
                      
                      
#add to plot

grid.arrange(arrangeGrob(p,top=legend, left = y.grob, bottom = x.grob))

#Gt3
Gt3_data=split(Means_days,Means_days$Para_species)$"Gt3"
Gt3_OS=split(Gt3_data,Gt3_data$Fish_type)$"OS"
Gt3_LA=split(Gt3_data,Gt3_data$Fish_type)$"LA"
Gt3_UA=split(Gt3_data,Gt3_data$Fish_type)$"UA"
#Gt
Gt_data=split(Means_days,Means_days$Para_species)$"Gt"
Gt_OS=split(Gt_data,Gt_data$Fish_type)$"OS"
Gt_LA=split(Gt_data,Gt_data$Fish_type)$"LA"
Gt_UA=split(Gt_data,Gt_data$Fish_type)$"UA"
#Gb
#Gt
Gb_data=split(Means_days,Means_days$Para_species)$"Gb"
Gb_OS=split(Gb_data,Gb_data$Fish_type)$"OS"
Gb_LA=split(Gb_data,Gb_data$Fish_type)$"LA"
Gb_UA=split(Gb_data,Gb_data$Fish_type)$"UA"



print(paste("Peak time for Gt3-Orn=",Gt3_OS$Day[which.max(Gt3_OS$Parasite_means)]))
print(paste("Peak time for Gt3-LA=",Gt3_LA$Day[which.max(Gt3_LA$Parasite_means)]))
print(paste("Peak time for Gt3-UA=",Gt3_UA$Day[which.max(Gt3_UA$Parasite_means)]))

print(paste("Peak time for Gt-Orn=",Gt_OS$Day[which.max(Gt_OS$Parasite_means)]))
print(paste("Peak time for Gt-LA=",Gt_LA$Day[which.max(Gt_LA$Parasite_means)]))
print(paste("Peak time for Gt-UA=",Gt_UA$Day[which.max(Gt_UA$Parasite_means)]))

print(paste("Peak time for Gb-Orn=",Gb_OS$Day[which.max(Gb_OS$Parasite_means)]))
print(paste("Peak time for Gb-LA=",Gb_LA$Day[which.max(Gb_LA$Parasite_means)]))
print(paste("Peak time for Gb-UA=",Gb_UA$Day[which.max(Gb_UA$Parasite_means)]))

Gt3_OS_Tail=split(Gt3_OS,Gt3_OS$Body_regions)$"Tail"
Gt3_OS_LR=split(Gt3_OS,Gt3_OS$Body_regions)$"Lower region"
Gt3_OS_UR=split(Gt3_OS,Gt3_OS$Body_regions)$"Upper region"
Gt3_OS_Head=split(Gt3_OS,Gt3_OS$Body_regions)$"Head"

print(paste("Peak time for Gt3-Orn at Tail=",Gt3_OS_Tail$Day[which.max(Gt3_OS_Tail$Parasite_means)]))
print(paste("Peak time for Gt3-Orn at Lower region=",Gt3_OS_LR$Day[which.max(Gt3_OS_LR$Parasite_means)]))
print(paste("Peak time for Gt3-Orn at Upper region=",Gt3_OS_UR$Day[which.max(Gt3_OS_UR$Parasite_means)]))
print(paste("Peak time for Gt3-Orn at Head=",Gt3_OS_Head$Day[which.max(Gt3_OS_Head$Parasite_means)]))

Gt3_LA_Tail=split(Gt3_LA,Gt3_LA$Body_regions)$"Tail"
Gt3_LA_LR=split(Gt3_LA,Gt3_LA$Body_regions)$"Lower region"
Gt3_LA_UR=split(Gt3_LA,Gt3_LA$Body_regions)$"Upper region"
Gt3_LA_Head=split(Gt3_LA,Gt3_LA$Body_regions)$"Head"

print(paste("Peak time for Gt3-LA at Tail=",Gt3_LA_Tail$Day[which.max(Gt3_LA_Tail$Parasite_means)]))
print(paste("Peak time for Gt3-LA at Lower region=",Gt3_LA_LR$Day[which.max(Gt3_LA_LR$Parasite_means)]))
print(paste("Peak time for Gt3-LA at Upper region=",Gt3_LA_UR$Day[which.max(Gt3_LA_UR$Parasite_means)]))
print(paste("Peak time for Gt3-LA at Head=",Gt3_LA_Head$Day[which.max(Gt3_LA_Head$Parasite_means)]))



Gt3_UA_Tail=split(Gt3_UA,Gt3_UA$Body_regions)$"Tail"
Gt3_UA_LR=split(Gt3_UA,Gt3_UA$Body_regions)$"Lower region"
Gt3_UA_UR=split(Gt3_UA,Gt3_UA$Body_regions)$"Upper region"
Gt3_UA_Head=split(Gt3_UA,Gt3_UA$Body_regions)$"Head"

print(paste("Peak time for Gt3-UA at Tail=",Gt3_UA_Tail$Day[which.max(Gt3_UA_Tail$Parasite_means)]))
print(paste("Peak time for Gt3-UA at Lower region=",Gt3_UA_LR$Day[which.max(Gt3_UA_LR$Parasite_means)]))
print(paste("Peak time for Gt3-UA at Upper region=",Gt3_UA_UR$Day[which.max(Gt3_UA_UR$Parasite_means)]))
print(paste("Peak time for Gt3-UA at Head=",Gt3_UA_Head$Day[which.max(Gt3_UA_Head$Parasite_means)]))

Gt_OS_Tail=split(Gt_OS,Gt_OS$Body_regions)$"Tail"
Gt_OS_LR=split(Gt_OS,Gt_OS$Body_regions)$"Lower region"
Gt_OS_UR=split(Gt_OS,Gt_OS$Body_regions)$"Upper region"
Gt_OS_Head=split(Gt_OS,Gt_OS$Body_regions)$"Head"

print(paste("Peak time for Gt-Orn at Tail=",Gt_OS_Tail$Day[which.max(Gt_OS_Tail$Parasite_means)]))
print(paste("Peak time for Gt-Orn at Lower region=",Gt_OS_LR$Day[which.max(Gt_OS_LR$Parasite_means)]))
print(paste("Peak time for Gt-Orn at Upper region=",Gt_OS_UR$Day[which.max(Gt_OS_UR$Parasite_means)]))
print(paste("Peak time for Gt-Orn at Head=",Gt_OS_Head$Day[which.max(Gt_OS_Head$Parasite_means)]))

Gt_LA_Tail=split(Gt_LA,Gt_LA$Body_regions)$"Tail"
Gt_LA_LR=split(Gt_LA,Gt_LA$Body_regions)$"Lower region"
Gt_LA_UR=split(Gt_LA,Gt_LA$Body_regions)$"Upper region"
Gt_LA_Head=split(Gt_LA,Gt_LA$Body_regions)$"Head"

print(paste("Peak time for Gt-LA at Tail=",Gt_LA_Tail$Day[which.max(Gt_LA_Tail$Parasite_means)]))
print(paste("Peak time for Gt-LA at Lower region=",Gt_LA_LR$Day[which.max(Gt_LA_LR$Parasite_means)]))
print(paste("Peak time for Gt-LA at Upper region=",Gt_LA_UR$Day[which.max(Gt_LA_UR$Parasite_means)]))
print(paste("Peak time for Gt-LA at Head=",Gt_LA_Head$Day[which.max(Gt_LA_Head$Parasite_means)]))



Gt_UA_Tail=split(Gt_UA,Gt_UA$Body_regions)$"Tail"
Gt_UA_LR=split(Gt_UA,Gt_UA$Body_regions)$"Lower region"
Gt_UA_UR=split(Gt_UA,Gt_UA$Body_regions)$"Upper region"
Gt_UA_Head=split(Gt_UA,Gt_UA$Body_regions)$"Head"

print(paste("Peak time for Gt-UA at Tail=",Gt_UA_Tail$Day[which.max(Gt_UA_Tail$Parasite_means)]))
print(paste("Peak time for Gt-UA at Lower region=",Gt_UA_LR$Day[which.max(Gt_UA_LR$Parasite_means)]))
print(paste("Peak time for Gt-UA at Upper region=",Gt_UA_UR$Day[which.max(Gt_UA_UR$Parasite_means)]))
print(paste("Peak time for Gt-UA at Head=",Gt_UA_Head$Day[which.max(Gt_UA_Head$Parasite_means)]))

Gb_OS_Tail=split(Gb_OS,Gb_OS$Body_regions)$"Tail"
Gb_OS_LR=split(Gb_OS,Gb_OS$Body_regions)$"Lower region"
Gb_OS_UR=split(Gb_OS,Gb_OS$Body_regions)$"Upper region"
Gb_OS_Head=split(Gt_OS,Gb_OS$Body_regions)$"Head"

print(paste("Peak time for Gb-Orn at Tail=",Gb_OS_Tail$Day[which.max(Gb_OS_Tail$Parasite_means)]))
print(paste("Peak time for Gb-Orn at Lower region=",Gb_OS_LR$Day[which.max(Gb_OS_LR$Parasite_means)]))
print(paste("Peak time for Gb-Orn at Upper region=",Gb_OS_UR$Day[which.max(Gb_OS_UR$Parasite_means)]))
print(paste("Peak time for Gb-Orn at Head=",Gb_OS_Head$Day[which.max(Gb_OS_Head$Parasite_means)]))

Gb_LA_Tail=split(Gb_LA,Gb_LA$Body_regions)$"Tail"
Gb_LA_LR=split(Gb_LA,Gb_LA$Body_regions)$"Lower region"
Gb_LA_UR=split(Gb_LA,Gb_LA$Body_regions)$"Upper region"
Gb_LA_Head=split(Gb_LA,Gb_LA$Body_regions)$"Head"

print(paste("Peak time for Gb-LA at Tail=",Gb_LA_Tail$Day[which.max(Gb_LA_Tail$Parasite_means)]))
print(paste("Peak time for Gb-LA at Lower region=",Gb_LA_LR$Day[which.max(Gb_LA_LR$Parasite_means)]))
print(paste("Peak time for Gb-LA at Upper region=",Gb_LA_UR$Day[which.max(Gb_LA_UR$Parasite_means)]))
print(paste("Peak time for Gb-LA at Head=",Gb_LA_Head$Day[which.max(Gb_LA_Head$Parasite_means)]))



Gb_UA_Tail=split(Gb_UA,Gb_UA$Body_regions)$"Tail"
Gb_UA_LR=split(Gb_UA,Gb_UA$Body_regions)$"Lower region"
Gb_UA_UR=split(Gb_UA,Gb_UA$Body_regions)$"Upper region"
Gb_UA_Head=split(Gb_UA,Gb_UA$Body_regions)$"Head"

print(paste("Peak time for Gb-UA at Tail=",Gb_UA_Tail$Day[which.max(Gb_UA_Tail$Parasite_means)]))
print(paste("Peak time for Gb-UA at Lower region=",Gb_UA_LR$Day[which.max(Gb_UA_LR$Parasite_means)]))
print(paste("Peak time for Gb-UA at Upper region=",Gb_UA_UR$Day[which.max(Gb_UA_UR$Parasite_means)]))
print(paste("Peak time for Gb-UA at Head=",Gb_UA_Head$Day[which.max(Gb_UA_Head$Parasite_means)]))

for (i in seq_day){
MultiData[[i]]$Fish_type=MultiData[[i]]$Group
levels(MultiData[[i]]$Fish_type)=c("Orn","LA","UA","Orn","LA","UA","Orn","LA","UA")

MultiData[[i]]$Parasite_strain=MultiData[[i]]$Group
levels(MultiData[[i]]$Parasite_strain)=c("Gt3","Gt3","Gt3","Gt","Gt","Gt","Gbull","Gbull","Gbull")
    }

day=seq(1,17,by=2)
MKW_results=list()
 for (i in day)
     {
MKW_results[[i]]=multkw(group=MultiData[[i]][,"Group"],y=MultiData[[i]][,11:14],simplify=F)
     print(MKW_results[[i]])
     }

#Day 1
day=1
kruskal.test(Tail_region~ Group, data = MultiData[[day]])
kruskal.test(Lower_region~ Group, data = MultiData[[day]])
kruskal.test(Upper_region~Group, data = MultiData[[day]])
kruskal.test(Head_region~ Group, data = MultiData[[day]])

#Day 3
day=3
kruskal.test(Tail_region~Group, data = MultiData[[day]])
kruskal.test(Lower_region~ Group, data = MultiData[[day]])
kruskal.test(Upper_region~Group, data = MultiData[[day]])
kruskal.test(Head_region~ Group, data = MultiData[[day]])

#Day 5
day=5
kruskal.test(Tail_region~ Group, data = MultiData[[day]])
kruskal.test(Lower_region~Group, data = MultiData[[day]])
kruskal.test(Upper_region~ Group, data = MultiData[[day]])
kruskal.test(Head_region~ Group, data = MultiData[[day]])

#Day 7
day=7
kruskal.test(Tail_region~ Group, data = MultiData[[day]])
kruskal.test(Lower_region~ Group, data = MultiData[[day]])
kruskal.test(Upper_region~Group, data = MultiData[[day]])
kruskal.test(Head_region~ Group, data = MultiData[[day]])

#Day 9
day=9
kruskal.test(Tail_region~ Group, data = MultiData[[day]])
kruskal.test(Lower_region~ Group, data = MultiData[[day]])
kruskal.test(Upper_region~ Group, data = MultiData[[day]])
kruskal.test(Head_region~ Group, data = MultiData[[day]])

#Day 11
day=11
kruskal.test(Tail_region~ Group, data = MultiData[[day]])
kruskal.test(Lower_region~ Group, data = MultiData[[day]])
kruskal.test(Upper_region~ Group, data = MultiData[[day]])
kruskal.test(Head_region~ Group, data = MultiData[[day]])

#Day 13
day=13
kruskal.test(Tail_region~ Group, data = MultiData[[day]])
kruskal.test(Lower_region~ Group, data = MultiData[[day]])
kruskal.test(Upper_region~ Group, data = MultiData[[day]])
kruskal.test(Head_region~Group, data = MultiData[[day]])

#Day 15
day=15
kruskal.test(Tail_region~ Group, data = MultiData[[day]])
kruskal.test(Lower_region~ Group, data = MultiData[[day]])
kruskal.test(Upper_region~ Group, data = MultiData[[day]])
kruskal.test(Head_region~ Group, data = MultiData[[day]])

#Day 17
day=17
kruskal.test(Tail_region~ Group, data = MultiData[[day]])
kruskal.test(Lower_region~ Group, data = MultiData[[day]])
kruskal.test(Upper_region~ Group, data = MultiData[[day]])
kruskal.test(Head_region~ Group, data = MultiData[[day]])

library(PMCMRplus)

day=1
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Tail_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Lower_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Upper_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Head_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")


day=3
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Tail_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Lower_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Upper_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Head_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")


day=5
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Tail_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Lower_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Upper_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Head_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")


day=7
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Tail_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Lower_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Upper_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Head_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")


day=9
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Tail_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Lower_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Upper_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Head_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")


day=11
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Tail_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Lower_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Upper_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Head_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")


day=13
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Tail_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Lower_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Upper_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Head_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")


day=15
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Tail_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Lower_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Upper_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Head_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")


day=17
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Tail_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Lower_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Upper_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")
posthoc.kruskal.dunn.test(x=MultiData[[day]]$Head_region, g=MultiData[[day]]$Group, p.adjust.method="bonferroni")

