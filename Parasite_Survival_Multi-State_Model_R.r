options(repr.plot.width=8, repr.plot.height=8,repr.plot.res = 300) #Setting plot size

library("msm")
library("msmtools") #integrated with msm package for further analysis
library("survival")
library("survivalAnalysis") #Multivariate Surival
library("rms")
#library("sjPlot")#Interaction plots
library(sjmisc)
library(ggplot2)
library(MuMIn) # use to calculate the marginal and conditional R-Sq
library(lme4) #for glmm
library("lmerTest")
#library(effects)#check for effect size
#theme_set(theme_sjplot())

library("lmtest") # for likelihood ratio test
#For Multivariate survival analysis
library(tidyverse)
library(tidytidbits)
library("survivalAnalysis") #Multivariate Surival
library("condSURV") #Conditional Survival Analysis
library(KernSmooth)
library(survminer)#Plot survival curves
library(plyr)#Transforming data
library(dplyr)
library(officer)
library(Rcpp)
library(rvg)

library(psycho)
library(dplyr)
library("tidyverse")
library("lmerTest")
library(ggplot2)
#library(modelbased)
#library(see)
library(grid)

#Setting working directory
setwd("C:/Users/user/Desktop/DataAnalysis_results_R/Multi-state Markov Modelling")

#Importing data for survival analysis
#Fish death status:  0=alive (right centered), 1=dead (Uncensored)
#Parasite extinction:  0=Not extinct (right centered), 1=Extinct (Uncensored)
#Multistates: State1=fish alive but remained infected, State2=fish alive without infection, State3=fish dead

Survival_data<-read.csv(file="MultiSate_Model_Data.csv")

Survival_data$Sex_fish<-factor(Survival_data$Sex_fish,levels=c(1,2),labels=c("Female fish","Male fish"))
Survival_data$Fish_population<-factor(Survival_data$Fish_population,levels=c(1,2,3),labels=c("Upper Aripo","Lower Aripo","Ornamental"))
Survival_data$Parasite_type<-factor(Survival_data$Parasite_type,levels=c(1,2,3),labels=c("Gt3","Gt","Gb"))
Survival_data$States_cat<-factor(Survival_data$Multistates,levels=c(1,2,3),labels=c("State 1","State 2","State 3"))

Survival_data$Size_Category[Survival_data$Fish_size<=17.5]="Cat_small"
Survival_data$Size_Category[Survival_data$Fish_size>17.5]="Large"


Survival_data$Categorized_size[Survival_data$Fish_size<=14.5]="Small"
Survival_data$Categorized_size[Survival_data$Fish_size>14.5&Survival_data$Fish_size<=18]="Medium"
Survival_data$Categorized_size[Survival_data$Fish_size>18]="Large"
#attach(Survival_data)

#View fish size and number of fish
Survival_data2<-read.csv(file="Survival_data_PhD_Updated.csv")
table(Survival_data2$Fish_size)

#Categorized_size=
table(Survival_data2$Fish_size<=14.5)[2]

table(Survival_data2$Fish_size>14.5&Survival_data2$Fish_size<18)[2]

table(Survival_data2$Fish_size>=18)[2]

#Changing reference category for fish size category 1
Survival_data$Size_Category=factor(Survival_data$Size_Category ,labels=c("Cat_small","Large"), ordered = FALSE )
Survival_data$Size_Category <- relevel(Survival_data$Size_Category, ref = "Cat_small")


#Changing reference category for fish size category 2
Survival_data$Categorized_size=factor(Survival_data$Categorized_size ,labels=c("Large","Medium","Small"), ordered = FALSE )
Survival_data$Categorized_size <- relevel(Survival_data$Categorized_size, ref = "Small")



#Importing data
Gt3_plot=read.csv(file="GT3_Plot.csv")
Gt_plot=read.csv(file="LA-Turn_Plot.csv")
Gbull_plot=read.csv(file="LA-Bull_plot.csv")
#Combining the three data set
Combined_data<-rbind(Gt3_plot,Gt_plot,Gbull_plot)

Transformed_data<-ddply(Combined_data, c("Day", "Parasite","ID"), summarise,
                    N    = length(Combined_data))


Splitdata<-split(Transformed_data,Transformed_data$ID)
#Saving data Separate for each fish in csv file
lapply(names(Splitdata), function(x){
    write_csv(Splitdata[[x]], path = paste(x, ".csv", sep = ""))
    })

#Calling all splitted data corresponding to each fish
Data_Observed=NULL
for (i in 1:157){
Data_Observed[[i]]<-Splitdata[[i]][,-4] 
    }
#Data_Observed[[1]]

#Adding Parasite load to the data
Parasite_load_fish=list()
Fish_identity=NULL

for (i in 1:157){
    Parasite_load_fish[[i]]<-tapply(Data_Observed[[i]]$Parasite,Data_Observed[[i]]$Day,max)
    Fish_identity[[i]]<-names(Splitdata)[i]
}

Fish_ID=list()
Max_load=list()
Time_day=list()

for (i in 1:157){
    Fish_ID[[i]]=t(as.vector(as.numeric(rep(Fish_identity[[i]],9))))
    Max_load[[i]]=t(as.vector(Parasite_load_fish[[i]]))
    Time_day[[i]]=t(as.vector(seq(1,17,by=2)))
    }

ID_data=t(as.data.frame(do.call(cbind,Fish_ID)))
Load_data=t(as.data.frame(do.call(cbind,Max_load)))
Time_data=t(as.data.frame(do.call(cbind,Time_day)))
ID_Load_time_data=as.data.frame(cbind(ID_data,Time_data,Load_data))
names(ID_Load_time_data)=c("ID","Time","Parasite_load")
rownames(ID_Load_time_data)=NULL #Eliminating row names

Parasite_load=ID_Load_time_data[,3]

 #Required data
Multistate_data=cbind(Survival_data[order(Survival_data$ID),],Parasite_load)
head(Multistate_data,n=9)
attach(Multistate_data)
#Saving Multistate_data
#write.csv(Multistate_data,"Multistate_data_updated.csv")

#Adding parasite extinction status to the data
Multistate_data$Parasite_extinction<- rep(NA,dim(Multistate_data)[1])

unique_ids<- sort(Multistate_data$ID)
for(i in seq_along(unique_ids)){
    data_id<- Multistate_data[Multistate_data$ID==unique_ids[i], ]
    for(t_index in 1:9){
      if(data_id[t_index, ]$Parasite_load==0){ #when there is parasite extinction
         Multistate_data[Multistate_data$ID==unique_ids[i], ][t_index, ]$Parasite_extinction<- "Extinct"  
      }else if(data_id[t_index, ]$Parasite_load>0){#not extinction
         Multistate_data[Multistate_data$ID==unique_ids[i], ][t_index, ]$Parasite_extinction<- "Not_extinct"  
      }  
        
    }
  
}

#Data on Parasite extinction has been added
head(Multistate_data,n=9)

#very close to the mean sojourn-time from the time-inhomogeneous Markov model
#mean(Multistate_data[,c(1,6,7)][Multistate_data$Multistates==1,][[3]]) 

#Percentage of infecteds, recovered and dead fish at day 17
Day_17<-split(Multistate_data,Time)$"17"
table(Day_17$Multistates)/157

 Multistate_data_alive<- Multistate_data[Multistate_data$Multistates!=3, ]#fish alive

Day_parasite_fish<- NULL
for (i in seq(1,17,by=2)) {
    Day_parasite_fish[[i]]<- table(Multistate_data_alive[Multistate_data_alive$Time==i, ]$Fish_population,
    Multistate_data_alive[Multistate_data_alive$Time==i, ]$Parasite_type,
      Multistate_data_alive[Multistate_data_alive$Time==i, ]$Parasite_extinction)
}


day<-1
paste("Summary of Fish alive on day:","",day)

Day_parasite_fish[[day]]
print(paste("Sample size (during no parasite extinction) at time=",day,"","is","",sum( Day_parasite_fish[[day]])))

day<-3
paste("Summary of Fish alive on day:","",day)

Day_parasite_fish[[day]]
print(paste("sample size at time=",day,"","is","",sum( Day_parasite_fish[[day]])))
print(paste("Sample size (during parasite extinction) at time=",day,"","is","",sum( Day_parasite_fish[[day]][,,1])))
print(paste("Sample size (during no parasite extinction) at time=",day,"","is","",sum( Day_parasite_fish[[day]][,,2])))

day<-5
paste("Summary of Fish alive on day:","",day)

Day_parasite_fish[[day]]
print(paste("sample size at time=",day,"","is","",sum( Day_parasite_fish[[day]])))
print(paste("Sample size (during parasite extinction) at time=",day,"","is","",sum( Day_parasite_fish[[day]][,,1])))
print(paste("Sample size (during no parasite extinction) at time=",day,"","is","",sum( Day_parasite_fish[[day]][,,2])))

day<-7
paste("Summary of Fish alive on day:","",day)

Day_parasite_fish[[day]]
print(paste("sample size at time=",day,"","is","",sum( Day_parasite_fish[[day]])))
print(paste("Sample size (during parasite extinction) at time=",day,"","is","",sum( Day_parasite_fish[[day]][,,1])))
print(paste("Sample size (during no parasite extinction) at time=",day,"","is","",sum( Day_parasite_fish[[day]][,,2])))

day<-9
paste("Summary of Fish alive on day:","",day)

Day_parasite_fish[[day]]
print(paste("sample size at time=",day,"","is","",sum( Day_parasite_fish[[day]])))
print(paste("Sample size (during parasite extinction) at time=",day,"","is","",sum( Day_parasite_fish[[day]][,,1])))
print(paste("Sample size (during no parasite extinction) at time=",day,"","is","",sum( Day_parasite_fish[[day]][,,2])))

day<-11
paste("Summary of Fish alive on day:","",day)

Day_parasite_fish[[day]]
print(paste("sample size at time=",day,"","is","",sum( Day_parasite_fish[[day]])))
print(paste("Sample size (during parasite extinction) at time=",day,"","is","",sum( Day_parasite_fish[[day]][,,1])))
print(paste("Sample size (during no parasite extinction) at time=",day,"","is","",sum( Day_parasite_fish[[day]][,,2])))

day<-13
paste("Summary of Fish alive on day:","",day)

Day_parasite_fish[[day]]
print(paste("sample size at time=",day,"","is","",sum( Day_parasite_fish[[day]])))
print(paste("Sample size (during parasite extinction) at time=",day,"","is","",sum( Day_parasite_fish[[day]][,,1])))
print(paste("Sample size (during no parasite extinction) at time=",day,"","is","",sum( Day_parasite_fish[[day]][,,2])))

day<-15
paste("Summary of Fish alive on day:","",day)

Day_parasite_fish[[day]]
print(paste("sample size at time=",day,"","is","",sum( Day_parasite_fish[[day]])))
print(paste("Sample size (during parasite extinction) at time=",day,"","is","",sum( Day_parasite_fish[[day]][,,1])))
print(paste("Sample size (during no parasite extinction) at time=",day,"","is","",sum( Day_parasite_fish[[day]][,,2])))

day<-17
paste("Summary of Fish alive on day:","",day)

Day_parasite_fish[[day]]
print(paste("sample size at time=",day,"","is","",sum( Day_parasite_fish[[day]])))
print(paste("Sample size (during parasite extinction) at time=",day,"","is","",sum( Day_parasite_fish[[day]][,,1])))
print(paste("Sample size (during no parasite extinction) at time=",day,"","is","",sum( Day_parasite_fish[[day]][,,2])))



#State 1: Fish alive and no parasite extinction
#State 2: Fish alive and parasite extinct 
#State 3: Fish dead and no parasite extinction
#statetable.msm(Multistates,ID, data=Survival_data)
statetable.msm(Multistates,ID, data=Multistate_data)

Infected_class <-split(Multistate_data,Multistates)$"1"
Total_number_day_spents_State1<-sum(as.vector(tapply(Infected_class$Time,Infected_class$ID,max)))
print(paste("Total days spent in infected class=",Total_number_day_spents_State1))

#Initial guess of transition rates
Q<-rbind(c(0,1,1),c(0,0,0),c(0,0,0))
rownames(Q) <- colnames(Q) <- c("state1", "state2", "state3")
Q

#Q.crude <- crudeinits.msm(Multistates ~ Time,ID,data=Survival_data, qmatrix=Q)
Q.crude <- crudeinits.msm(Multistates ~ Time,ID,data=Multistate_data, qmatrix=Q)
Q.crude

#setting Gt as reference category
Multistate_data$Parasite_type=relevel(Multistate_data$Parasite_type,ref="Gt")

MSM_pvalues<-function(msm_model,covariate_num){
Covariates_levels<-msm_model$qcmodel$covlabels
log_estimates<-log(unlist(as.matrix(hazard.msm(msm_model)[toString(Covariates_levels[covariate_num])])))

#from state 1 to 2
std_err_1_2<- (log_estimates[5]-log_estimates[3])/(2*1.96)
z_1_2<-abs(log_estimates[1]/std_err_1_2)
pvalue_1_2<- exp((−0.717*z_1_2)-(0.416*z_1_2^2))

#from state 1 to 3
std_err_1_3<- (log_estimates[6]-log_estimates[4])/(2*1.96)
z_1_3<-abs(log_estimates[2]/std_err_1_3)
pvalue_1_3<- exp((−0.717*z_1_3)-(0.416*z_1_3^2))
    
HR_estimates=as.data.frame(hazard.msm(msm_model)[toString(Covariates_levels[covariate_num])])
HR_estimates$P_value=c(pvalue_1_2,pvalue_1_3)
colnames(HR_estimates)=c("HR","L","U","P-value")     
HR_estimates$Variable=c(toString(Covariates_levels[covariate_num])) 
        
    return(HR_estimates)
        
}



#Given State 1-2 & State 1-3 covariates: 
#For each 15 unique combinations, there are 15*15=225 ways of pairing them up including itself
#For empty models: either 0 covariate for State 1-2 & 15 covariates for State 1-3, 
#  either 15 covariates for State 1-3 & 0 covariate for State 1-3 and  0 covariates for State 1-3 & 0 covariate for State 1-3
#which gives (15*15)+15+15+1=256 or 4^4 total combination of covariates including the empty cases.

#breakdown: 4 singles, 6 pairs, 4  tripples, 1  quadruple

# make a list of independent variables
predictors<- c("Sex_fish","Fish_size","Parasite_type","Fish_population")

n=seq_along(predictors)

## Create list of models
list.of.models <- lapply(seq_along((predictors)), function(n) {
    predictors_combinations <- apply(X = combn(predictors, n), MARGIN = 2, paste,collapse = "and")
})

vector.of.models =unlist(list.of.models)
vector.of.models=str_split(vector.of.models,"and")
vector.of.models

#Variables
single_preds=list(vector.of.models[[1]],vector.of.models[[2]],vector.of.models[[3]],vector.of.models[[4]])
double_preds=list(vector.of.models[[5]],vector.of.models[[6]],vector.of.models[[7]],vector.of.models[[8]]
              ,vector.of.models[[9]],vector.of.models[[10]])
tripple_preds=list(vector.of.models[[11]],vector.of.models[[12]],vector.of.models[[13]],vector.of.models[[14]])
quadruple_preds=list(vector.of.models[[15]] ) 

Total_models_breakdown=c(
4*4, # 1 on 1
4*6,  # 1 on 2
4* 4, #1 on 3
4*1,  #1 on 4


6*4,  #2 on 1
6*6,  #2 on 2
6*4,  #2 on 3
6*1,  #2 on 4

 4*4, #3 on 1
4*6,   #3 on 2
 4*4,        #3 on 3           
 4*1,        #3 on 4

1*4,        #4 on 1
1*6,        #4 on 2
 1*4,       #4 on 3
 1*1       # 4 on 4
 )                  
Total_models_breakdown
print(paste("Total models to fit with at least a predictor in both transitions=",sum(Total_models_breakdown)) ) 

print(paste("Overall total models to fit including empty models in both transitions=",sum(Total_models_breakdown)+31) )               

# Function for model selection (computing both AIC and BIC values)
AIC_BIC_estimator<- function(fitted_model){
    n<- dim(fitted_model$data$mf)[1]#sample size
    k<- fitted_model$paramdata$nopt#number of parameters
    minus2LogLik<- fitted_model$minus2loglik
    AIC_value<- 2*k +minus2LogLik
    BIC_value<- k*log(n)+minus2LogLik
    return(list(AIC_value=AIC_value,BIC_value=BIC_value))
}


#Case1 - 16 models: Fitted msm of single preds (for state 1-2) vs single preds (for state 1-3)
model_variables1=c()
for (i in 1:length(single_preds)){
       model_variables1[[i]]=as.data.frame(matrix(NA,nrow=length(single_preds),ncol=4))
      names(model_variables1[[i]])=c("Preds_1-2","Preds_1-3","AIC","BIC")
         predictor1=single_preds[[i]]
        for(j in 1:length(single_preds)){
            
         predictor2=single_preds[[j]] 
            
msm_model1=msm( Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~get(predictor1),"1-3"=~get(predictor2)),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))        
         model_variables1[[i]][j,1:2]=c(single_preds[[i]],single_preds[[j]])
         model_variables1[[i]][j,3]=AIC_BIC_estimator(msm_model1)$AIC_value#Computing the AIC
         model_variables1[[i]][j,4]=AIC_BIC_estimator(msm_model1)$BIC_value#Computing the BIC
           }
                  }

model_single_vs_single=do.call("rbind",model_variables1)

model_single_vs_single
write.csv(model_single_vs_single,"model_single_vs_single.csv")

#Case2 - 24 models: Fitted msm of single preds (for state 1-2) vs double preds (for state 1-3)
model_variables2=c()
for (i in 1:length(single_preds)){
       model_variables2[[i]]=as.data.frame(matrix(NA,nrow=length(double_preds),ncol=4))
      names(model_variables2[[i]])=c("Preds_1-2","Preds_1-3","AIC","BIC")
         predictor1=single_preds[[i]]
        for(j in 1:length(double_preds)){
            
         predictor2=double_preds[[j]] 
            
msm_model2=msm(Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~get(predictor1),
                 "1-3"=~get(predictor2[1])+get(predictor2[2])),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))         
    model_variables2[[i]][j,1:2]=c(paste(single_preds[[i]]),paste(double_preds[[j]][1],double_preds[[j]][2],sep ='+'))
    model_variables2[[i]][j,3]=AIC_BIC_estimator(msm_model2)$AIC_value#Computing the AIC
    model_variables2[[i]][j,4]=AIC_BIC_estimator(msm_model2)$BIC_value#Computing the BIC
           }
                  }

model_single_vs_double=do.call("rbind",model_variables2)
model_single_vs_double
write.csv(model_single_vs_double,"model_single_vs_double.csv")

#Case3 - 16 models: Fitted msm of single preds (for state 1-2) vs tripple preds (for state 1-3)
model_variables3=c()
for (i in 1:length(single_preds)){
       model_variables3[[i]]=as.data.frame(matrix(NA,nrow=length(tripple_preds),ncol=4))
      names(model_variables3[[i]])=c("Preds_1-2","Preds_1-3","AIC","BIC")
         predictor1=single_preds[[i]]
        for(j in 1:length(tripple_preds)){
            
         predictor2=tripple_preds[[j]] 
            
msm_model3=msm(Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~get(predictor1),
                 "1-3"=~get(predictor2[1])+get(predictor2[2])+get(predictor2[3])),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))         
    model_variables3[[i]][j,1:2]=c(paste(single_preds[[i]]),paste(tripple_preds[[j]][1],tripple_preds[[j]][2]
                                            ,tripple_preds[[j]][3],sep ='+'))
    model_variables3[[i]][j,3]=AIC_BIC_estimator(msm_model3)$AIC_value#Computing the AIC
    model_variables3[[i]][j,4]=AIC_BIC_estimator(msm_model3)$BIC_value#Computing the BIC
           }
                  }

model_single_vs_tripple=do.call("rbind",model_variables3)
model_single_vs_tripple
write.csv(model_single_vs_tripple,"model_single_vs_tripple.csv")

#Case4 - 16 models: Fitted msm of single preds (for state 1-2) vs quadruple preds (for state 1-3)
model_variables4=c()
for (i in 1:length(single_preds)){
       model_variables4[[i]]=as.data.frame(matrix(NA,nrow=length(quadruple_preds),ncol=4))
      names(model_variables4[[i]])=c("Preds_1-2","Preds_1-3","AIC","BIC")
         predictor1=single_preds[[i]]
        for(j in 1:length(quadruple_preds)){
            
         predictor2=quadruple_preds[[j]] 
            
msm_model4=msm(Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~get(predictor1),
                 "1-3"=~get(predictor2[1])+get(predictor2[2])+get(predictor2[3])+get(predictor2[4])),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))         
    model_variables4[[i]][j,1:2]=c(paste(single_preds[[i]]),paste(quadruple_preds[[j]][1],quadruple_preds[[j]][2]
                            ,quadruple_preds[[j]][3],quadruple_preds[[j]][4],sep ='+'))
    model_variables4[[i]][j,3]=AIC_BIC_estimator(msm_model4)$AIC_value#Computing the AIC
    model_variables4[[i]][j,4]=AIC_BIC_estimator(msm_model4)$BIC_value#Computing the BIC
           }
                  }

model_single_vs_quadruple=do.call("rbind",model_variables4)
model_single_vs_quadruple
write.csv(model_single_vs_quadruple,"model_single_vs_quadruple.csv")

#Case5 - 24 models: Fitted msm of double preds (for state 1-2) vs single preds (for state 1-3)
model_variables5=c()
for (i in 1:length(double_preds)){
       model_variables5[[i]]=as.data.frame(matrix(NA,nrow=length(single_preds),ncol=4))
      names(model_variables5[[i]])=c("Preds_1-2","Preds_1-3","AIC","BIC")
         predictor1=double_preds[[i]]
        for(j in 1:length(single_preds)){
            
         predictor2=single_preds[[j]] 
            
msm_model5=msm(Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~get(predictor1[1])+get(predictor1[2]),
                 "1-3"=~get(predictor2)),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))         
    model_variables5[[i]][j,1:2]=c(paste(double_preds[[i]][1],double_preds[[i]][2],sep ='+'),paste(single_preds[[j]]))
    model_variables5[[i]][j,3]=AIC_BIC_estimator(msm_model5)$AIC_value#Computing the AIC
    model_variables5[[i]][j,4]=AIC_BIC_estimator(msm_model5)$BIC_value#Computing the BIC
           }
                  }

model_double_vs_single=do.call("rbind",model_variables5)
model_double_vs_single
write.csv(model_double_vs_single,"model_double_vs_single.csv")

#Case6 - 36 models: Fitted msm of double preds (for state 1-2) vs double preds (for state 1-3)
model_variables6=c()
for (i in 1:length(double_preds)){
       model_variables6[[i]]=as.data.frame(matrix(NA,nrow=length(double_preds),ncol=4))
      names(model_variables6[[i]])=c("Preds_1-2","Preds_1-3","AIC","BIC")
         predictor1=double_preds[[i]]
        for(j in 1:length(double_preds)){
            
         predictor2=double_preds[[j]] 
            
msm_model6=msm(Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~get(predictor1[1])+get(predictor1[2]),
                 "1-3"=~get(predictor2[1])+get(predictor2[2])),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))         
    model_variables6[[i]][j,1:2]=c(paste(double_preds[[i]][1],double_preds[[i]][2],sep ='+'),
                                   paste(double_preds[[j]][1],double_preds[[j]][2],sep="+"))
    model_variables6[[i]][j,3]=AIC_BIC_estimator(msm_model6)$AIC_value#Computing the AIC
    model_variables6[[i]][j,4]=AIC_BIC_estimator(msm_model6)$BIC_value#Computing the BIC
           }
                  }

model_double_vs_double=do.call("rbind",model_variables6)
model_double_vs_double
write.csv(model_double_vs_double,"model_double_vs_double.csv")

#Case7 - 24 models: Fitted msm of double preds (for state 1-2) vs tripple preds (for state 1-3)
model_variables7=c()
for (i in 1:length(double_preds)){
       model_variables7[[i]]=as.data.frame(matrix(NA,nrow=length(tripple_preds),ncol=4))
      names(model_variables7[[i]])=c("Preds_1-2","Preds_1-3","AIC","BIC")
         predictor1=double_preds[[i]]
        for(j in 1:length(tripple_preds)){
            
         predictor2=tripple_preds[[j]] 
            
msm_model7=msm(Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~get(predictor1[1])+get(predictor1[2]),
                 "1-3"=~get(predictor2[1])+get(predictor2[2])+get(predictor2[3])),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))         
    model_variables7[[i]][j,1:2]=c(paste(double_preds[[i]][1],double_preds[[i]][2],sep ='+'),
                                   paste(tripple_preds[[j]][1],tripple_preds[[j]][2],tripple_preds[[j]][3],sep="+"))
    model_variables7[[i]][j,3]=AIC_BIC_estimator(msm_model7)$AIC_value#Computing the AIC
    model_variables7[[i]][j,4]=AIC_BIC_estimator(msm_model7)$BIC_value#Computing the BIC
           }
                  }

model_double_vs_tripple=do.call("rbind",model_variables7)
model_double_vs_tripple
write.csv(model_double_vs_tripple,"model_double_vs_tripple.csv")

#Case8 - 6 models: Fitted msm of double preds (for state 1-2) vs quadruple preds (for state 1-3)
model_variables8=c()
for (i in 1:length(double_preds)){
       model_variables8[[i]]=as.data.frame(matrix(NA,nrow=length(quadruple_preds),ncol=4))
      names(model_variables8[[i]])=c("Preds_1-2","Preds_1-3","AIC","BIC")
         predictor1=double_preds[[i]]
        for(j in 1:length(quadruple_preds)){
            
         predictor2=quadruple_preds[[j]] 
            
msm_model8=msm(Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~get(predictor1[1])+get(predictor1[2]),
                 "1-3"=~get(predictor2[1])+get(predictor2[2])+get(predictor2[3])+get(predictor2[4])),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))         
    model_variables8[[i]][j,1:2]=c(paste(double_preds[[i]][1],double_preds[[i]][2],sep ='+'),
    paste(quadruple_preds[[j]][1],quadruple_preds[[j]][2],quadruple_preds[[j]][3],quadruple_preds[[j]][4],sep="+"))
    model_variables8[[i]][j,3]=AIC_BIC_estimator(msm_model8)$AIC_value#Computing the AIC
    model_variables8[[i]][j,4]=AIC_BIC_estimator(msm_model8)$BIC_value#Computing the BIC
           }
                  }

model_double_vs_quadruple=do.call("rbind",model_variables8)
model_double_vs_quadruple
write.csv(model_double_vs_quadruple,"model_double_vs_quadruple.csv")

#Case9 - 16 models: Fitted msm of tripple preds (for state 1-2) vs single preds (for state 1-3)
model_variables9=c()
for (i in 1:length(tripple_preds)){
       model_variables9[[i]]=as.data.frame(matrix(NA,nrow=length(single_preds),ncol=4))
      names(model_variables9[[i]])=c("Preds_1-2","Preds_1-3","AIC","BIC")
         predictor1=tripple_preds[[i]]
        for(j in 1:length(single_preds)){
            
         predictor2=single_preds[[j]] 
            
msm_model9=msm(Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~get(predictor1[1])+get(predictor1[2])+get(predictor1[3]),
                 "1-3"=~get(predictor2)),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))         
    model_variables9[[i]][j,1:2]=c(paste(tripple_preds[[i]][1],tripple_preds[[i]][2],tripple_preds[[i]][3],sep ='+'),
    paste(single_preds[[j]]))
    model_variables9[[i]][j,3]=AIC_BIC_estimator(msm_model9)$AIC_value#Computing the AIC
    model_variables9[[i]][j,4]=AIC_BIC_estimator(msm_model9)$BIC_value#Computing the BIC
           }
                  }

model_tripple_vs_single=do.call("rbind",model_variables9)
model_tripple_vs_single
write.csv(model_tripple_vs_single,"model_tripple_vs_single.csv")

#Case10 - 24 models: Fitted msm of tripple preds (for state 1-2) vs double preds (for state 1-3)
model_variables10=c()
for (i in 1:length(tripple_preds)){
       model_variables10[[i]]=as.data.frame(matrix(NA,nrow=length(double_preds),ncol=4))
      names(model_variables10[[i]])=c("Preds_1-2","Preds_1-3","AIC","BIC")
         predictor1=tripple_preds[[i]]
        for(j in 1:length(double_preds)){
            
         predictor2=double_preds[[j]] 
            
msm_model10=msm(Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~get(predictor1[1])+get(predictor1[2])+get(predictor1[3]),
                 "1-3"=~get(predictor2[1])+get(predictor2[2])),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))          
    model_variables10[[i]][j,1:2]=c(paste(tripple_preds[[i]][1],tripple_preds[[i]][2],tripple_preds[[i]][3],sep ='+'),
    paste(double_preds[[j]][1],double_preds[[j]][2],sep="+"))
    model_variables10[[i]][j,3]=AIC_BIC_estimator(msm_model10)$AIC_value#Computing the AIC
    model_variables10[[i]][j,4]=AIC_BIC_estimator(msm_model10)$BIC_value#Computing the BIC
           }
                  }

model_tripple_vs_double=do.call("rbind",model_variables10)
model_tripple_vs_double
write.csv(model_tripple_vs_double,"model_tripple_vs_double.csv")

#Case11 - 16 models: Fitted msm of tripple preds (for state 1-2) vs tripple preds (for state 1-3)
model_variables11=c()
for (i in 1:length(tripple_preds)){
       model_variables11[[i]]=as.data.frame(matrix(NA,nrow=length(tripple_preds),ncol=4))
      names(model_variables11[[i]])=c("Preds_1-2","Preds_1-3","AIC","BIC")
         predictor1=tripple_preds[[i]]
        for(j in 1:length(tripple_preds)){
            
         predictor2=tripple_preds[[j]] 
            
msm_model11=msm(Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~get(predictor1[1])+get(predictor1[2])+get(predictor1[3]),
"1-3"=~get(predictor2[1])+get(predictor2[2])+get(predictor2[3])),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))          
    model_variables11[[i]][j,1:2]=c(paste(tripple_preds[[i]][1],tripple_preds[[i]][2],tripple_preds[[i]][3],sep ='+'),
    paste(tripple_preds[[j]][1],tripple_preds[[j]][2],tripple_preds[[j]][3],sep="+"))
    model_variables11[[i]][j,3]=AIC_BIC_estimator(msm_model11)$AIC_value#Computing the AIC
    model_variables11[[i]][j,4]=AIC_BIC_estimator(msm_model11)$BIC_value#Computing the BIC
           }
                  }

model_tripple_vs_tripple=do.call("rbind",model_variables11)
model_tripple_vs_tripple
write.csv(model_tripple_vs_tripple,"model_tripple_vs_tripple.csv")

#Case12 - 4 models: Fitted msm of tripple preds (for state 1-2) vs quadruple preds (for state 1-3)
model_variables12=c()
for (i in 1:length(tripple_preds)){
       model_variables12[[i]]=as.data.frame(matrix(NA,nrow=length(quadruple_preds),ncol=4))
      names(model_variables12[[i]])=c("Preds_1-2","Preds_1-3","AIC","BIC")
         predictor1=tripple_preds[[i]]
        for(j in 1:length(quadruple_preds)){
            
         predictor2=quadruple_preds[[j]] 
            
msm_model12=msm(Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~get(predictor1[1])+get(predictor1[2])+get(predictor1[3]),
                 "1-3"=~get(predictor2[1])+get(predictor2[2])+get(predictor2[3])+get(predictor2[4])),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))           
    model_variables12[[i]][j,1:2]=c(paste(tripple_preds[[i]][1],tripple_preds[[i]][2],tripple_preds[[i]][3],sep ='+'),
paste(quadruple_preds[[j]][1],quadruple_preds[[j]][2],quadruple_preds[[j]][3],quadruple_preds[[j]][4],sep="+"))
    model_variables12[[i]][j,3]=AIC_BIC_estimator(msm_model12)$AIC_value#Computing the AIC
    model_variables12[[i]][j,4]=AIC_BIC_estimator(msm_model12)$BIC_value#Computing the BIC
           }
                  }

model_tripple_vs_quadruple=do.call("rbind",model_variables12)
model_tripple_vs_quadruple
write.csv(model_tripple_vs_quadruple,"model_tripple_vs_quadruple.csv")

#Case13 - 4 models: Fitted msm of quadruple preds (for state 1-2) vs single preds (for state 1-3)
model_variables13=c()
for (i in 1:length(quadruple_preds)){
       model_variables13[[i]]=as.data.frame(matrix(NA,nrow=length(single_preds),ncol=4))
      names(model_variables13[[i]])=c("Preds_1-2","Preds_1-3","AIC","BIC")
         predictor1=quadruple_preds[[i]]
        for(j in 1:length(single_preds)){
            
         predictor2=single_preds[[j]] 
            
msm_model13=msm(Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~get(predictor1[1])+get(predictor1[2])+get(predictor1[3])+get(predictor1[4]),
                 "1-3"=~get(predictor2)),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))          
    model_variables13[[i]][j,1:2]=c(paste(quadruple_preds[[i]][1],quadruple_preds[[i]][2],quadruple_preds[[i]][3]
                                ,quadruple_preds[[i]][4],sep ='+'),paste(single_preds[[j]]))
    model_variables13[[i]][j,3]=AIC_BIC_estimator(msm_model13)$AIC_value#Computing the AIC
    model_variables13[[i]][j,4]=AIC_BIC_estimator(msm_model13)$BIC_value#Computing the BIC
           }
                  }

model_quadruple_vs_single=do.call("rbind",model_variables13)
model_quadruple_vs_single
write.csv(model_quadruple_vs_single,"model_quadruple_vs_single.csv")

#Case14 - 6 models: Fitted msm of quadruple preds (for state 1-2) vs double preds (for state 1-3)
model_variables14=c()
for (i in 1:length(quadruple_preds)){
       model_variables14[[i]]=as.data.frame(matrix(NA,nrow=length(double_preds),ncol=4))
      names(model_variables14[[i]])=c("Preds_1-2","Preds_1-3","AIC","BIC")
         predictor1=quadruple_preds[[i]]
        for(j in 1:length(double_preds)){
            
         predictor2=double_preds[[j]] 
            
msm_model14=msm(Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~get(predictor1[1])+get(predictor1[2])+get(predictor1[3])+get(predictor1[4]),
                 "1-3"=~get(predictor2[1])+get(predictor2[2])),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))         
    model_variables14[[i]][j,1:2]=c(paste(quadruple_preds[[i]][1],quadruple_preds[[i]][2],quadruple_preds[[i]][3]
                                ,quadruple_preds[[i]][4],sep ='+'),
        paste(double_preds[[j]][1],double_preds[[j]][2],sep="+"))
    model_variables14[[i]][j,3]=AIC_BIC_estimator(msm_model14)$AIC_value#Computing the AIC
    model_variables14[[i]][j,4]=AIC_BIC_estimator(msm_model14)$BIC_value#Computing the BIC
           }
                  }

model_quadruple_vs_double=do.call("rbind",model_variables14)
model_quadruple_vs_double
write.csv(model_quadruple_vs_double,"model_quadruple_vs_double.csv")

#Case15 - 4 models: Fitted msm of quadruple preds (for state 1-2) vs tripple preds (for state 1-3)
model_variables15=c()
for (i in 1:length(quadruple_preds)){
       model_variables15[[i]]=as.data.frame(matrix(NA,nrow=length(tripple_preds),ncol=4))
      names(model_variables15[[i]])=c("Preds_1-2","Preds_1-3","AIC","BIC")
         predictor1=quadruple_preds[[i]]
        for(j in 1:length(tripple_preds)){
            
         predictor2=tripple_preds[[j]] 
            
msm_model15=msm(Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~get(predictor1[1])+get(predictor1[2])+get(predictor1[3])+get(predictor1[4]),
                 "1-3"=~get(predictor2[1])+get(predictor2[2])+get(predictor2[3])),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))        
    model_variables15[[i]][j,1:2]=c(paste(quadruple_preds[[i]][1],quadruple_preds[[i]][2],quadruple_preds[[i]][3]
                                ,quadruple_preds[[i]][4],sep ='+'),
        paste(tripple_preds[[j]][1],tripple_preds[[j]][2],tripple_preds[[j]][3],sep="+"))
    model_variables15[[i]][j,3]=AIC_BIC_estimator(msm_model15)$AIC_value#Computing the AIC
    model_variables15[[i]][j,4]=AIC_BIC_estimator(msm_model15)$BIC_value#Computing the BIC
           }
                  }

model_quadruple_vs_tripple=do.call("rbind",model_variables15)
model_quadruple_vs_tripple
write.csv(model_quadruple_vs_tripple,"model_quadruple_vs_tripple.csv")

#Case16 - 1 model: Fitted msm of quadruple preds (for state 1-2) vs quadruple preds (for state 1-3)
model_variables16=c()
for (i in 1:length(quadruple_preds)){
       model_variables16[[i]]=as.data.frame(matrix(NA,nrow=length(quadruple_preds),ncol=4))
      names(model_variables16[[i]])=c("Preds_1-2","Preds_1-3","AIC","BIC")
         predictor1=quadruple_preds[[i]]
        for(j in 1:length(quadruple_preds)){
            
         predictor2=quadruple_preds[[j]] 
            
msm_model16=msm(Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~get(predictor1[1])+get(predictor1[2])+get(predictor1[3])
                 +get(predictor1[4]),
"1-3"=~get(predictor2[1])+get(predictor2[2])+get(predictor2[3])
                 +get(predictor2[4])
                ),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))        
    model_variables16[[i]][j,1:2]=c(paste(quadruple_preds[[i]][1],quadruple_preds[[i]][2],quadruple_preds[[i]][3]
                                ,quadruple_preds[[i]][4],sep ='+'),
        paste(quadruple_preds[[j]][1],quadruple_preds[[j]][2],quadruple_preds[[j]][3],quadruple_preds[[j]][4],sep="+"))
    model_variables16[[i]][j,3]=AIC_BIC_estimator(msm_model16)$AIC_value#Computing the AIC
    model_variables16[[i]][j,4]=AIC_BIC_estimator(msm_model16)$BIC_value#Computing the BIC
           }
                  }

model_quadruple_vs_quadruple=do.call("rbind",model_variables16)
model_quadruple_vs_quadruple
write.csv(model_quadruple_vs_quadruple,"model_quadruple_vs_quadruple.csv")

MSM_combined_multiple=rbind(model_single_vs_single,model_single_vs_double,model_single_vs_tripple,model_single_vs_quadruple,
 model_double_vs_single,model_double_vs_double,model_double_vs_tripple,model_double_vs_quadruple,
 model_tripple_vs_single,model_tripple_vs_double,model_tripple_vs_tripple,model_tripple_vs_quadruple,
model_quadruple_vs_single,model_quadruple_vs_double,model_quadruple_vs_tripple,model_quadruple_vs_quadruple)

names(MSM_combined_multiple)=c("Predictors for  State 1-2 transition","Predictors for  State 1-3 transition","AIC","BIC")
MSM_combined_multiple

write.csv(MSM_combined_multiple,"MSM_combined_multiple.csv")

#Importing previous results of the 225 models
MSM_combined_1=read.csv(file="MSM_combined_multiple.csv")
MSM_combined_multiple1=MSM_combined_1[,-1]
names(MSM_combined_multiple1)=c("Predictors for  State 1-2 transition","Predictors for  State 1-3 transition","AIC","BIC")

#Case17 - 4 models: Fitted msm of single preds (for state 1-2) vs empty preds (for state 1-3)
empty_pred=c(1)
model_variables17=c()
for (i in 1:length(single_preds)){
       model_variables17[[i]]=as.data.frame(matrix(NA,nrow=length(empty_pred),ncol=4))
      names(model_variables17[[i]])=c("Preds_1-2","Preds_1-3","AIC","BIC")
         predictor1=single_preds[[i]]

    
            
msm_model17=msm( Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~get(predictor1),"1-3"=~1),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))        
         model_variables17[[i]][1,1:2]=c(single_preds[[i]],c("empty"))
         model_variables17[[i]][j,3]=AIC_BIC_estimator(msm_model17)$AIC_value#Computing the AIC
         model_variables17[[i]][j,4]=AIC_BIC_estimator(msm_model17)$BIC_value#Computing the BIC
           
                  }

model_single_vs_empty=do.call("rbind",model_variables17)
model_single_vs_empty
write.csv(model_single_vs_empty,"model_single_vs_empty.csv")

#Case18 - 6 models: Fitted msm of double preds (for state 1-2) vs empty preds (for state 1-3)
empty_pred=c(1)
model_variables18=c()
for (i in 1:length(double_preds)){
       model_variables18[[i]]=as.data.frame(matrix(NA,nrow=length(empty_pred),ncol=4))
      names(model_variables18[[i]])=c("Preds_1-2","Preds_1-3","AIC","BIC")
         predictor1=double_preds[[i]]

    
            
msm_model18=msm( Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~get(predictor1[1])+get(predictor1[2]),"1-3"=~1),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))        
         model_variables18[[i]][1,1:2]=c(paste(double_preds[[i]][1],double_preds[[i]][2],sep="+"),c("empty"))
         model_variables18[[i]][j,3]=AIC_BIC_estimator(msm_model18)$AIC_value#Computing the AIC
         model_variables18[[i]][j,4]=AIC_BIC_estimator(msm_model18)$BIC_value#Computing the BIC
           
                  }

model_double_vs_empty=do.call("rbind",model_variables18)
model_double_vs_empty
write.csv(model_double_vs_empty,"model_double_vs_empty.csv")

#Case19 - 4 models: Fitted msm of tripple preds (for state 1-2) vs empty preds (for state 1-3)
empty_pred=c(1)
model_variables19=c()
for (i in 1:length(tripple_preds)){
       model_variables19[[i]]=as.data.frame(matrix(NA,nrow=length(empty_pred),ncol=4))
      names(model_variables19[[i]])=c("Preds_1-2","Preds_1-3","AIC","BIC")
         predictor1=tripple_preds[[i]]

    
            
msm_model19=msm( Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~get(predictor1[1])+get(predictor1[2])+get(predictor1[3]),"1-3"=~1),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))        
         model_variables19[[i]][1,1:2]=c(paste(tripple_preds[[i]][1],tripple_preds[[i]][2],
                                               tripple_preds[[i]][3],sep="+"),c("empty"))
        model_variables19[[i]][j,3]=AIC_BIC_estimator(msm_model19)$AIC_value#Computing the AIC
        model_variables19[[i]][j,4]=AIC_BIC_estimator(msm_model19)$BIC_value#Computing the BIC
           
                  }

model_tripple_vs_empty=do.call("rbind",model_variables19)
model_tripple_vs_empty
write.csv(model_tripple_vs_empty,"model_tripple_vs_empty.csv")

#Case20 - 1 model: Fitted msm of quadruple preds (for state 1-2) vs empty preds (for state 1-3)
empty_pred=c(1)
model_variables20=c()
for (i in 1:length(quadruple_preds)){
       model_variables20[[i]]=as.data.frame(matrix(NA,nrow=length(empty_pred),ncol=4))
      names(model_variables20[[i]])=c("Preds_1-2","Preds_1-3","AIC","BIC")
         predictor1=quadruple_preds[[i]]
         
msm_model20=msm( Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~get(predictor1[1])+get(predictor1[2])+get(predictor1[3])+get(predictor1[4])
                 ,"1-3"=~1),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))        
         model_variables20[[i]][1,1:2]=c(paste(quadruple_preds[[i]][1],quadruple_preds[[i]][2],
            quadruple_preds[[i]][3], quadruple_preds[[i]][4],sep="+"),c("empty"))
        model_variables20[[i]][j,3]=AIC_BIC_estimator(msm_model20)$AIC_value#Computing the AIC
        model_variables20[[i]][j,4]=AIC_BIC_estimator(msm_model20)$BIC_value#Computing the BIC
           
                  }

model_quadruple_vs_empty=do.call("rbind",model_variables20)
model_quadruple_vs_empty
write.csv(model_quadruple_vs_empty,"model_quadruple_vs_empty.csv")

#Case21 - 4 models: Fitted msm of empty preds (for state 1-2) vs single preds (for state 1-3)
empty_pred=c(1)
model_variables21=c()
for (i in 1:length(single_preds)){
       model_variables21[[i]]=as.data.frame(matrix(NA,nrow=length(empty_pred),ncol=4))
      names(model_variables21[[i]])=c("Preds_1-2","Preds_1-3","AIC","BIC")
         predictor2=single_preds[[i]]

    
            
msm_model21=msm( Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~1,"1-3"=~get(predictor2)),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))        
         model_variables21[[i]][1,1:2]=c(c("empty"),single_preds[[i]])
         model_variables21[[i]][j,3]=AIC_BIC_estimator(msm_model21)$AIC_value#Computing the AIC
         model_variables21[[i]][j,4]=AIC_BIC_estimator(msm_model21)$BIC_value#Computing the BIC
           
                  }

model_empty_vs_single=do.call("rbind",model_variables21)
model_empty_vs_single
write.csv(model_empty_vs_single,"model_empty_vs_single.csv")

#Case22 - 6 models: Fitted msm of empty preds (for state 1-2) vs double preds (for state 1-3)
empty_pred=c(1)
model_variables22=c()
for (i in 1:length(double_preds)){
       model_variables22[[i]]=as.data.frame(matrix(NA,nrow=length(empty_pred),ncol=4))
      names(model_variables22[[i]])=c("Preds_1-2","Preds_1-3","AIC","BIC")
         predictor2=double_preds[[i]]
         
msm_model22=msm( Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~1,"1-3"=~get(predictor2[1])+get(predictor2[2])),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))        
         model_variables22[[i]][1,1:2]=c(c("empty"),paste(double_preds[[i]][1],double_preds[[i]][2],sep="+"))
         model_variables22[[i]][j,3]=AIC_BIC_estimator(msm_model22)$AIC_value#Computing the AIC
         model_variables22[[i]][j,4]=AIC_BIC_estimator(msm_model22)$BIC_value#Computing the BIC
           
                  }

model_empty_vs_double=do.call("rbind",model_variables22)
model_empty_vs_double
write.csv(model_empty_vs_double,"model_empty_vs_double.csv")

#Case23 - 4 models: Fitted msm of empty preds (for state 1-2) vs tripple preds (for state 1-3)
empty_pred=c(1)
model_variables23=c()
for (i in 1:length(tripple_preds)){
       model_variables23[[i]]=as.data.frame(matrix(NA,nrow=length(empty_pred),ncol=4))
      names(model_variables23[[i]])=c("Preds_1-2","Preds_1-3","AIC","BIC")
         predictor2=tripple_preds[[i]]
         
msm_model23=msm( Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~1,"1-3"=~get(predictor2[1])+get(predictor2[2])+get(predictor2[3])),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))        
         model_variables23[[i]][1,1:2]=c(c("empty"),paste(tripple_preds[[i]][1],tripple_preds[[i]][2],
        tripple_preds[[i]][3],sep="+"))
        model_variables23[[i]][j,3]=AIC_BIC_estimator(msm_model23)$AIC_value#Computing the AIC
        model_variables23[[i]][j,4]=AIC_BIC_estimator(msm_model23)$BIC_value#Computing the BIC
           
                  }

model_empty_vs_tripple=do.call("rbind",model_variables23)
model_empty_vs_tripple
write.csv(model_empty_vs_tripple,"model_empty_vs_tripple.csv")

#Case24 - 1 model: Fitted msm of empty preds (for state 1-2) vs quadruple preds (for state 1-3)
empty_pred=c(1)
model_variables24=c()
for (i in 1:length(quadruple_preds)){
       model_variables24[[i]]=as.data.frame(matrix(NA,nrow=length(empty_pred),ncol=4))
      names(model_variables24[[i]])=c("Preds_1-2","Preds_1-3","AIC","BIC")
         predictor2=quadruple_preds[[i]]
         
msm_model24=msm( Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~1,"1-3"=~get(predictor2[1])+get(predictor2[2])+get(predictor2[3])
                +get(predictor2[4])),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))        
         model_variables24[[i]][1,1:2]=c(c("empty"),paste(quadruple_preds[[i]][1],quadruple_preds[[i]][2],
        quadruple_preds[[i]][3],quadruple_preds[[i]][4],sep="+"))
        model_variables24[[i]][j,3]=AIC_BIC_estimator(msm_model24)$AIC_value#Computing the AIC
        model_variables24[[i]][j,4]=AIC_BIC_estimator(msm_model24)$BIC_value#Computing the BIC
           
                  }

model_empty_vs_quadruple=do.call("rbind",model_variables24)
model_empty_vs_quadruple
write.csv(model_empty_vs_quadruple,"model_empty_vs_quadruple.csv")

#Case25 - 1 model: Fitted msm of empty preds (for state 1-2) vs empty preds (for state 1-3)
empty_pred=c(1)


model_variables25=as.data.frame(matrix(NA,nrow=length(empty_pred),ncol=4))
names(model_variables25)=c("Preds_1-2","Preds_1-3","AIC","BIC")

msm_model25=msm( Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude,exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,deathexact=3,obstype=2,
                           control = list(fnscale = 4000, maxit = 10000))        
  
model_variables25[1,1:2]=c(c("empty"),c("empty"))
       model_variables25[1,3]=AIC(msm_model25)
model_variables25[1,3]=AIC_BIC_estimator(msm_model25)$AIC_value#Computing the AIC
model_variables25[1,4]=AIC_BIC_estimator(msm_model25)$BIC_value#Computing the BIC                    

model_empty_vs_empty=model_variables25
model_empty_vs_empty
write.csv(model_empty_vs_empty,"model_empty_vs_empty.csv")

#Combining the results of the last 31 models
MSM_combined_multiple2=rbind(model_single_vs_empty,model_double_vs_empty,model_tripple_vs_empty,model_quadruple_vs_empty,
  model_empty_vs_single, model_empty_vs_double,model_empty_vs_tripple,model_empty_vs_quadruple,model_empty_vs_empty)

names(MSM_combined_multiple2)=c("Predictors for  State 1-2 transition","Predictors for  State 1-3 transition","AIC","BIC")
MSM_combined_multiple2

names(MSM_combined_multiple2)
names(MSM_combined_multiple1)

#Combine all 256 data/fitted model AIC and BIC statistics
MSM_combined_multiple=rbind(MSM_combined_multiple1,MSM_combined_multiple2)
MSM_combined_multiple

write.csv(MSM_combined_multiple,"MSM_combined_multiple.csv")

# Choosing the best model among all the fitted models (based on the AIC and BIC)
MSM_combined_multiple[which.min(MSM_combined_multiple$AIC),]
MSM_combined_multiple[which.min(MSM_combined_multiple$BIC),]

#Comparing between the two best model using a Likelihood ratio test

#Simpler model
Multistate_model_244=msm(Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~1,
                 "1-3"=~Fish_population),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,
                          deathexact=3,obstype=2,control = list(fnscale = 4000, maxit = 10000))

#Relatively multivariable model
Multistate_model_47=msm(Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~Fish_size,
                 "1-3"=~Sex_fish+Parasite_type+Fish_population),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,
                          deathexact=3,obstype=2,control = list(fnscale = 4000, maxit = 10000))


#Function to perform LRT for a multistate Markov model
LRT_func<- function(model1,model2){
    minus2LogLik1 <- model1$minus2loglik
    minus2LogLik2 <- model2$minus2loglik
    loglik1<-  minus2LogLik1/(-2)#loglikelihood of model1
    loglik2<-  minus2LogLik2/(-2)#loglikelihood of model2
    Lambda_tstats<- -2*(loglik1-loglik2)
    degree_freedom<-  model2$paramdata$nopt-model1$paramdata$nopt#diference between model parameters
    p.val <- pchisq(Lambda_tstats, df =degree_freedom, lower.tail = FALSE)
    if(p.val<0.05) print(paste("The complex model fits the data better than the simple model "))
    else if(p.val>=0.05) print(paste("The complex model does not fit the data better than the simple model "))
    return(list(teststats=Lambda_tstats,p_value=p.val))
}



LRT_func(model1=Multistate_model_244,model2=Multistate_model_47)

Multistate_model_best=msm(Multistates ~ Time, subject=ID, data =Multistate_data, qmatrix = Q.crude, 
covariates= list("1-2"=~Fish_size,
                 "1-3"=~Sex_fish+Parasite_type+Fish_population),exacttimes=F,method="BFGS",
                            pci=c(3,5,7,9,11,13,15),gen.inits=TRUE,
                          deathexact=3,obstype=2,control = list(fnscale = 4000, maxit = 10000))

for(i in 1:length(Multistate_model_best$qcmodel$covlabels[-c(7,8,9,10,11,12,13)])){
    print(MSM_pvalues(msm_model=Multistate_model_best,covariate_num=i))
    }

TP=NULL
time=seq(1,17,2)
for (i in time){
TP[[i]]=pmatrix.msm(Multistate_model_best, t=i)
    print(TP[[i]])
    }

#Extracting transition probabilities for each state over time 
Prob_state1= c(TP[[1]][1,1],TP[[3]][1,1],TP[[5]][1,1],TP[[7]][1,1],TP[[9]][1,1],
                  TP[[11]][1,1],TP[[13]][1,1],TP[[15]][1,1],TP[[17]][1,1])
Prob_state2= c(TP[[1]][1,2],TP[[3]][1,2],TP[[5]][1,2],TP[[7]][1,2],TP[[9]][1,2],
                  TP[[11]][1,2],TP[[13]][1,2],TP[[15]][1,2],TP[[17]][1,2]) 
Prob_state3= c(TP[[1]][1,3],TP[[3]][1,3],TP[[5]][1,3],TP[[7]][1,3],TP[[9]][1,3],
                  TP[[11]][1,3],TP[[13]][1,3],TP[[15]][1,3],TP[[17]][1,3])


Gt3_fishType_Deathrate_malefish=array(dim=c(3,8,3)) #estimates and its 95% confidence intervals
Gt3_fishType_Deathrate_femalefish=array(dim=c(3,8,3)) #estimates and its 95% confidence intervals

Gt_fishType_Deathrate_malefish=array(dim=c(3,8,3)) #estimates and its 95% confidence intervals
Gt_fishType_Deathrate_femalefish=array(dim=c(3,8,3)) #estimates and its 95% confidence intervals

Gb_fishType_Deathrate_malefish=array(dim=c(3,8,3)) #estimates and its 95% confidence intervals
Gb_fishType_Deathrate_femalefish=array(dim=c(3,8,3)) #estimates and its 95% confidence intervals

#Gt3-UA-Male fish predicted death rate
Gt3_fishType_Deathrate_malefish[1,1, ]=as.vector(qmatrix.msm(Multistate_model_best, 
                covariates = list (Parasite_type = "Gt3",Fish_population='Upper Aripo',Sex_fish="Male fish"))[1,3])[c(1,3,4)]


Gt3_fishType_Deathrate_malefish[1,2, ]=as.vector(qmatrix.msm(Multistate_model_best, 
            covariates = list (Parasite_type = "Gt3", `timeperiod[3,5)`=1,Fish_population='Upper Aripo',
                              Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_malefish[1,3, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt3", `timeperiod[5,7)`=1,Fish_population='Upper Aripo',
                      Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_malefish[1,4, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt3", `timeperiod[7,9)`=1,Fish_population='Upper Aripo',
                      Sex_fish="Male fish"))[1,3])[c(1,3,4)]


Gt3_fishType_Deathrate_malefish[1,5, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt3", `timeperiod[9,11)`=1,Fish_population='Upper Aripo',
                      Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_malefish[1,6, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt3", `timeperiod[11,13)`=1,Fish_population='Upper Aripo',
                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_malefish[1,7, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt3", `timeperiod[13,15)`=1,Fish_population='Upper Aripo',
                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_malefish[1,8, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt3", `timeperiod[15,Inf)`=1,Fish_population='Upper Aripo',
                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]


Gt3_fishType_Deathrate_malefish[1, ,]

#Gt3-UA-Female fish predicted death rate
Gt3_fishType_Deathrate_femalefish[1,1, ]=as.vector(qmatrix.msm(Multistate_model_best, 
                covariates = list (Parasite_type = "Gt3",Fish_population='Upper Aripo',Sex_fish="Female fish"))[1,3])[c(1,3,4)]




Gt3_fishType_Deathrate_femalefish[1,2, ]=as.vector(qmatrix.msm(Multistate_model_best, 
            covariates = list (Parasite_type = "Gt3", `timeperiod[3,5)`=1,Fish_population='Upper Aripo',
                              Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_femalefish[1,3, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt3", `timeperiod[5,7)`=1,Fish_population='Upper Aripo',
                      Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_femalefish[1,4, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt3", `timeperiod[7,9)`=1,Fish_population='Upper Aripo',
                      Sex_fish="Female fish"))[1,3])[c(1,3,4)]


Gt3_fishType_Deathrate_femalefish[1,5, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt3", `timeperiod[9,11)`=1,Fish_population='Upper Aripo',
                      Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_femalefish[1,6, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt3", `timeperiod[11,13)`=1,Fish_population='Upper Aripo',
                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_femalefish[1,7, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt3", `timeperiod[13,15)`=1,Fish_population='Upper Aripo',
                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_femalefish[1,8, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt3", `timeperiod[15,Inf)`=1,Fish_population='Upper Aripo',
                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]


Gt3_fishType_Deathrate_femalefish[1, ,]

#Gt3-LA-Male fish predicted death rate
Gt3_fishType_Deathrate_malefish[2,1, ]=as.vector(qmatrix.msm(Multistate_model_best, 
                covariates = list (Parasite_type = "Gt3",Fish_population='Lower Aripo',
                                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_malefish[2,2, ]=as.vector(qmatrix.msm(Multistate_model_best, 
            covariates = list (Parasite_type = "Gt3", `timeperiod[3,5)`=1,Fish_population='Lower Aripo',
                               Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_malefish[2,3, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt3", `timeperiod[5,7)`=1,Fish_population='Lower Aripo',
                       Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_malefish[2,4, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt3", `timeperiod[7,9)`=1,Fish_population='Lower Aripo',
                       Sex_fish="Male fish"))[1,3])[c(1,3,4)]


Gt3_fishType_Deathrate_malefish[2,5, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt3", `timeperiod[9,11)`=1,Fish_population='Lower Aripo',
                       Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_malefish[2,6, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt3", `timeperiod[11,13)`=1,Fish_population='Lower Aripo',
                    Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_malefish[2,7, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt3", `timeperiod[13,15)`=1,Fish_population='Lower Aripo',
                    Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_malefish[2,8, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt3", `timeperiod[15,Inf)`=1,Fish_population='Lower Aripo',
                    Sex_fish="Male fish"))[1,3])[c(1,3,4)]


Gt3_fishType_Deathrate_malefish[2, ,]

#Gt3-LA-Female fish predicted death rate
Gt3_fishType_Deathrate_femalefish[2,1, ]=as.vector(qmatrix.msm(Multistate_model_best, 
                covariates = list (Parasite_type = "Gt3",Fish_population='Lower Aripo',
                                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_femalefish[2,2, ]=as.vector(qmatrix.msm(Multistate_model_best, 
            covariates = list (Parasite_type = "Gt3", `timeperiod[3,5)`=1,Fish_population='Lower Aripo',
                               Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_femalefish[2,3, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt3", `timeperiod[5,7)`=1,Fish_population='Lower Aripo',
                       Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_femalefish[2,4, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt3", `timeperiod[7,9)`=1,Fish_population='Lower Aripo',
                       Sex_fish="Female fish"))[1,3])[c(1,3,4)]


Gt3_fishType_Deathrate_femalefish[2,5, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt3", `timeperiod[9,11)`=1,Fish_population='Lower Aripo',
                       Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_femalefish[2,6, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt3", `timeperiod[11,13)`=1,Fish_population='Lower Aripo',
                    Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_femalefish[2,7, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt3", `timeperiod[13,15)`=1,Fish_population='Lower Aripo',
                    Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_femalefish[2,8, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt3", `timeperiod[15,Inf)`=1,Fish_population='Lower Aripo',
                    Sex_fish="Female fish"))[1,3])[c(1,3,4)]


Gt3_fishType_Deathrate_femalefish[2, ,]

#Gt3-OS-Male fish predicted death rate
Gt3_fishType_Deathrate_malefish[3,1, ]=as.vector(qmatrix.msm(Multistate_model_best, 
                covariates = list (Parasite_type = "Gt3",Fish_population='Ornamental',
                                  Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_malefish[3,2, ]=as.vector(qmatrix.msm(Multistate_model_best, 
            covariates = list (Parasite_type = "Gt3", `timeperiod[3,5)`=1,Fish_population='Ornamental',
                              Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_malefish[3,3, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt3", `timeperiod[5,7)`=1,Fish_population='Ornamental',
                      Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_malefish[3,4, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt3", `timeperiod[7,9)`=1,Fish_population='Ornamental',
                      Sex_fish="Male fish"))[1,3])[c(1,3,4)]


Gt3_fishType_Deathrate_malefish[3,5, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt3", `timeperiod[9,11)`=1,Fish_population='Ornamental',
                      Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_malefish[3,6, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt3", `timeperiod[11,13)`=1,Fish_population='Ornamental',
                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_malefish[3,7, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt3", `timeperiod[13,15)`=1,Fish_population='Ornamental',
                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_malefish[3,8, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt3", `timeperiod[15,Inf)`=1,Fish_population='Ornamental',
                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]


Gt3_fishType_Deathrate_malefish[3, ,]

#Gt3-OS-Female fish predicted death rate
Gt3_fishType_Deathrate_femalefish[3,1, ]=as.vector(qmatrix.msm(Multistate_model_best, 
                covariates = list (Parasite_type = "Gt3",Fish_population='Ornamental',
                                  Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_femalefish[3,2, ]=as.vector(qmatrix.msm(Multistate_model_best, 
            covariates = list (Parasite_type = "Gt3", `timeperiod[3,5)`=1,Fish_population='Ornamental',
                              Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_femalefish[3,3, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt3", `timeperiod[5,7)`=1,Fish_population='Ornamental',
                      Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_femalefish[3,4, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt3", `timeperiod[7,9)`=1,Fish_population='Ornamental',
                      Sex_fish="Female fish"))[1,3])[c(1,3,4)]


Gt3_fishType_Deathrate_femalefish[3,5, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt3", `timeperiod[9,11)`=1,Fish_population='Ornamental',
                      Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_femalefish[3,6, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt3", `timeperiod[11,13)`=1,Fish_population='Ornamental',
                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_femalefish[3,7, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt3", `timeperiod[13,15)`=1,Fish_population='Ornamental',
                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt3_fishType_Deathrate_femalefish[3,8, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt3", `timeperiod[15,Inf)`=1,Fish_population='Ornamental',
                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]


Gt3_fishType_Deathrate_femalefish[3, ,]

#Gt-UA-Male fish predicted death rate
Gt_fishType_Deathrate_malefish[1,1, ]=as.vector(qmatrix.msm(Multistate_model_best, 
                covariates = list (Parasite_type = "Gt",Fish_population='Upper Aripo',
                                  Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_malefish[1,2, ]=as.vector(qmatrix.msm(Multistate_model_best, 
            covariates = list (Parasite_type = "Gt", `timeperiod[3,5)`=1,Fish_population='Upper Aripo',
                              Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_malefish[1,3, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt", `timeperiod[5,7)`=1,Fish_population='Upper Aripo',
                      Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_malefish[1,4, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt", `timeperiod[7,9)`=1,Fish_population='Upper Aripo',
                      Sex_fish="Male fish"))[1,3])[c(1,3,4)]


Gt_fishType_Deathrate_malefish[1,5, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt", `timeperiod[9,11)`=1,Fish_population='Upper Aripo',
                      Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_malefish[1,6, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt", `timeperiod[11,13)`=1,Fish_population='Upper Aripo',
                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_malefish[1,7, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt", `timeperiod[13,15)`=1,Fish_population='Upper Aripo',
                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_malefish[1,8, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt", `timeperiod[15,Inf)`=1,Fish_population='Upper Aripo',
                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]


Gt_fishType_Deathrate_malefish[1, ,]

#Gt-UA-Female fish predicted death rate
Gt_fishType_Deathrate_femalefish[1,1, ]=as.vector(qmatrix.msm(Multistate_model_best, 
                covariates = list (Parasite_type = "Gt",Fish_population='Upper Aripo',
                                  Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_femalefish[1,2, ]=as.vector(qmatrix.msm(Multistate_model_best, 
            covariates = list (Parasite_type = "Gt", `timeperiod[3,5)`=1,Fish_population='Upper Aripo',
                              Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_femalefish[1,3, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt", `timeperiod[5,7)`=1,Fish_population='Upper Aripo',
                      Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_femalefish[1,4, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt", `timeperiod[7,9)`=1,Fish_population='Upper Aripo',
                      Sex_fish="Female fish"))[1,3])[c(1,3,4)]


Gt_fishType_Deathrate_femalefish[1,5, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt", `timeperiod[9,11)`=1,Fish_population='Upper Aripo',
                      Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_femalefish[1,6, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt", `timeperiod[11,13)`=1,Fish_population='Upper Aripo',
                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_femalefish[1,7, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt", `timeperiod[13,15)`=1,Fish_population='Upper Aripo',
                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_femalefish[1,8, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt", `timeperiod[15,Inf)`=1,Fish_population='Upper Aripo',
                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]


Gt_fishType_Deathrate_femalefish[1, ,]

#Gt-LA-Male fish predicted death rate
Gt_fishType_Deathrate_malefish[2,1, ]=as.vector(qmatrix.msm(Multistate_model_best, 
                covariates = list (Parasite_type = "Gt",Fish_population='Lower Aripo',
                                  Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_malefish[2,2, ]=as.vector(qmatrix.msm(Multistate_model_best, 
            covariates = list (Parasite_type = "Gt", `timeperiod[3,5)`=1,Fish_population='Lower Aripo',
                              Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_malefish[2,3, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt", `timeperiod[5,7)`=1,Fish_population='Lower Aripo',
                      Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_malefish[2,4, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt", `timeperiod[7,9)`=1,Fish_population='Lower Aripo',
                      Sex_fish="Male fish"))[1,3])[c(1,3,4)]


Gt_fishType_Deathrate_malefish[2,5, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt", `timeperiod[9,11)`=1,Fish_population='Lower Aripo',
                      Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_malefish[2,6, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt", `timeperiod[11,13)`=1,Fish_population='Lower Aripo',
                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_malefish[2,7, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt", `timeperiod[13,15)`=1,Fish_population='Lower Aripo',
                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_malefish[2,8, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt", `timeperiod[15,Inf)`=1,Fish_population='Lower Aripo',
                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]


Gt_fishType_Deathrate_malefish[2, ,]

#Gt-LA-Female fish predicted death rate
Gt_fishType_Deathrate_femalefish[2,1, ]=as.vector(qmatrix.msm(Multistate_model_best, 
                covariates = list (Parasite_type = "Gt",Fish_population='Lower Aripo',
                                  Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_femalefish[2,2, ]=as.vector(qmatrix.msm(Multistate_model_best, 
            covariates = list (Parasite_type = "Gt", `timeperiod[3,5)`=1,Fish_population='Lower Aripo',
                              Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_femalefish[2,3, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt", `timeperiod[5,7)`=1,Fish_population='Lower Aripo',
                      Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_femalefish[2,4, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt", `timeperiod[7,9)`=1,Fish_population='Lower Aripo',
                      Sex_fish="Female fish"))[1,3])[c(1,3,4)]


Gt_fishType_Deathrate_femalefish[2,5, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt", `timeperiod[9,11)`=1,Fish_population='Lower Aripo',
                      Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_femalefish[2,6, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt", `timeperiod[11,13)`=1,Fish_population='Lower Aripo',
                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_femalefish[2,7, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt", `timeperiod[13,15)`=1,Fish_population='Lower Aripo',
                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_femalefish[2,8, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt", `timeperiod[15,Inf)`=1,Fish_population='Lower Aripo',
                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]


Gt_fishType_Deathrate_femalefish[2, ,]

#Gt-OS-Male fish predicted death rate
Gt_fishType_Deathrate_malefish[3,1, ]=as.vector(qmatrix.msm(Multistate_model_best, 
                covariates = list (Parasite_type = "Gt",Fish_population='Ornamental',
                                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_malefish[3,2, ]=as.vector(qmatrix.msm(Multistate_model_best, 
            covariates = list (Parasite_type = "Gt", `timeperiod[3,5)`=1,Fish_population='Ornamental',
                              Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_malefish[3,3, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt", `timeperiod[5,7)`=1,Fish_population='Ornamental',
                      Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_malefish[3,4, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt", `timeperiod[7,9)`=1,Fish_population='Ornamental',
                      Sex_fish="Male fish"))[1,3])[c(1,3,4)]


Gt_fishType_Deathrate_malefish[3,5, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt", `timeperiod[9,11)`=1,Fish_population='Ornamental',
                      Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_malefish[3,6, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt", `timeperiod[11,13)`=1,Fish_population='Ornamental',
                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_malefish[3,7, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt", `timeperiod[13,15)`=1,Fish_population='Ornamental',
                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_malefish[3,8, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt", `timeperiod[15,Inf)`=1,Fish_population='Ornamental',
                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]


Gt_fishType_Deathrate_malefish[3, ,]

#Gt-OS-Female fish predicted death rate
Gt_fishType_Deathrate_femalefish[3,1, ]=as.vector(qmatrix.msm(Multistate_model_best, 
                covariates = list (Parasite_type = "Gt",Fish_population='Ornamental',
                                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_femalefish[3,2, ]=as.vector(qmatrix.msm(Multistate_model_best, 
            covariates = list (Parasite_type = "Gt", `timeperiod[3,5)`=1,Fish_population='Ornamental',
                              Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_femalefish[3,3, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt", `timeperiod[5,7)`=1,Fish_population='Ornamental',
                      Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_femalefish[3,4, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt", `timeperiod[7,9)`=1,Fish_population='Ornamental',
                      Sex_fish="Female fish"))[1,3])[c(1,3,4)]


Gt_fishType_Deathrate_femalefish[3,5, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gt", `timeperiod[9,11)`=1,Fish_population='Ornamental',
                      Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_femalefish[3,6, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt", `timeperiod[11,13)`=1,Fish_population='Ornamental',
                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_femalefish[3,7, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt", `timeperiod[13,15)`=1,Fish_population='Ornamental',
                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gt_fishType_Deathrate_femalefish[3,8, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gt", `timeperiod[15,Inf)`=1,Fish_population='Ornamental',
                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]


Gt_fishType_Deathrate_femalefish[3, ,]

#Gb-UA-Male fish predicted death rate
Gb_fishType_Deathrate_malefish[1,1, ]=as.vector(qmatrix.msm(Multistate_model_best, 
                covariates = list (Parasite_type = "Gbull",Fish_population='Upper Aripo',
                                  Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_malefish[1,2, ]=as.vector(qmatrix.msm(Multistate_model_best, 
            covariates = list (Parasite_type = "Gbull", `timeperiod[3,5)`=1,Fish_population='Upper Aripo',
                              Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_malefish[1,3, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gbull", `timeperiod[5,7)`=1,Fish_population='Upper Aripo',
                      Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_malefish[1,4, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gbull", `timeperiod[7,9)`=1,Fish_population='Upper Aripo',
                      Sex_fish="Male fish"))[1,3])[c(1,3,4)]


Gb_fishType_Deathrate_malefish[1,5, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gbull", `timeperiod[9,11)`=1,Fish_population='Upper Aripo',
                      Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_malefish[1,6, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gbull", `timeperiod[11,13)`=1,Fish_population='Upper Aripo',
                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_malefish[1,7, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gbull", `timeperiod[13,15)`=1,Fish_population='Upper Aripo',
                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_malefish[1,8, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gbull", `timeperiod[15,Inf)`=1,Fish_population='Upper Aripo',
                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]


Gb_fishType_Deathrate_malefish[1, ,]

#Gb-UA-Female fish predicted death rate
Gb_fishType_Deathrate_femalefish[1,1, ]=as.vector(qmatrix.msm(Multistate_model_best, 
                covariates = list (Parasite_type = "Gbull",Fish_population='Upper Aripo',
                                  Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_femalefish[1,2, ]=as.vector(qmatrix.msm(Multistate_model_best, 
            covariates = list (Parasite_type = "Gbull", `timeperiod[3,5)`=1,Fish_population='Upper Aripo',
                              Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_femalefish[1,3, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gbull", `timeperiod[5,7)`=1,Fish_population='Upper Aripo',
                      Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_femalefish[1,4, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gbull", `timeperiod[7,9)`=1,Fish_population='Upper Aripo',
                      Sex_fish="Female fish"))[1,3])[c(1,3,4)]


Gb_fishType_Deathrate_femalefish[1,5, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gbull", `timeperiod[9,11)`=1,Fish_population='Upper Aripo',
                      Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_femalefish[1,6, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gbull", `timeperiod[11,13)`=1,Fish_population='Upper Aripo',
                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_femalefish[1,7, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gbull", `timeperiod[13,15)`=1,Fish_population='Upper Aripo',
                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_femalefish[1,8, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gbull", `timeperiod[15,Inf)`=1,Fish_population='Upper Aripo',
                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]


Gb_fishType_Deathrate_femalefish[1, ,]

#Gb-LA-Male fish predicted death rate
Gb_fishType_Deathrate_malefish[2,1, ]=as.vector(qmatrix.msm(Multistate_model_best, 
                covariates = list (Parasite_type = "Gbull",Fish_population='Lower Aripo',
                                  Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_malefish[2,2, ]=as.vector(qmatrix.msm(Multistate_model_best, 
            covariates = list (Parasite_type = "Gbull", `timeperiod[3,5)`=1,Fish_population='Lower Aripo',
                              Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_malefish[2,3, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gbull", `timeperiod[5,7)`=1,Fish_population='Lower Aripo',
                      Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_malefish[2,4, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gbull", `timeperiod[7,9)`=1,Fish_population='Lower Aripo',
                      Sex_fish="Male fish"))[1,3])[c(1,3,4)]


Gb_fishType_Deathrate_malefish[2,5, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gbull", `timeperiod[9,11)`=1,Fish_population='Lower Aripo',
                      Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_malefish[2,6, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gbull", `timeperiod[11,13)`=1,Fish_population='Lower Aripo',
                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_malefish[2,7, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gbull", `timeperiod[13,15)`=1,Fish_population='Lower Aripo',
                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_malefish[2,8, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gbull", `timeperiod[15,Inf)`=1,Fish_population='Lower Aripo',
                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]


Gb_fishType_Deathrate_malefish[2, ,]

#Gb-LA-Female fish predicted death rate
Gb_fishType_Deathrate_femalefish[2,1, ]=as.vector(qmatrix.msm(Multistate_model_best, 
                covariates = list (Parasite_type = "Gbull",Fish_population='Lower Aripo',
                                  Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_femalefish[2,2, ]=as.vector(qmatrix.msm(Multistate_model_best, 
            covariates = list (Parasite_type = "Gbull", `timeperiod[3,5)`=1,Fish_population='Lower Aripo',
                              Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_femalefish[2,3, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gbull", `timeperiod[5,7)`=1,Fish_population='Lower Aripo',
                      Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_femalefish[2,4, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gbull", `timeperiod[7,9)`=1,Fish_population='Lower Aripo',
                      Sex_fish="Female fish"))[1,3])[c(1,3,4)]


Gb_fishType_Deathrate_femalefish[2,5, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gbull", `timeperiod[9,11)`=1,Fish_population='Lower Aripo',
                      Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_femalefish[2,6, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gbull", `timeperiod[11,13)`=1,Fish_population='Lower Aripo',
                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_femalefish[2,7, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gbull", `timeperiod[13,15)`=1,Fish_population='Lower Aripo',
                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_femalefish[2,8, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gbull", `timeperiod[15,Inf)`=1,Fish_population='Lower Aripo',
                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]


Gb_fishType_Deathrate_femalefish[2, ,]

#Gb-OS-Male fish predicted death rate
Gb_fishType_Deathrate_malefish[3,1, ]=as.vector(qmatrix.msm(Multistate_model_best, 
                covariates = list (Parasite_type = "Gbull",Fish_population='Ornamental',
                                  Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_malefish[3,2, ]=as.vector(qmatrix.msm(Multistate_model_best, 
            covariates = list (Parasite_type = "Gbull", `timeperiod[3,5)`=1,Fish_population='Ornamental',
                              Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_malefish[3,3, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gbull", `timeperiod[5,7)`=1,Fish_population='Ornamental',
                      Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_malefish[3,4, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gbull", `timeperiod[7,9)`=1,Fish_population='Ornamental',
                      Sex_fish="Male fish"))[1,3])[c(1,3,4)]


Gb_fishType_Deathrate_malefish[3,5, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gbull", `timeperiod[9,11)`=1,Fish_population='Ornamental',
                      Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_malefish[3,6, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gbull", `timeperiod[11,13)`=1,Fish_population='Ornamental',
                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_malefish[3,7, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gbull", `timeperiod[13,15)`=1,Fish_population='Ornamental',
                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_malefish[3,8, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gbull", `timeperiod[15,Inf)`=1,Fish_population='Ornamental',
                   Sex_fish="Male fish"))[1,3])[c(1,3,4)]


Gb_fishType_Deathrate_malefish[3, ,]

#Gb-OS-Female fish predicted death rate
Gb_fishType_Deathrate_femalefish[3,1, ]=as.vector(qmatrix.msm(Multistate_model_best, 
                covariates = list (Parasite_type = "Gbull",Fish_population='Ornamental',
                                  Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_femalefish[3,2, ]=as.vector(qmatrix.msm(Multistate_model_best, 
            covariates = list (Parasite_type = "Gbull", `timeperiod[3,5)`=1,Fish_population='Ornamental',
                              Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_femalefish[3,3, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gbull", `timeperiod[5,7)`=1,Fish_population='Ornamental',
                      Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_femalefish[3,4, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gbull", `timeperiod[7,9)`=1,Fish_population='Ornamental',
                      Sex_fish="Female fish"))[1,3])[c(1,3,4)]


Gb_fishType_Deathrate_femalefish[3,5, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Parasite_type = "Gbull", `timeperiod[9,11)`=1,Fish_population='Ornamental',
                      Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_femalefish[3,6, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gbull", `timeperiod[11,13)`=1,Fish_population='Ornamental',
                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_femalefish[3,7, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gbull", `timeperiod[13,15)`=1,Fish_population='Ornamental',
                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]

Gb_fishType_Deathrate_femalefish[3,8, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Parasite_type = "Gbull", `timeperiod[15,Inf)`=1,Fish_population='Ornamental',
                   Sex_fish="Female fish"))[1,3])[c(1,3,4)]


Gb_fishType_Deathrate_femalefish[3, ,]

Recoveryrate_fishsize=array(dim=c(6,8,3)) #estimates and its 95% confidence intervals

fish_sizes=c(11,14,17,20,23,26)
for(i in 1:length(fish_sizes)){

Recoveryrate_fishsize[i,1, ]=as.vector(qmatrix.msm(Multistate_model_best, 
                covariates = list (Fish_size=fish_sizes[i]))[1,2])[c(1,3,4)]

Recoveryrate_fishsize[i,2, ]=as.vector(qmatrix.msm(Multistate_model_best, 
            covariates = list (Fish_size=fish_sizes[i], `timeperiod[3,5)`=1))[1,2])[c(1,3,4)]

Recoveryrate_fishsize[i,3, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Fish_size=fish_sizes[i], `timeperiod[5,7)`=1))[1,2])[c(1,3,4)]

Recoveryrate_fishsize[i,4, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Fish_size=fish_sizes[i], `timeperiod[7,9)`=1))[1,2])[c(1,3,4)]

Recoveryrate_fishsize[i,5, ]=as.vector(qmatrix.msm(Multistate_model_best, 
    covariates = list (Fish_size=fish_sizes[i], `timeperiod[9,11)`=1))[1,2])[c(1,3,4)]

Recoveryrate_fishsize[i,6, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Fish_size=fish_sizes[i], `timeperiod[11,13)`=1))[1,2])[c(1,3,4)]

Recoveryrate_fishsize[i,7, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Fish_size=fish_sizes[i], `timeperiod[13,15)`=1))[1,2])[c(1,3,4)]

Recoveryrate_fishsize[i,8, ]=as.vector(qmatrix.msm(Multistate_model_best, 
 covariates = list (Fish_size=fish_sizes[i], `timeperiod[15,Inf)`=1))[1,2])[c(1,3,4)]

          }

time=as.character(c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"))

#Gt3-fish groups
Gt3_Death_Rates=as.data.frame(rbind(Gt3_fishType_Deathrate[1, ,],Gt3_fishType_Deathrate[2, ,],Gt3_fishType_Deathrate[3, ,]))
names(Gt3_Death_Rates)=c("Estimate","Lower_CI","Upper_CI")
Gt3_fish= c(rep("Gt3-UA",8),rep("Gt3-LA",8),rep("Gt3-OS",8))

Gt3_Death_Rates$Time_interval=rep(time,3)
Gt3_Death_Rates$Gt3_fish=Gt3_fish


#Gt-fish groups
Gt_Death_Rates=as.data.frame(rbind(Gt_fishType_Deathrate[1, ,],Gt_fishType_Deathrate[2, ,],Gt_fishType_Deathrate[3, ,]))
names(Gt_Death_Rates)=c("Estimate","Lower_CI","Upper_CI")
Gt_fish= c(rep("Gt-UA",8),rep("Gt-LA",8),rep("Gt-OS",8))

Gt_Death_Rates$Time_interval=rep(time,3)
Gt_Death_Rates$Gt_fish=Gt_fish


#Gb-fish groups
Gb_Death_Rates=as.data.frame(rbind(Gb_fishType_Deathrate[1, ,],Gb_fishType_Deathrate[2, ,],Gb_fishType_Deathrate[3, ,]))
names(Gb_Death_Rates)=c("Estimate","Lower_CI","Upper_CI")
Gb_fish= c(rep("Gb-UA",8),rep("Gb-LA",8),rep("Gb-OS",8))

Gb_Death_Rates$Time_interval=rep(time,3)
Gb_Death_Rates$Gb_fish=Gb_fish


write.csv(Gt3_Death_Rates,"Gt3_Death_Rates.csv")
write.csv(Gt_Death_Rates,"Gt_Death_Rates.csv")
write.csv(Gb_Death_Rates,"Gb_Death_Rates.csv")

Gt3_Death_Rates

o<-par(mar=c(0,4,2,2))#Run this before the code below

nf<-layout(matrix(1:6, nrow=3,ncol=2))

    
par(o)#
o<-par(mar=c(0,4,2,2)) 

#first half of plot
plot(Gt3_fishType_Deathrate_malefish[1, ,][,1],type="b",col="blue",lwd=2,xaxt = "n",ylab="",xlab="",pch=24,cex.axis=1.2,
     ylim=c(0,2.2))
arrows(x0=1:8, y0=Gt3_fishType_Deathrate_malefish[1, ,][,2], x1=1:8, y1=Gt3_fishType_Deathrate_malefish[1, ,][,3], code=3, 
      angle=90, length=0.05,col ="blue",lwd=2)

lines(Gt_fishType_Deathrate_malefish[1, ,][,1],type="b",col="red",lwd=2,pch=19)
arrows(x0=1:8, y0=Gt_fishType_Deathrate_malefish[1, ,][,2], x1=1:8, y1=Gt_fishType_Deathrate_malefish[1, ,][,3], code=3, 
      angle=90, length=0.05,col ="red",lwd=2)

lines(Gb_fishType_Deathrate_malefish[1, ,][,1],type="b",col="green",lwd=2,pch=3)
arrows(x0=1:8, y0=Gb_fishType_Deathrate_malefish[1, ,][,2], x1=1:8, y1=Gb_fishType_Deathrate_malefish[1, ,][,3], code=3, 
      angle=90, length=0.05,col ="green",lwd=2)

text(3,2.1,"Male UA fish stock",cex=1.5,lwd=3)


o<-par(mar=c(0,4,0,2))
plot(Gt3_fishType_Deathrate_malefish[2, ,][,1],type="b",col="blue",lwd=2,xaxt = "n",ylab="",xlab="",
     ylim=c(0,2.2),pch=24,cex.lab=1.4, cex.axis=1.2)
arrows(x0=1:8, y0=Gt3_fishType_Deathrate_malefish[2, ,][,2], x1=1:8, y1=Gt3_fishType_Deathrate_malefish[2, ,][,3], code=3, 
      angle=90, length=0.05,col ="blue",lwd=2)

lines(Gt_fishType_Deathrate_malefish[2, ,][,1],type="b",col="red",lwd=2,pch=19)
arrows(x0=1:8, y0=Gt_fishType_Deathrate_malefish[2, ,][,2], x1=1:8, y1=Gt_fishType_Deathrate_malefish[2, ,][,3], code=3, 
      angle=90, length=0.05,col ="red",lwd=2)

lines(Gb_fishType_Deathrate_malefish[2, ,][,1],type="b",col="green",lwd=2,pch=3)
arrows(x0=1:8, y0=Gb_fishType_Deathrate_malefish[2, ,][,2], x1=1:8, y1=Gb_fishType_Deathrate_malefish[2, ,][,3], code=3, 
      angle=90, length=0.05,col ="green",lwd=2)

legend(x = .9,y=1.5,inset = 0,
        legend = c("Gt3","Gt","Gb"), 
        col=c("blue","red","green"), cex=1.35, horiz = TRUE,pt.cex = 1,
       box.lwd = 2,fill=c("blue","red","green"))




text(3,2.1,"Male LA fish stock",cex=1.5,lwd=3)


mtext(text = "Rate of host mortality",
      side = 2,#side 2 = left
      line = 3,cex=.9)

par(mar=c(4,4,0,2))

plot(Gt3_fishType_Deathrate_malefish[3, ,][,1],type="b",col="blue",lwd=2,xaxt = "n",ylab="",xlab="",
     pch=24,cex.lab=1.4, cex.axis=1.2,ylim=c(0,2.2))
arrows(x0=1:8, y0=Gt3_fishType_Deathrate_malefish[3, ,][,2], x1=1:8, y1=Gt3_fishType_Deathrate_malefish[3, ,][,3], code=3, 
      angle=90, length=0.05,col ="blue",lwd=2)


lines(Gt_fishType_Deathrate_malefish[3, ,][,1],type="b",col="red",lwd=2,pch=19)
arrows(x0=1:8, y0=Gt_fishType_Deathrate_malefish[3, ,][,2], x1=1:8, y1=Gt_fishType_Deathrate_malefish[3, ,][,3], code=3, 
      angle=90, length=0.05,col ="red",lwd=2)

lines(Gb_fishType_Deathrate_malefish[3, ,][,1],type="b",col="green",lwd=2,pch=3)
arrows(x0=1:8, y0=Gb_fishType_Deathrate_malefish[3, ,][,2], x1=1:8, y1=Gb_fishType_Deathrate_malefish[3, ,][,3], code=3, 
      angle=90, length=0.05,col ="green",lwd=2)

axis(1, at=1:8, labels=c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"))
text(3,2.1,"Male OS fish stock",cex=1.5,lwd=3)

mtext(text = "Observed time intervals (in days)",
      side = 1,#side 1 = bottom
      line = 3,cex=.9)



#second half of plot
o<-par(mar=c(0,4,2,2))

plot(Gt3_fishType_Deathrate_femalefish[1, ,][,1],type="b",col="blue",lwd=2,xaxt = "n",ylab="",xlab="",pch=24,cex.axis=1.2,
     ylim=c(0,2.2))
arrows(x0=1:8, y0=Gt3_fishType_Deathrate_femalefish[1, ,][,2], x1=1:8, y1=Gt3_fishType_Deathrate_femalefish[1, ,][,3], code=3, 
      angle=90, length=0.05,col ="blue",lwd=2)

lines(Gt_fishType_Deathrate_femalefish[1, ,][,1],type="b",col="red",lwd=2,pch=19)
arrows(x0=1:8, y0=Gt_fishType_Deathrate_femalefish[1, ,][,2], x1=1:8, y1=Gt_fishType_Deathrate_femalefish[1, ,][,3], code=3, 
      angle=90, length=0.05,col ="red",lwd=2)

lines(Gb_fishType_Deathrate_femalefish[1, ,][,1],type="b",col="green",lwd=2,pch=3)
arrows(x0=1:8, y0=Gb_fishType_Deathrate_femalefish[1, ,][,2], x1=1:8, y1=Gb_fishType_Deathrate_femalefish[1, ,][,3], code=3, 
      angle=90, length=0.05,col ="green",lwd=2)

text(3,2.1,"Female UA fish stock",cex=1.5,lwd=3)


o<-par(mar=c(0,4,0,2))
plot(Gt3_fishType_Deathrate_femalefish[2, ,][,1],type="b",col="blue",lwd=2,xaxt = "n",ylab="",xlab="",
     ylim=c(0,2.2),pch=24,cex.lab=1.4, cex.axis=1.2)
arrows(x0=1:8, y0=Gt3_fishType_Deathrate_femalefish[2, ,][,2], x1=1:8, y1=Gt3_fishType_Deathrate_femalefish[2, ,][,3], code=3, 
      angle=90, length=0.05,col ="blue",lwd=2)

lines(Gt_fishType_Deathrate_femalefish[2, ,][,1],type="b",col="red",lwd=2,pch=19)
arrows(x0=1:8, y0=Gt_fishType_Deathrate_femalefish[2, ,][,2], x1=1:8, y1=Gt_fishType_Deathrate_femalefish[2, ,][,3], code=3, 
      angle=90, length=0.05,col ="red",lwd=2)

lines(Gb_fishType_Deathrate_femalefish[2, ,][,1],type="b",col="green",lwd=2,pch=3)
arrows(x0=1:8, y0=Gb_fishType_Deathrate_femalefish[2, ,][,2], x1=1:8, y1=Gb_fishType_Deathrate_femalefish[2, ,][,3], code=3, 
      angle=90, length=0.05,col ="green",lwd=2)

legend(x = .9,y=1.5,,inset = 0,
        legend = c("Gt3","Gt","Gb"), 
        col=c("blue","red","green"), cex=1.35, horiz = TRUE,pt.cex = 1,
       box.lwd = 2,fill=c("blue","red","green"))

text(3,2.1,"Female LA fish stock",cex=1.5,lwd=3)

#mtext(text = "Predicted host mortality rate",
 #     side = 2,#side 2 = left
 #     line = 3,cex=.9)

par(mar=c(4,4,0,2))

plot(Gt3_fishType_Deathrate_femalefish[3, ,][,1],type="b",col="blue",lwd=2,xaxt = "n",ylab="",xlab="",
     pch=24,cex.lab=1.4, cex.axis=1.2,ylim=c(0,2.2))
arrows(x0=1:8, y0=Gt3_fishType_Deathrate_femalefish[3, ,][,2], x1=1:8, y1=Gt3_fishType_Deathrate_femalefish[3, ,][,3], code=3, 
      angle=90, length=0.05,col ="blue",lwd=2)


lines(Gt_fishType_Deathrate_femalefish[3, ,][,1],type="b",col="red",lwd=2,pch=19)
arrows(x0=1:8, y0=Gt_fishType_Deathrate_femalefish[3, ,][,2], x1=1:8, y1=Gt_fishType_Deathrate_femalefish[3, ,][,3], code=3, 
      angle=90, length=0.05,col ="red",lwd=2)

lines(Gb_fishType_Deathrate_femalefish[3, ,][,1],type="b",col="green",lwd=2,pch=3)
arrows(x0=1:8, y0=Gb_fishType_Deathrate_femalefish[3, ,][,2], x1=1:8, y1=Gb_fishType_Deathrate_femalefish[3, ,][,3], code=3, 
      angle=90, length=0.05,col ="green",lwd=2)

axis(1, at=1:8, labels=c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"))
text(3,2.1,"Female OS fish stock",cex=1.5,lwd=3)

mtext(text = "Observed time intervals (in days)",
      side = 1,#side 1 = bottom
      line = 3,cex=.9)

par(o)

o<-par(mar=c(0,4,2,2))#Run this before the code below

nf<-layout(matrix(1:6, nrow=3,ncol=2))

 
par(o)#
o<-par(mar=c(0,4,2,2)) 

#first half of plot
plot(Gt3_fishType_Deathrate_malefish[1, ,][,1],type="b",col="grey40",lwd=3,xaxt = "n",ylab="",xlab="",cex.axis=1.2,
     ylim=c(0,2.2))
arrows(x0=1:8, y0=Gt3_fishType_Deathrate_malefish[1, ,][,2], x1=1:8, y1=Gt3_fishType_Deathrate_malefish[1, ,][,3], code=3, 
      angle=90, length=0.05,col ="grey40",lwd=3)

lines(Gt_fishType_Deathrate_malefish[1, ,][,1],type="b",col="grey0",lwd=3)
arrows(x0=1:8, y0=Gt_fishType_Deathrate_malefish[1, ,][,2], x1=1:8, y1=Gt_fishType_Deathrate_malefish[1, ,][,3], code=3, 
      angle=90, length=0.05,col ="grey0",lwd=3)

lines(Gb_fishType_Deathrate_malefish[1, ,][,1],type="b",col="grey80",lwd=3)
arrows(x0=1:8, y0=Gb_fishType_Deathrate_malefish[1, ,][,2], x1=1:8, y1=Gb_fishType_Deathrate_malefish[1, ,][,3], code=3, 
      angle=90, length=0.05,col ="grey80",lwd=3)

text(3,2.1,"Male UA fish stock",cex=1.5,lwd=3)


o<-par(mar=c(0,4,0,2))
plot(Gt3_fishType_Deathrate_malefish[2, ,][,1],type="b",col="grey40",lwd=3,xaxt = "n",ylab="",xlab="",
     ylim=c(0,2.2),cex.lab=1.4, cex.axis=1.2)
arrows(x0=1:8, y0=Gt3_fishType_Deathrate_malefish[2, ,][,2], x1=1:8, y1=Gt3_fishType_Deathrate_malefish[2, ,][,3], code=3, 
      angle=90, length=0.05,col ="grey40",lwd=3)

lines(Gt_fishType_Deathrate_malefish[2, ,][,1],type="b",col="grey0",lwd=3)
arrows(x0=1:8, y0=Gt_fishType_Deathrate_malefish[2, ,][,2], x1=1:8, y1=Gt_fishType_Deathrate_malefish[2, ,][,3], code=3, 
      angle=90, length=0.05,col ="grey0",lwd=3)

lines(Gb_fishType_Deathrate_malefish[2, ,][,1],type="b",col="grey80",lwd=3)
arrows(x0=1:8, y0=Gb_fishType_Deathrate_malefish[2, ,][,2], x1=1:8, y1=Gb_fishType_Deathrate_malefish[2, ,][,3], code=3, 
      angle=90, length=0.05,col ="grey80",lwd=3)

legend(x = .9,y=1.5,,inset = 0,
        legend = c("Gb","Gt3","Gt"), 
        col=c("grey80","grey40","grey0"), cex=1.35, horiz = TRUE,pt.cex = 1,
      box.lwd = 2,
       fill=c("grey80","grey40","grey0"))

text(3,2.1,"Male LA fish stock",cex=1.5,lwd=3)


mtext(text = "Rate of host mortality",
      side = 2,#side 2 = left
      line = 3,cex=.9)

par(mar=c(4,4,0,2))

plot(Gt3_fishType_Deathrate_malefish[3, ,][,1],type="b",col="grey40",lwd=3,xaxt = "n",ylab="",xlab=""
     ,cex.lab=1.4, cex.axis=1.2,ylim=c(0,2.2))
arrows(x0=1:8, y0=Gt3_fishType_Deathrate_malefish[3, ,][,2], x1=1:8, y1=Gt3_fishType_Deathrate_malefish[3, ,][,3], code=3, 
      angle=90, length=0.05,col ="grey40",lwd=3)


lines(Gt_fishType_Deathrate_malefish[3, ,][,1],type="b",col="grey0",lwd=3)
arrows(x0=1:8, y0=Gt_fishType_Deathrate_malefish[3, ,][,2], x1=1:8, y1=Gt_fishType_Deathrate_malefish[3, ,][,3], code=3, 
      angle=90, length=0.05,col ="grey0",lwd=3)

lines(Gb_fishType_Deathrate_malefish[3, ,][,1],type="b",col="grey80",lwd=3)
arrows(x0=1:8, y0=Gb_fishType_Deathrate_malefish[3, ,][,2], x1=1:8, y1=Gb_fishType_Deathrate_malefish[3, ,][,3], code=3, 
      angle=90, length=0.05,col ="grey80",lwd=3)

axis(1, at=1:8, labels=c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"))
text(3,2.1,"Male OS fish stock",cex=1.5,lwd=3)

mtext(text = "Observed time intervals (in days)",
      side = 1,#side 1 = bottom
      line = 3,cex=.9)



#second half of plot
o<-par(mar=c(0,4,2,2))

plot(Gt3_fishType_Deathrate_femalefish[1, ,][,1],type="b",col="grey40",lwd=3,xaxt = "n",ylab="",xlab="",cex.axis=1.2,
     ylim=c(0,2.2))
arrows(x0=1:8, y0=Gt3_fishType_Deathrate_femalefish[1, ,][,2], x1=1:8, y1=Gt3_fishType_Deathrate_femalefish[1, ,][,3], code=3, 
      angle=90, length=0.05,col ="grey40",lwd=3)

lines(Gt_fishType_Deathrate_femalefish[1, ,][,1],type="b",col="grey0",lwd=3)
arrows(x0=1:8, y0=Gt_fishType_Deathrate_femalefish[1, ,][,2], x1=1:8, y1=Gt_fishType_Deathrate_femalefish[1, ,][,3], code=3, 
      angle=90, length=0.05,col ="grey0",lwd=3)

lines(Gb_fishType_Deathrate_femalefish[1, ,][,1],type="b",col="grey80",lwd=3)
arrows(x0=1:8, y0=Gb_fishType_Deathrate_femalefish[1, ,][,2], x1=1:8, y1=Gb_fishType_Deathrate_femalefish[1, ,][,3], code=3, 
      angle=90, length=0.05,col ="grey80",lwd=3)

text(3,2.1,"Female UA fish stock",cex=1.5,lwd=3)


o<-par(mar=c(0,4,0,2))
plot(Gt3_fishType_Deathrate_femalefish[2, ,][,1],type="b",col="grey40",lwd=3,xaxt = "n",ylab="",xlab="",
     ylim=c(0,2.2),cex.lab=1.4, cex.axis=1.2)
arrows(x0=1:8, y0=Gt3_fishType_Deathrate_femalefish[2, ,][,2], x1=1:8, y1=Gt3_fishType_Deathrate_femalefish[2, ,][,3], code=3, 
      angle=90, length=0.05,col ="grey40",lwd=3)

lines(Gt_fishType_Deathrate_femalefish[2, ,][,1],type="b",col="grey0",lwd=3)
arrows(x0=1:8, y0=Gt_fishType_Deathrate_femalefish[2, ,][,2], x1=1:8, y1=Gt_fishType_Deathrate_femalefish[2, ,][,3], code=3, 
      angle=90, length=0.05,col ="grey0",lwd=3)

lines(Gb_fishType_Deathrate_femalefish[2, ,][,1],type="b",col="grey80",lwd=3)
arrows(x0=1:8, y0=Gb_fishType_Deathrate_femalefish[2, ,][,2], x1=1:8, y1=Gb_fishType_Deathrate_femalefish[2, ,][,3], code=3, 
      angle=90, length=0.05,col ="grey80",lwd=3)

legend(x = .9,y=1.5,,inset = 0,
        legend = c("Gb","Gt3","Gt"), 
        col=c("grey80","grey40","grey0"), cex=1.35, horiz = TRUE,pt.cex = 1,
     box.lwd = 2,fill=c("grey80","grey40","grey0"))



text(3,2.1,"Female LA fish stock",cex=1.5,lwd=3)

#mtext(text = "Predicted host mortality rate",
 #     side = 2,#side 2 = left
 #     line = 3,cex=.9)

par(mar=c(4,4,0,2))

plot(Gt3_fishType_Deathrate_femalefish[3, ,][,1],type="b",col="grey40",lwd=3,xaxt = "n",ylab="",xlab=""
     ,cex.lab=1.4, cex.axis=1.2,ylim=c(0,2.2))
arrows(x0=1:8, y0=Gt3_fishType_Deathrate_femalefish[3, ,][,2], x1=1:8, y1=Gt3_fishType_Deathrate_femalefish[3, ,][,3], code=3, 
      angle=90, length=0.05,col ="grey40",lwd=3)


lines(Gt_fishType_Deathrate_femalefish[3, ,][,1],type="b",col="grey0",lwd=3)
arrows(x0=1:8, y0=Gt_fishType_Deathrate_femalefish[3, ,][,2], x1=1:8, y1=Gt_fishType_Deathrate_femalefish[3, ,][,3], code=3, 
      angle=90, length=0.05,col ="grey0",lwd=3)

lines(Gb_fishType_Deathrate_femalefish[3, ,][,1],type="b",col="grey80",lwd=3)
arrows(x0=1:8, y0=Gb_fishType_Deathrate_femalefish[3, ,][,2], x1=1:8, y1=Gb_fishType_Deathrate_femalefish[3, ,][,3], code=3, 
      angle=90, length=0.05,col ="grey80",lwd=3)

axis(1, at=1:8, labels=c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"))
text(3,2.1,"Female OS fish stock",cex=1.5,lwd=3)

mtext(text = "Observed time intervals (in days)",
      side = 1,#side 1 = bottom
      line = 3,cex=.9)

par(o)

o<-par(mar=c(0,4,2,2))#Run this before the code below

nf<-layout(matrix(1:6, nrow=3,ncol=2))

par(o)#
o<-par(mar=c(0,4,2,2))

#first half of plot
plot(Recoveryrate_fishsize[1, ,][,1],type="b",col="blue",lwd=3,xaxt = "n",ylab="",xlab="",
     ylim=c(0,.81),pch=24,cex.lab=1.4, cex.axis=1.2)
arrows(x0=1:8, y0=Recoveryrate_fishsize[1, ,][,2], x1=1:8, y1=Recoveryrate_fishsize[1, ,][,3], code=3, 
      angle=90, length=0.05,col ="blue",lwd=2)
text(2.5,.81,"Fish size: 11mm",cex=1.5,lwd=3)

o<-par(mar=c(0,4,0,2))
plot(Recoveryrate_fishsize[2, ,][,1],type="b",col="blue",lwd=3,pch=24,xaxt = "n",ylab="",xlab="",ylim=c(0,.81))
arrows(x0=1:8, y0=Recoveryrate_fishsize[2, ,][,2], x1=1:8, y1=Recoveryrate_fishsize[2, ,][,3], code=3, 
      angle=90, length=0.05,col ="blue",lwd=2)
text(2.5,.81,"Fish size: 14mm",cex=1.5,lwd=3)

mtext(text = "Rate of host recovery",
      side = 2,#side left
      line = 3,cex=.9)

par(mar=c(4,4,0,2))
plot(Recoveryrate_fishsize[3, ,][,1],type="b",col="blue",lwd=3,pch=24,xaxt = "n",ylab="",
     xlab="",ylim=c(0,.81))
arrows(x0=1:8, y0=Recoveryrate_fishsize[3, ,][,2], x1=1:8, y1=Recoveryrate_fishsize[3, ,][,3], code=3, 
      angle=90, length=0.05,col ="blue",lwd=2)
axis(1, at=1:8, labels=c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"))

text(2.5,.81,"Fish size: 17mm",cex=1.5,lwd=3)
mtext(text = "Observed time intervals (in days)",
      side = 1,#side 1 = bottom
      line = 3,cex=.9)

#second half of plot
o<-par(mar=c(0,4,2,2))
plot(Recoveryrate_fishsize[4, ,][,1],type="b",col="blue",lwd=3,pch=24,xaxt = "n",ylab="",xlab="",ylim=c(0,.81))
arrows(x0=1:8, y0=Recoveryrate_fishsize[4, ,][,2], x1=1:8, y1=Recoveryrate_fishsize[4, ,][,3], code=3, 
      angle=90, length=0.05,col ="blue",lwd=2)
text(2.5,.81,"Fish size: 20mm",cex=1.5,lwd=3)

o<-par(mar=c(0,4,0,2))
plot(Recoveryrate_fishsize[5, ,][,1],type="b",col="blue",lwd=3,pch=24,xaxt = "n",ylab="",xlab="",ylim=c(0,.81))
arrows(x0=1:8, y0=Recoveryrate_fishsize[5, ,][,2], x1=1:8, y1=Recoveryrate_fishsize[5, ,][,3], code=3, 
      angle=90, length=0.05,col ="blue",lwd=2)
text(2.5,.81,"Fish size: 23mm",cex=1.5,lwd=3)

par(mar=c(4,4,0,2))
plot(Recoveryrate_fishsize[6, ,][,1],type="b",col="blue",lwd=3,pch=24,xaxt = "n",ylab="",
     xlab="",ylim=c(0,.81))
arrows(x0=1:8, y0=Recoveryrate_fishsize[6, ,][,2], x1=1:8, y1=Recoveryrate_fishsize[6, ,][,3], code=3, 
      angle=90, length=0.05,col ="blue",lwd=2)
axis(1, at=1:8, labels=c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"))

text(2.5,.81,"Fish size: 26mm",cex=1.5,lwd=3)
mtext(text = "Observed time intervals (in days)",
      side = 1,#side 1 = bottom
      line = 3,cex=.9)



par(o)

o<-par(mar=c(0,4,2,2))#Run this before the code below

nf<-layout(matrix(1:6, nrow=3,ncol=2))

par(o)#
o<-par(mar=c(0,4,2,2))

#first half of plot
plot(Recoveryrate_fishsize[1, ,][,1],type="b",col="black",lwd=3,xaxt = "n",ylab="",xlab="",
     ylim=c(0,.81),pch=24,cex.lab=1.4, cex.axis=1.2)
arrows(x0=1:8, y0=Recoveryrate_fishsize[1, ,][,2], x1=1:8, y1=Recoveryrate_fishsize[1, ,][,3], code=3, 
      angle=90, length=0.05,col ="black",lwd=2)
text(2.5,.81,"Fish size: 11mm",cex=1.5,lwd=3)

o<-par(mar=c(0,4,0,2))
plot(Recoveryrate_fishsize[2, ,][,1],type="b",col="black",lwd=3,pch=24,xaxt = "n",ylab="",xlab="",ylim=c(0,.81))
arrows(x0=1:8, y0=Recoveryrate_fishsize[2, ,][,2], x1=1:8, y1=Recoveryrate_fishsize[2, ,][,3], code=3, 
      angle=90, length=0.05,col ="black",lwd=2)
text(2.5,.81,"Fish size: 14mm",cex=1.5,lwd=3)

mtext(text = "Rate of host recovery",
      side = 2,#side left
      line = 3,cex=.9)

par(mar=c(4,4,0,2))
plot(Recoveryrate_fishsize[3, ,][,1],type="b",col="black",lwd=3,pch=24,xaxt = "n",ylab="",
     xlab="",ylim=c(0,.81))
arrows(x0=1:8, y0=Recoveryrate_fishsize[3, ,][,2], x1=1:8, y1=Recoveryrate_fishsize[3, ,][,3], code=3, 
      angle=90, length=0.05,col ="black",lwd=2)
axis(1, at=1:8, labels=c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"))

text(2.5,.81,"Fish size: 17mm",cex=1.5,lwd=3)
mtext(text = "Observed time intervals (in days)",
      side = 1,#side 1 = bottom
      line = 3,cex=.9)

#second half of plot
o<-par(mar=c(0,4,2,2))
plot(Recoveryrate_fishsize[4, ,][,1],type="b",col="black",lwd=3,pch=24,xaxt = "n",ylab="",xlab="",ylim=c(0,.81))
arrows(x0=1:8, y0=Recoveryrate_fishsize[4, ,][,2], x1=1:8, y1=Recoveryrate_fishsize[4, ,][,3], code=3, 
      angle=90, length=0.05,col ="black",lwd=2)
text(2.5,.81,"Fish size: 20mm",cex=1.5,lwd=3)

o<-par(mar=c(0,4,0,2))
plot(Recoveryrate_fishsize[5, ,][,1],type="b",col="black",lwd=3,pch=24,xaxt = "n",ylab="",xlab="",ylim=c(0,.81))
arrows(x0=1:8, y0=Recoveryrate_fishsize[5, ,][,2], x1=1:8, y1=Recoveryrate_fishsize[5, ,][,3], code=3, 
      angle=90, length=0.05,col ="black",lwd=2)
text(2.5,.81,"Fish size: 23mm",cex=1.5,lwd=3)

par(mar=c(4,4,0,2))
plot(Recoveryrate_fishsize[6, ,][,1],type="b",col="black",lwd=3,pch=24,xaxt = "n",ylab="",
     xlab="",ylim=c(0,.81))
arrows(x0=1:8, y0=Recoveryrate_fishsize[6, ,][,2], x1=1:8, y1=Recoveryrate_fishsize[6, ,][,3], code=3, 
      angle=90, length=0.05,col ="black",lwd=2)
axis(1, at=1:8, labels=c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"))

text(2.5,.81,"Fish size: 26mm",cex=1.5,lwd=3)
mtext(text = "Observed time intervals (in days)",
      side = 1,#side 1 = bottom
      line = 3,cex=.9)



par(o)


q12_rates=data.frame(matrix(NA,nrow=8,ncol=3))#estimates and its 95% confidence intervals
q13_rates=data.frame(matrix(NA,nrow=8,ncol=3))

rownames(q12_rates)=rownames(q13_rates)=c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17")
colnames(q12_rates)=colnames(q13_rates)=c("estimate","lower_CI","upper_CI")

q12_rates[1,]<-as.vector(qmatrix.msm(Multistate_model_best)[1,2])[c(1,3,4)]
q12_rates[2,]<-as.vector(qmatrix.msm(Multistate_model_best,covariates = list (`timeperiod[3,5)`=1))[1,2])[c(1,3,4)]
q12_rates[3,]<-as.vector(qmatrix.msm(Multistate_model_best,covariates = list (`timeperiod[5,7)`=1))[1,2])[c(1,3,4)]
q12_rates[4,]<-as.vector(qmatrix.msm(Multistate_model_best,covariates = list (`timeperiod[7,9)`=1))[1,2])[c(1,3,4)]
q12_rates[5,]<-as.vector(qmatrix.msm(Multistate_model_best,covariates = list (`timeperiod[9,11)`=1))[1,2])[c(1,3,4)]
q12_rates[6,]<-as.vector(qmatrix.msm(Multistate_model_best,covariates = list (`timeperiod[11,13)`=1))[1,2])[c(1,3,4)]
q12_rates[7,]<-as.vector(qmatrix.msm(Multistate_model_best,covariates = list (`timeperiod[13,15)`=1))[1,2])[c(1,3,4)]
q12_rates[8,]<-as.vector(qmatrix.msm(Multistate_model_best,covariates = list (`timeperiod[15,Inf)`=1))[1,2])[c(1,3,4)]


q13_rates[1,]<-as.vector(qmatrix.msm(Multistate_model_best)[1,3])[c(1,3,4)]
q13_rates[2,]<-as.vector(qmatrix.msm(Multistate_model_best,covariates = list (`timeperiod[3,5)`=1))[1,3])[c(1,3,4)]
q13_rates[3,]<-as.vector(qmatrix.msm(Multistate_model_best,covariates = list (`timeperiod[5,7)`=1))[1,3])[c(1,3,4)]
q13_rates[4,]<-as.vector(qmatrix.msm(Multistate_model_best,covariates = list (`timeperiod[7,9)`=1))[1,3])[c(1,3,4)]
q13_rates[5,]<-as.vector(qmatrix.msm(Multistate_model_best,covariates = list (`timeperiod[9,11)`=1))[1,3])[c(1,3,4)]
q13_rates[6,]<-as.vector(qmatrix.msm(Multistate_model_best,covariates = list (`timeperiod[11,13)`=1))[1,3])[c(1,3,4)]
q13_rates[7,]<-as.vector(qmatrix.msm(Multistate_model_best,covariates = list (`timeperiod[13,15)`=1))[1,3])[c(1,3,4)]
q13_rates[8,]<-as.vector(qmatrix.msm(Multistate_model_best,covariates = list (`timeperiod[15,Inf)`=1))[1,3])[c(1,3,4)]


write.csv(q12_rates[,1],"q12_rates_estimates.csv")
write.csv(q13_rates[,1],"q13_rates_estimates.csv")

Mean_sojourn_time=function(q12,q13,fish_size,Parasite_strain,Fish_sex,Fish_type){
    
    Expected_time_given_leave_period<-c() #storing E(time in state 1|leave in period i)
    Uncond_mean<-c() #storing unconditional mean E(S_j)
    #P(leave state 1 in period j of interval length t_j-t_{j-1}=2)
    Prob_leave_period1<- 1-exp(-2*(q12[1] +q13[1]))
    Prob_leave_period2<- (1-Prob_leave_period1)* (1-exp(-2*(q12[2] +q13[2])))
    Prob_leave_period3<- (1-Prob_leave_period1-Prob_leave_period2)*(1-exp(-2*(q12[3] +q13[3])))
    Prob_leave_period4<- (1-Prob_leave_period1-Prob_leave_period2-Prob_leave_period3)*(1-exp(-2*(q12[4] +q13[4])))
    Prob_leave_period5<- (1-Prob_leave_period1-Prob_leave_period2-Prob_leave_period3-Prob_leave_period4)*
                         (1-exp(-2*(q12[5] +q13[5])))
    
    Prob_leave_period6<- (1-Prob_leave_period1-Prob_leave_period2-Prob_leave_period3-Prob_leave_period4-Prob_leave_period5)*
                         (1-exp(-2*(q12[6] +q13[6])))
    
    Prob_leave_period7<- (1-Prob_leave_period1-Prob_leave_period2-Prob_leave_period3-Prob_leave_period4-Prob_leave_period5-
                         Prob_leave_period6)*(1-exp(-2*(q12[7] +q13[7])))
    
    Prob_leave_period8_or_later<- 1-(Prob_leave_period1+Prob_leave_period2+Prob_leave_period3+Prob_leave_period4
                                    +Prob_leave_period5+Prob_leave_period6+Prob_leave_period7)
    
    #Uncondition mean E(T1^(j))
        Uncond_mean[[1]]<-(1/(q12[1] +q13[1]))
        Uncond_mean[[2]]<-(1/(q12[2] +q13[2]))
        Uncond_mean[[3]]<-(1/(q12[3] +q13[3]))
        Uncond_mean[[4]]<-(1/(q12[4] +q13[4]))
        Uncond_mean[[5]]<-(1/(q12[5] +q13[5]))
        Uncond_mean[[6]]<-(1/(q12[6] +q13[6]))
        Uncond_mean[[7]]<-(1/(q12[7] +q13[7]))
        Uncond_mean[[8]]<-(1/(q12[8] +q13[8]))
    #tj-t_{j-1}=2
    
    
    
    #E(time in state 1|leave in period i of  interval length t) 
    Expected_time_given_leave_period[[1]]<- 1+ (Uncond_mean[[1]]-(2+Uncond_mean[[1]])*
                                                exp(-(q12[1] +q13[1])*2))/(1-exp(-(q12[1] +q13[1])*2))
    
    Expected_time_given_leave_period[[2]]<- 3+ (Uncond_mean[[2]]-(2+Uncond_mean[[2]])*
                                                exp(-(q12[2] +q13[2])*2))/(1-exp(-(q12[2] +q13[2])*2))
    
    Expected_time_given_leave_period[[3]]<- 5+ (Uncond_mean[[3]]-(2+Uncond_mean[[3]])*
                                                exp(-(q12[3] +q13[3])*2))/(1-exp(-(q12[3] +q13[3])*2))
    
    Expected_time_given_leave_period[[4]]<- 7+ (Uncond_mean[[4]]-(2+Uncond_mean[[4]])*
                                                exp(-(q12[4] +q13[4])*2))/(1-exp(-(q12[4] +q13[4])*2))
    
    Expected_time_given_leave_period[[5]]<- 9+ (Uncond_mean[[5]]-(2+Uncond_mean[[5]])*
                                                exp(-(q12[5] +q13[5])*2))/(1-exp(-(q12[5] +q13[5])*2))
    
     Expected_time_given_leave_period[[6]]<- 11+ (Uncond_mean[[6]]-(2+Uncond_mean[[6]])*
                                                exp(-(q12[6] +q13[6])*2))/(1-exp(-(q12[6] +q13[6])*2))
    
     Expected_time_given_leave_period[[7]]<- 13+ (Uncond_mean[[7]]-(2+Uncond_mean[[7]])*
                                                exp(-(q12[7] +q13[7])*2))/(1-exp(-(q12[7] +q13[7])*2))
    
     #For period 15 or later, we assume that q12(t,z)=q12(8,z) and q13(t,z)=q13(8,z)
     Expected_time_given_leave_period[[8]]<- 15+ (Uncond_mean[[8]]-(2+Uncond_mean[[8]])*
                                                exp(-(q12[8] +q13[8])*2))/(1-exp(-(q12[8] +q13[8])*2))
    
    
    #E(time in state 1|leave in period i of  interval length t)*P(leave in period j of interval length t=2)
    
    sojourn_time<-(Prob_leave_period1*Expected_time_given_leave_period[[1]])+
                  (Prob_leave_period2*Expected_time_given_leave_period[[2]])+
                  (Prob_leave_period3*Expected_time_given_leave_period[[3]])+
                  (Prob_leave_period4*Expected_time_given_leave_period[[4]])+
                  (Prob_leave_period5*Expected_time_given_leave_period[[5]])+
                  (Prob_leave_period6*Expected_time_given_leave_period[[6]])+
                  (Prob_leave_period7*Expected_time_given_leave_period[[7]])+
                  (Prob_leave_period8_or_later*Expected_time_given_leave_period[[8]])
    
   #return the mean sojourn time for the time-inhomogeneous Markov model
return(paste("Mean sojourn time=",sojourn_time,"days given the covariates:",Parasite_strain,"&",
             Fish_sex,"&",Fish_type,"&","fish size of",fish_size,"mm" ))
}

#Results for time homogeneous case (Not needed)
#sojourn.msm(Multistate_model_best)
#1/(q12_rate s[[1]][1]+q13_rates[[1]][1])

fish_sizes=c(11,17,26)

for (fish_size_index in 1:length(fish_sizes)){
#Mean sojourn time given covariates Gt3 strain & Male fish & UA stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gt3_fishType_Deathrate_malefish[1,,][,1]
print(Mean_sojourn_time(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],Parasite_strain="Gt3 strain",
                  Fish_sex="Male fish",Fish_type="UA stock"))

#Mean sojourn time given covariates Gt3 strain & Male fish & LA stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gt3_fishType_Deathrate_malefish[2,,][,1]
print(Mean_sojourn_time(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],Parasite_strain="Gt3 strain",
                  Fish_sex="Male fish",Fish_type="LA stock"))

#Mean sojourn time given covariates Gt3 strain & Male fish & OS stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gt3_fishType_Deathrate_malefish[3,,][,1]
print(Mean_sojourn_time(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],Parasite_strain="Gt3 strain",
                  Fish_sex="Male fish",Fish_type="OS stock"))
    
    }

fish_sizes=c(11,17,26)

for (fish_size_index in 1:length(fish_sizes)){
#Mean sojourn time given covariates Gt3 strain & female fish & UA stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gt3_fishType_Deathrate_femalefish[1,,][,1]
print(Mean_sojourn_time(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],Parasite_strain="Gt3 strain",
                  Fish_sex="Female fish",Fish_type="UA stock"))

#Mean sojourn time given covariates Gt3 strain & female fish & LA stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gt3_fishType_Deathrate_femalefish[2,,][,1]
print(Mean_sojourn_time(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],Parasite_strain="Gt3 strain",
                  Fish_sex="Female fish",Fish_type="LA stock"))

#Mean sojourn time given covariates Gt3 strain & female fish & OS stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gt3_fishType_Deathrate_femalefish[3,,][,1]
print(Mean_sojourn_time(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],Parasite_strain="Gt3 strain",
                  Fish_sex="Female fish",Fish_type="OS stock"))
    
    }

fish_sizes=c(11,17,26)

for (fish_size_index in 1:length(fish_sizes)){
#Mean sojourn time given covariates Gt strain & Male fish & UA stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gt_fishType_Deathrate_malefish[1,,][,1]
print(Mean_sojourn_time(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],Parasite_strain="Gt strain",
                  Fish_sex="Male fish",Fish_type="UA stock"))

#Mean sojourn time given covariates Gt strain & Male fish & LA stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gt_fishType_Deathrate_malefish[2,,][,1]
print(Mean_sojourn_time(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],Parasite_strain="Gt strain",
                  Fish_sex="Male fish",Fish_type="LA stock"))

#Mean sojourn time given covariates Gt strain & Male fish & OS stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gt_fishType_Deathrate_malefish[3,,][,1]
print(Mean_sojourn_time(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],Parasite_strain="Gt strain",
                  Fish_sex="Male fish",Fish_type="OS stock"))
    
    }

fish_sizes=c(11,17,26)

for (fish_size_index in 1:length(fish_sizes)){
#Mean sojourn time given covariates Gt strain & female fish & UA stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gt_fishType_Deathrate_femalefish[1,,][,1]
print(Mean_sojourn_time(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],Parasite_strain="Gt strain",
                  Fish_sex="Female fish",Fish_type="UA stock"))

#Mean sojourn time given covariates Gt strain & female fish & LA stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gt_fishType_Deathrate_femalefish[2,,][,1]
print(Mean_sojourn_time(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],Parasite_strain="Gt strain",
                  Fish_sex="Female fish",Fish_type="LA stock"))

#Mean sojourn time given covariates Gt strain & female fish & OS stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gt_fishType_Deathrate_femalefish[3,,][,1]
print(Mean_sojourn_time(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],Parasite_strain="Gt strain",
                  Fish_sex="Female fish",Fish_type="OS stock"))
    
    }

fish_sizes=c(11,17,26)

for (fish_size_index in 1:length(fish_sizes)){
#Mean sojourn time given covariates Gb strain & Male fish & UA stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gb_fishType_Deathrate_malefish[1,,][,1]
print(Mean_sojourn_time(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],Parasite_strain="Gb strain",
                  Fish_sex="Male fish",Fish_type="UA stock"))

#Mean sojourn time given covariates Gb strain & Male fish & LA stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gb_fishType_Deathrate_malefish[2,,][,1]
print(Mean_sojourn_time(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],Parasite_strain="Gb strain",
                  Fish_sex="Male fish",Fish_type="LA stock"))

#Mean sojourn time given covariates Gb strain & Male fish & OS stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gb_fishType_Deathrate_malefish[3,,][,1]
print(Mean_sojourn_time(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],Parasite_strain="Gb strain",
                  Fish_sex="Male fish",Fish_type="OS stock"))
    
    }

fish_sizes=c(11,17,26)

for (fish_size_index in 1:length(fish_sizes)){
#Mean sojourn time given covariates Gb strain & Female fish & UA stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gb_fishType_Deathrate_femalefish[1,,][,1]
print(Mean_sojourn_time(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],Parasite_strain="Gb strain",
                  Fish_sex="Female fish",Fish_type="UA stock"))

#Mean sojourn time given covariates Gb strain & Female fish & LA stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gb_fishType_Deathrate_femalefish[2,,][,1]
print(Mean_sojourn_time(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],Parasite_strain="Gb strain",
                  Fish_sex="Female fish",Fish_type="LA stock"))

#Mean sojourn time given covariates Gb strain & Female fish & OS stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gb_fishType_Deathrate_femalefish[3,,][,1]
print(Mean_sojourn_time(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],Parasite_strain="Gb strain",
                  Fish_sex="Female fish",Fish_type="OS stock"))
    
    }

#sojourn.msm(Multistate_model_best, covariates = list (`timeperiod[3,5)`=1), ci=c("delta","normal","bootstrap","none"),
#  cl=0.95, B=1000) #for period 3-5

Prob_go_from_state_1_to_2_or_3_next=function(q12,q13,fish_size,Parasite_strain,Fish_sex,Fish_type){
    
   Prob_state_1_2_leave_state1<-c() #P(go from state 1 to 2| leave state 1 in period j)
   Prob_state_1_3_leave_state1<-c() #P(go from state 1 to 3| leave state 1 in period j)
    
    #P(leave in period j of interval length t=2)
    Prob_leave_period1<- 1-exp(-2*(q12[1] +q13[1]))
    Prob_leave_period2<- (1-Prob_leave_period1)* (1-exp(-2*(q12[2] +q13[2])))
    Prob_leave_period3<- (1-Prob_leave_period1-Prob_leave_period2)*(1-exp(-2*(q12[3] +q13[3])))
    Prob_leave_period4<- (1-Prob_leave_period1-Prob_leave_period2-Prob_leave_period3)*(1-exp(-2*(q12[4] +q13[4])))
    Prob_leave_period5<- (1-Prob_leave_period1-Prob_leave_period2-Prob_leave_period3-Prob_leave_period4)*
                         (1-exp(-2*(q12[5] +q13[5])))
    
    Prob_leave_period6<- (1-Prob_leave_period1-Prob_leave_period2-Prob_leave_period3-Prob_leave_period4-Prob_leave_period5)*
                         (1-exp(-2*(q12[6] +q13[6])))
    
    Prob_leave_period7<- (1-Prob_leave_period1-Prob_leave_period2-Prob_leave_period3-Prob_leave_period4-Prob_leave_period5-
                         Prob_leave_period6)*(1-exp(-2*(q12[7] +q13[7])))
    
    Prob_leave_period8_or_later<- 1-(Prob_leave_period1+Prob_leave_period2+Prob_leave_period3+Prob_leave_period4
                                    +Prob_leave_period5+Prob_leave_period6+Prob_leave_period7)
    
    #P(go from state 1 to 2| leave state 1 in period j)
    Prob_state_1_2_leave_state1[[1]]<- q12[1]/(q12[1]+ q13[1])
    Prob_state_1_2_leave_state1[[2]]<- q12[2]/(q12[2]+ q13[2])
    Prob_state_1_2_leave_state1[[3]]<- q12[3]/(q12[3]+ q13[3])
    Prob_state_1_2_leave_state1[[4]]<- q12[4]/(q12[4]+ q13[4])
    Prob_state_1_2_leave_state1[[5]]<- q12[5]/(q12[5]+ q13[5])
    Prob_state_1_2_leave_state1[[6]]<- q12[6]/(q12[6]+ q13[6])
    Prob_state_1_2_leave_state1[[7]]<- q12[7]/(q12[7]+ q13[7])
    Prob_state_1_2_leave_state1[[8]]<- q12[8]/(q12[8]+ q13[8])
    
    #P(go from state 1 to 3| leave state 1 in period j)
    Prob_state_1_3_leave_state1[[1]]<- q13[1]/(q12[1]+ q13[1])
    Prob_state_1_3_leave_state1[[2]]<- q13[2]/(q12[2]+ q13[2])
    Prob_state_1_3_leave_state1[[3]]<- q13[3]/(q12[3]+ q13[3])
    Prob_state_1_3_leave_state1[[4]]<- q13[4]/(q12[4]+ q13[4])
    Prob_state_1_3_leave_state1[[5]]<- q13[5]/(q12[5]+ q13[5])
    Prob_state_1_3_leave_state1[[6]]<- q13[6]/(q12[6]+ q13[6])
    Prob_state_1_3_leave_state1[[7]]<- q13[7]/(q12[7]+ q13[7])
    Prob_state_1_3_leave_state1[[8]]<- q13[8]/(q12[8]+ q13[8]) #period 8 or later
    
    #P(go from state 1 to 2| leave state 1)
    Prob_go_from_state_1_to_2_given_leave_state1<- (Prob_state_1_2_leave_state1[[1]]*Prob_leave_period1)+
                                                   (Prob_state_1_2_leave_state1[[2]]*Prob_leave_period2)+      
                                                   (Prob_state_1_2_leave_state1[[3]]*Prob_leave_period3)+
                                                   (Prob_state_1_2_leave_state1[[4]]*Prob_leave_period4)+ 
                                                   (Prob_state_1_2_leave_state1[[5]]*Prob_leave_period5)+
                                                   (Prob_state_1_2_leave_state1[[6]]*Prob_leave_period6)+
                                                   (Prob_state_1_2_leave_state1[[7]]*Prob_leave_period7)+
                                                   (Prob_state_1_2_leave_state1[[8]]*Prob_leave_period8_or_later)
    #P(go from state 1 to 3| leave state 1)
    Prob_go_from_state_1_to_3_given_leave_state1<- (Prob_state_1_3_leave_state1[[1]]*Prob_leave_period1)+
                                                   (Prob_state_1_3_leave_state1[[2]]*Prob_leave_period2)+      
                                                   (Prob_state_1_3_leave_state1[[3]]*Prob_leave_period3)+
                                                   (Prob_state_1_3_leave_state1[[4]]*Prob_leave_period4)+ 
                                                   (Prob_state_1_3_leave_state1[[5]]*Prob_leave_period5)+
                                                   (Prob_state_1_3_leave_state1[[6]]*Prob_leave_period6)+
                                                   (Prob_state_1_3_leave_state1[[7]]*Prob_leave_period7)+
                                                   (Prob_state_1_3_leave_state1[[8]]*Prob_leave_period8_or_later) 
    
    return(paste("P(1->2)=",Prob_go_from_state_1_to_2_given_leave_state1, "&","P(1->3)=",
             Prob_go_from_state_1_to_3_given_leave_state1,"","given:",Parasite_strain,"&",
             Fish_sex,"&",Fish_type,"&","fish size of",fish_size,"mm" ))
    
    }
    

fish_sizes=c(11,17,26)

for (fish_size_index in 1:length(fish_sizes)){
#probability of next transition given covariates Gt3 strain & Male fish & UA stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gt3_fishType_Deathrate_malefish[1,,][,1]
print(Prob_go_from_state_1_to_2_or_3_next(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],
                            Parasite_strain="Gt3 strain",Fish_sex="Male fish",Fish_type="UA stock"))

#probability of next transitiongiven covariates Gt3 strain & Male fish & LA stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gt3_fishType_Deathrate_malefish[2,,][,1]
print(Prob_go_from_state_1_to_2_or_3_next(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],
                    Parasite_strain="Gt3 strain", Fish_sex="Male fish",Fish_type="LA stock"))

#probability of next transition given covariates Gt3 strain & Male fish & OS stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gt3_fishType_Deathrate_malefish[3,,][,1]
print(Prob_go_from_state_1_to_2_or_3_next(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],
        Parasite_strain="Gt3 strain",Fish_sex="Male fish",Fish_type="OS stock"))
    
    }

fish_sizes=c(11,17,26)

for (fish_size_index in 1:length(fish_sizes)){
#probability of next transition given covariates Gt3 strain & female fish & UA stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gt3_fishType_Deathrate_femalefish[1,,][,1]
print(Prob_go_from_state_1_to_2_or_3_next(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],
                            Parasite_strain="Gt3 strain",Fish_sex="Female fish",Fish_type="UA stock"))

#probability of next transitiongiven covariates Gt3 strain & female fish & LA stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gt3_fishType_Deathrate_femalefish[2,,][,1]
print(Prob_go_from_state_1_to_2_or_3_next(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],
                    Parasite_strain="Gt3 strain", Fish_sex="Female fish",Fish_type="LA stock"))

#probability of next transition given covariates Gt3 strain & female fish & OS stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gt3_fishType_Deathrate_femalefish[3,,][,1]
print(Prob_go_from_state_1_to_2_or_3_next(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],
        Parasite_strain="Gt3 strain",Fish_sex="Female fish",Fish_type="OS stock"))
    
    }

fish_sizes=c(11,17,26)

for (fish_size_index in 1:length(fish_sizes)){
#probability of next transition given covariates Gt strain & Male fish & UA stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gt_fishType_Deathrate_malefish[1,,][,1]
print(Prob_go_from_state_1_to_2_or_3_next(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],
                            Parasite_strain="Gt strain",Fish_sex="Male fish",Fish_type="UA stock"))

#probability of next transitiongiven covariates Gt strain & Male fish & LA stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gt_fishType_Deathrate_malefish[2,,][,1]
print(Prob_go_from_state_1_to_2_or_3_next(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],
                    Parasite_strain="Gt strain", Fish_sex="Male fish",Fish_type="LA stock"))

#probability of next transition given covariates Gt strain & Male fish & OS stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gt_fishType_Deathrate_malefish[3,,][,1]
print(Prob_go_from_state_1_to_2_or_3_next(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],
        Parasite_strain="Gt strain",Fish_sex="Male fish",Fish_type="OS stock"))
    
    }

fish_sizes=c(11,17,26)

for (fish_size_index in 1:length(fish_sizes)){
#probability of next transition given covariates Gt strain & Female fish & UA stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gt_fishType_Deathrate_femalefish[1,,][,1]
print(Prob_go_from_state_1_to_2_or_3_next(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],
                            Parasite_strain="Gt strain",Fish_sex="Female fish",Fish_type="UA stock"))

#probability of next transitiongiven covariates Gt strain & Female fish & LA stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gt_fishType_Deathrate_femalefish[2,,][,1]
print(Prob_go_from_state_1_to_2_or_3_next(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],
                    Parasite_strain="Gt strain", Fish_sex="Female fish",Fish_type="LA stock"))

#probability of next transition given covariates Gt strain & Female fish & OS stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gt_fishType_Deathrate_femalefish[3,,][,1]
print(Prob_go_from_state_1_to_2_or_3_next(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],
        Parasite_strain="Gt strain",Fish_sex="Female fish",Fish_type="OS stock"))
    
    }

fish_sizes=c(11,17,26)

for (fish_size_index in 1:length(fish_sizes)){
#probability of next transition given covariates Gb strain & Male fish & UA stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gb_fishType_Deathrate_malefish[1,,][,1]
print(Prob_go_from_state_1_to_2_or_3_next(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],
                            Parasite_strain="Gb strain",Fish_sex="Male fish",Fish_type="UA stock"))

#probability of next transitiongiven covariates Gb strain & Male fish & LA stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gb_fishType_Deathrate_malefish[2,,][,1]
print(Prob_go_from_state_1_to_2_or_3_next(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],
                    Parasite_strain="Gb strain", Fish_sex="Male fish",Fish_type="LA stock"))

#probability of next transition given covariates Gb strain & Male fish & OS stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gb_fishType_Deathrate_malefish[3,,][,1]
print(Prob_go_from_state_1_to_2_or_3_next(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],
        Parasite_strain="Gb strain",Fish_sex="Male fish",Fish_type="OS stock"))
    
    }

fish_sizes=c(11,17,26)

for (fish_size_index in 1:length(fish_sizes)){
#probability of next transition given covariates Gb strain & female fish & UA stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gb_fishType_Deathrate_femalefish[1,,][,1]
print(Prob_go_from_state_1_to_2_or_3_next(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],
                            Parasite_strain="Gb strain",Fish_sex="Female fish",Fish_type="UA stock"))

#probability of next transitiongiven covariates Gb strain & female fish & LA stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gb_fishType_Deathrate_femalefish[2,,][,1]
print(Prob_go_from_state_1_to_2_or_3_next(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],
                    Parasite_strain="Gb strain", Fish_sex="Female fish",Fish_type="LA stock"))

#probability of next transition given covariates Gb strain & female fish & OS stock & different fish sizes
q12_rates<-Recoveryrate_fishsize[fish_size_index, ,][,1];q13_rates<-Gb_fishType_Deathrate_femalefish[3,,][,1]
print(Prob_go_from_state_1_to_2_or_3_next(q12=q12_rates,q13=q13_rates,fish_size=fish_sizes[fish_size_index],
        Parasite_strain="Gb strain",Fish_sex="Female fish",Fish_type="OS stock"))
    
    }



Gt3_fishType_death_prob=array(dim=c(3,8,1))#estimates death probability
Gt_fishType_death_prob=array(dim=c(3,8,1)) #estimates death probability
Gb_fishType_death_prob=array(dim=c(3,8,1)) #estimates death probability 

Gt3_fishType_death_prob1=array(dim=c(3,8,1))#estimates death probability
Gt_fishType_death_prob1=array(dim=c(3,8,1)) #estimates death probability
Gb_fishType_death_prob1=array(dim=c(3,8,1)) #estimates death probability 

time_intervals=rbind(seq(1,15,by=2),seq(1,15,by=2)+2)

time=c(3,5,7,9,11,13,15,17)
for (i in 1:length(time)){
  Gt3_fishType_death_prob[1,i,]=pmatrix.msm(Multistate_model_best,  t1=time_intervals[,i][1],t=time_intervals[,i][2],
            covariates = list (Parasite_type = "Gt3",Fish_population='Upper Aripo'))[1,3]
  Gt3_fishType_death_prob[2,i,]=pmatrix.msm(Multistate_model_best,  t1=time_intervals[,i][1],t=time_intervals[,i][2],
            covariates = list (Parasite_type = "Gt3",Fish_population='Lower Aripo'))[1,3]
  Gt3_fishType_death_prob[3,i,]=pmatrix.msm(Multistate_model_best,  t1=time_intervals[,i][1],t=time_intervals[,i][2],
            covariates = list (Parasite_type = "Gt3",Fish_population='Ornamental'))[1,3]
  Gt_fishType_death_prob[1,i,]=pmatrix.msm(Multistate_model_best,  t1=time_intervals[,i][1],t=time_intervals[,i][2],
            covariates = list (Parasite_type = "Gt",Fish_population='Upper Aripo'))[1,3]
  Gt_fishType_death_prob[2,i,]=pmatrix.msm(Multistate_model_best,  t1=time_intervals[,i][1],t=time_intervals[,i][2],
            covariates = list (Parasite_type = "Gt",Fish_population='Lower Aripo'))[1,3]
  Gt_fishType_death_prob[3,i,]=pmatrix.msm(Multistate_model_best,  t1=time_intervals[,i][1],t=time_intervals[,i][2],
            covariates = list (Parasite_type = "Gt",Fish_population='Ornamental'))[1,3]
  Gb_fishType_death_prob[1,i,]=pmatrix.msm(Multistate_model_best,  t1=time_intervals[,i][1],t=time_intervals[,i][2],
            covariates = list (Parasite_type = "Gbull",Fish_population='Upper Aripo'))[1,3]
  Gb_fishType_death_prob[2,i,]=pmatrix.msm(Multistate_model_best,  t1=time_intervals[,i][1],t=time_intervals[,i][2],
            covariates = list (Parasite_type = "Gbull",Fish_population='Lower Aripo'))[1,3]
  Gb_fishType_death_prob[3,i,]=pmatrix.msm(Multistate_model_best,  t1=time_intervals[,i][1],t=time_intervals[,i][2],
            covariates = list (Parasite_type = "Gbull",Fish_population='Ornamental'))[1,3]
}

time=c(3,5,7,9,11,13,15,17)
for (i in 1:length(time)){
   Gt3_fishType_death_prob1[1,i,]=pmatrix.msm(Multistate_model_best,  t1=1,t=time[i],
            covariates = list (Parasite_type = "Gt3",Fish_population='Upper Aripo'))[1,3]
Gt3_fishType_death_prob1[2,i,]=pmatrix.msm(Multistate_model_best,t1=1,t=time[i],
            covariates = list (Parasite_type = "Gt3",Fish_population='Lower Aripo'))[1,3]
Gt3_fishType_death_prob1[3,i,]=pmatrix.msm(Multistate_model_best,t1=1,t=time[i],
            covariates = list (Parasite_type = "Gt3",Fish_population='Ornamental'))[1,3]
Gt_fishType_death_prob1[1,i,]=pmatrix.msm(Multistate_model_best,t1=1,t=time[i],
            covariates = list (Parasite_type = "Gt",Fish_population='Upper Aripo'))[1,3]
Gt_fishType_death_prob1[2,i,]=pmatrix.msm(Multistate_model_best,t1=1,t=time[i],
            covariates = list (Parasite_type = "Gt",Fish_population='Lower Aripo'))[1,3]
Gt_fishType_death_prob1[3,i,]=pmatrix.msm(Multistate_model_best,t1=1,t=time[i],
            covariates = list (Parasite_type = "Gt",Fish_population='Ornamental'))[1,3]
Gb_fishType_death_prob1[1,i,]=pmatrix.msm(Multistate_model_best,t1=1,t=time[i],
            covariates = list (Parasite_type = "Gbull",Fish_population='Upper Aripo'))[1,3]
Gb_fishType_death_prob1[2,i,]=pmatrix.msm(Multistate_model_best,t1=1,t=time[i],
            covariates = list (Parasite_type = "Gbull",Fish_population='Lower Aripo'))[1,3]
Gb_fishType_death_prob1[3,i,]=pmatrix.msm(Multistate_model_best,t1=1,t=time[i],
            covariates = list (Parasite_type = "Gbull",Fish_population='Ornamental'))[1,3]

}

#Between "1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"

o<-par(mar=c(0,4,2,2))#Run this before the code below

nf<-layout(matrix(1:3, nrow=3,ncol=1))

    
# rest parameters
par(o)#
o<-par(mar=c(0,4,2,2))

plot(Gb_fishType_death_prob[1, ,],type="b",col="green",lwd=3,xaxt = "n",ylab="",xlab="",pch=3,cex.axis=1.2,ylim=c(0,0.5))
lines(Gt_fishType_death_prob[1, ,],type="b",col="red",lwd=3,pch=19)
lines(Gt3_fishType_death_prob[1, ,],type="b",col="blue",lwd=3,pch=24)



text(2,0.49,"Fish stock: Upper Aripo",cex=1.5,lwd=3)


o<-par(mar=c(0,4,0,2))
plot(Gt3_fishType_death_prob[2, ,],type="b",col="blue",lwd=3,xaxt = "n",ylab="Predicted probability of death",xlab="",
     ylim=c(0,0.28),pch=24,cex.lab=1.4, cex.axis=1.2)
lines(Gt_fishType_death_prob[2, ,],type="b",col="red",lwd=3,pch=19)
lines(Gb_fishType_death_prob[2, ,],type="b",col="green",lwd=3,pch=3)

legend(x = 5,y=0.1,,inset = 0,
        legend = c("Gt3","Gt","Gb"), 
        col=c("blue","red","green"), lwd=2, cex=1.35, horiz = TRUE,pch=c(24,19,3),pt.cex = 1)

text(2,0.28,"Fish stock: Lower Aripo",cex=1.5,lwd=3)



par(mar=c(4,4,0,2))

plot(Gb_fishType_death_prob[3, ,],type="b",col="green",lwd=3,xaxt = "n",ylab="",xlab="Observed time intervals (in days)",
     pch=3,cex.lab=1.4, cex.axis=1.2, ylim=c(0,0.13))
lines(Gt_fishType_death_prob[3, ,],type="b",col="red",lwd=3,pch=19)
lines(Gt3_fishType_death_prob[3, ,],type="b",col="blue",lwd=3,pch=24)

axis(1, at=1:8, labels=c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"))
text(2,0.13,"Fish stock: Ornamental",cex=1.5,lwd=3)


par(o)

#From t=1 to t=3,5,7,9,11,13,15,17

o<-par(mar=c(0,4,2,2))#Run this before the code below

nf<-layout(matrix(1:3, nrow=3,ncol=1))

    
# rest parameters
par(o)#
o<-par(mar=c(0,4,2,2))

plot(Gb_fishType_death_prob1[1, ,],type="b",col="green",lwd=3,xaxt = "n",ylab="",xlab="",pch=3,cex.axis=1.2,ylim=c(0,0.5))
lines(Gt_fishType_death_prob1[1, ,],type="b",col="red",lwd=3,pch=19)
lines(Gt3_fishType_death_prob1[1, ,],type="b",col="blue",lwd=3,pch=24)



text(2,0.49,"Fish stock: Upper Aripo",cex=1.5,lwd=3)


o<-par(mar=c(0,4,0,2))
plot(Gt3_fishType_death_prob1[2, ,],type="b",col="blue",lwd=3,xaxt = "n",ylab="Predicted probability of death",xlab="",
     ylim=c(0,0.28),pch=24,cex.lab=1.4, cex.axis=1.2)
lines(Gt_fishType_death_prob1[2, ,],type="b",col="red",lwd=3,pch=19)
lines(Gb_fishType_death_prob1[2, ,],type="b",col="green",lwd=3,pch=3)

legend(x = 5,y=0.1,,inset = 0,
        legend = c("Gt3","Gt","Gb"), 
        col=c("blue","red","green"), lwd=2, cex=1.35, horiz = TRUE,pch=c(24,19,3),pt.cex = 1)

text(2,0.28,"Fish stock: Lower Aripo",cex=1.5,lwd=3)



par(mar=c(4,4,0,2))

plot(Gb_fishType_death_prob1[3, ,],type="b",col="green",lwd=3,xaxt = "n",ylab="",xlab="Observed time intervals (in days)",
     pch=3,cex.lab=1.4, cex.axis=1.2, ylim=c(0,0.13))
lines(Gt_fishType_death_prob1[3, ,],type="b",col="red",lwd=3,pch=19)
lines(Gt3_fishType_death_prob1[3, ,],type="b",col="blue",lwd=3,pch=24)

axis(1, at=1:8, labels=c("1-3","1-5","1-7","1-9","1-11","1-13","1-15","1-17"))
text(2,0.13,"Fish stock: Ornamental",cex=1.5,lwd=3)


par(o)

Prob1=c(c(Gt3_fishType_death_prob[1, ,]),c(Gt_fishType_death_prob[1, ,]),c(Gb_fishType_death_prob[1, ,]))
Group1=c(rep("Gt3-UA",8),rep("Gt-UA",8),rep("Gb-UA",8))
Time_interval1=c(c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"),c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"),
              c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"))

Prob2=c(c(Gt3_fishType_death_prob[2, ,]),c(Gt_fishType_death_prob[2, ,]),c(Gb_fishType_death_prob[2, ,]))
Group2=c(rep("Gt3-LA",8),rep("Gt-LA",8),rep("Gb-LA",8))
Time_interval2=c(c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"),c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"),
               c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"))

Prob3=c(c(Gt3_fishType_death_prob[3, ,]),c(Gt_fishType_death_prob[3, ,]),c(Gb_fishType_death_prob[3, ,]))
Group3=c(rep("Gt3-OS",8),rep("Gt-OS",8),rep("Gb-OS",8))
Time_interval3=c(c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"),c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"),
            c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"))


Prob=c(Prob1,Prob2,Prob3)
Group=c(Group1,Group2,Group3)
Time_interval=c(Time_interval1,Time_interval2,Time_interval3)

Prob_data_UA=data.frame(Prob=Prob1,Group=Group1,Time_interval=Time_interval1)
Prob_data_LA=data.frame(Prob=Prob2,Group=Group2,Time_interval=Time_interval2)
Prob_data_OS=data.frame(Prob=Prob3,Group=Group3,Time_interval=Time_interval3)
Pred_prob_data=data.frame(Prob,Group,Time_interval)

head(Prob_data_UA)

Rates1=c(c(Gt3_fishType_Deathrate[1, ,][, 1]),c(Gt_fishType_Deathrate[1, ,][, 1]),c(Gb_fishType_Deathrate[1, ,][, 1]))
Group1=c(rep("Gt3-UA",8),rep("Gt-UA",8),rep("Gb-UA",8))
Time_interval1=c(c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"),c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"),
              c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"))

Rates2=c(c(Gt3_fishType_Deathrate[2, ,][, 1]),c(Gt_fishType_Deathrate[2, ,][, 1]),c(Gb_fishType_Deathrate[2, ,][, 1]))
Group2=c(rep("Gt3-LA",8),rep("Gt-LA",8),rep("Gb-LA",8))
Time_interval2=c(c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"),c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"),
               c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"))

Rates3=c(c(Gt3_fishType_Deathrate[3, ,][, 1]),c(Gt_fishType_Deathrate[3, ,][, 1]),c(Gb_fishType_Deathrate[3, ,][, 1]))
Group3=c(rep("Gt3-OS",8),rep("Gt-OS",8),rep("Gb-OS",8))
Time_interval3=c(c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"),c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"),
            c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"))


Rates=c(Rates1,Rates2,Rates3)
Group=c(Group1,Group2,Group3)
Time_interval=c(Time_interval1,Time_interval2,Time_interval3)

Rate_data_UA=data.frame(Rate=Rates1,Group=Group1,Time_interval=Time_interval1)
Rate_data_LA=data.frame(Rate=Rates2,Group=Group2,Time_interval=Time_interval2)
Rate_data_OS=data.frame(Rate=Rates3,Group=Group3,Time_interval=Time_interval3)
Pred_Rate_data=data.frame(Rates,Group,Time_interval)

head(Rate_data_UA)

write.csv(Pred_Rate_data,"Death_rates_MSM.csv")


dim(Pred_Rate_data)

Prob_UA <- lmer(Prob~ Group + (1|Time_interval), data=Prob_data_UA)
anova(Prob_UA)

results_UA<-estimate_means(Prob_UA)
print(results_UA)

estimate_contrasts(Prob_UA,standardize=F)

Prob_LA <- lmer(Prob~ Group + (1|Time_interval), data=Prob_data_LA)
anova(Prob_LA)

results_LA<-estimate_means(Prob_LA)
print(results_LA)

estimate_contrasts(Prob_LA,standardize=F)

Prob_OS <- lmer(Prob~ Group + (1|Time_interval), data=Prob_data_OS)
anova(Prob_OS)

results_OS<-estimate_means(Prob_OS)
print(results_OS)

estimate_contrasts(Prob_OS,standardize=F)

#GLMM on Combined probability data

Prob_fit <- lmer(Prob~ Group + (1|Time_interval), data=Pred_prob_data)
anova(Prob_fit)

results<-estimate_means(Prob_fit)
print(results)

estimate_contrasts(Prob_fit,standardize=F)

Mean_compare=ggplot(results, aes(x=results$Group, y=results$Mean, group=1)) +
  geom_line(position=position_dodge(0.1)) +
 geom_pointrange(aes(ymin=CI_low, ymax=CI_high))  + ylab("Mean probability of death") +xlab("Groups") +theme_modern() +
    theme(plot.margin = unit(c(1,1,0.5,0.5), "cm"))+
geom_errorbar(aes(ymin=results$CI_low, ymax=results$CI_high),width=.1,position=position_dodge(0.1))

Mean_compare+ theme(axis.ticks = element_line(size = 1))

Rate_UA <- lmer(Rate~ Group + (1|Time_interval), data=Rate_data_UA)
anova(Rate_UA)

Rate_results_UA<-estimate_means(Rate_UA)
print(Rate_results_UA)

estimate_contrasts(Rate_UA,standardize=F)

Rate_LA <- lmer(Rate~ Group + (1|Time_interval), data=Rate_data_LA)
anova(Rate_LA)

Rate_results_LA<-estimate_means(Rate_LA)
print(Rate_results_LA)

estimate_contrasts(Rate_LA,standardize=F)

Rate_OS <- lmer(Rate~ Group + (1|Time_interval), data=Rate_data_OS)
anova(Rate_OS)

Rate_results_OS<-estimate_means(Rate_OS)
print(Rate_results_OS)

estimate_contrasts(Rate_OS,standardize=F)

#GLMM on Combined probability data
Rate_fit <- lmer(Rates~ Group + (1|Time_interval), data=Pred_Rate_data)
anova(Rate_fit)

results<-estimate_means(Rate_fit)
print(results)

estimate_contrasts(Rate_fit,standardize=F)

Mean_compare=ggplot(results, aes(x=results$Group, y=results$Mean, group=1)) +
  geom_line(position=position_dodge(0.1)) +
 geom_pointrange(aes(ymin=CI_low, ymax=CI_high))  + ylab("Mean death rate") +xlab("Groups") +theme_modern() +
    theme(plot.margin = unit(c(1,1,0.5,0.5), "cm"))+
geom_errorbar(aes(ymin=results$CI_low, ymax=results$CI_high),width=.1,position=position_dodge(0.1))

Mean_compare+ theme(axis.ticks = element_line(size = 1))




plot(Prob_state1,type="o",col="red",lwd=3
      ,ylab="Probability of host infection status",las=1,xaxt = "n",xlab="Time (infection period in days)",
    ylim=c(0,1),pch=15)
lines(Prob_state2,type="o",col="blue",lwd=3,pch=24)
lines(Prob_state3,type="o",col="green",lwd=3,pch=19)

axis(1, at=1:9, labels=c("1","3","5","7","9","11","13","15","17"))
text <- c("Fish remains infected (state 1)","Fish alive with loss of infection (state 2)",
          "Fish dead (state 3)")
legend_order <- matrix(1:3,ncol=1,byrow = T)
legend(x = 3.3,y=1.01,legend = text[legend_order],
       col=c("red","blue","green"),ncol=1, lwd=2, cex=.8,title="Transitions from infected state conditioned on covariates",
      lty = c(1,1,1)[legend_order],pch=c(15,24,19))



#Multistate_model_best$Qmatrices
#qmatrix.msm(Multistate_model_best, covariates = 0)
#Multistate_model_best$Qmatrices
Multistate_model_best$Qmatrices$baseline
Multistate_model_best$Qmatrices

Multistate_model_best$Qmatrices$baseline[1,2]

abs(Multistate_model_best$Qmatrices$`timeperiod[3,5)`)[1,2]


abs(Multistate_model_best$Qmatrices$`timeperiod[1,3)`)[1,2]

#Multistate_model_best$Qmatrices

Qrate_State2=c(Multistate_model_best$Qmatrices$baseline[1,2],abs(Multistate_model_best$Qmatrices$`timeperiod[3,5)`)[1,2],
abs(Multistate_model_best$Qmatrices$`timeperiod[5,7)`)[1,2],abs(Multistate_model_best$Qmatrices$`timeperiod[7,9)`)[1,2],
abs(Multistate_model_best$Qmatrices$`timeperiod[9,11)`)[1,2],  abs(Multistate_model_best$Qmatrices$`timeperiod[11,13)`)[1,2],
abs(Multistate_model_best$Qmatrices$`timeperiod[13,15)`)[1,2],  abs(Multistate_model_best$Qmatrices$`timeperiod[15,Inf)`)[1,2])

Qrate_State3=c(Multistate_model_best$Qmatrices$baseline[1,3],abs(Multistate_model_best$Qmatrices$`timeperiod[3,5)`)[1,3],
abs(Multistate_model_best$Qmatrices$`timeperiod[5,7)`)[1,3],abs(Multistate_model_best$Qmatrices$`timeperiod[7,9)`)[1,3],
abs(Multistate_model_best$Qmatrices$`timeperiod[9,11)`)[1,3],  abs(Multistate_model_best$Qmatrices$`timeperiod[11,13)`)[1,3],
abs(Multistate_model_best$Qmatrices$`timeperiod[13,15)`)[1,3],  abs(Multistate_model_best$Qmatrices$`timeperiod[15,Inf)`)[1,3])


plot(Qrate_State2,type="s",col="red",lwd=3
      ,ylab="Transition rate",las=1,xaxt = "n",xlab="Observed time intervals (in days) ",
    ylim=c(0,3.1))
lines(Qrate_State3,type="s",col="blue",lwd=2)


axis(1, at=1:8, labels=c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"))
text <- c("Fish alive with loss of infection (state 2)",
          "Fish dead (state 3)")
legend_order <- matrix(1:2,ncol=1,byrow = T)
legend(x = 1.3,y=3,legend = text[legend_order],
       col=c("red","blue"),ncol=1, cex=1.1,title="Rate of transition by infected fish (from state 1)",
     box.lwd = 2,fill=c("red","blue"))





#Multistate_model_best$Qmatrices

Qrate_State2=c(Multistate_model_best$Qmatrices$baseline[1,2],abs(Multistate_model_best$Qmatrices$`timeperiod[3,5)`)[1,2],
abs(Multistate_model_best$Qmatrices$`timeperiod[5,7)`)[1,2],abs(Multistate_model_best$Qmatrices$`timeperiod[7,9)`)[1,2],
abs(Multistate_model_best$Qmatrices$`timeperiod[9,11)`)[1,2],  abs(Multistate_model_best$Qmatrices$`timeperiod[11,13)`)[1,2],
abs(Multistate_model_best$Qmatrices$`timeperiod[13,15)`)[1,2],  abs(Multistate_model_best$Qmatrices$`timeperiod[15,Inf)`)[1,2])

Qrate_State3=c(Multistate_model_best$Qmatrices$baseline[1,3],abs(Multistate_model_best$Qmatrices$`timeperiod[3,5)`)[1,3],
abs(Multistate_model_best$Qmatrices$`timeperiod[5,7)`)[1,3],abs(Multistate_model_best$Qmatrices$`timeperiod[7,9)`)[1,3],
abs(Multistate_model_best$Qmatrices$`timeperiod[9,11)`)[1,3],  abs(Multistate_model_best$Qmatrices$`timeperiod[11,13)`)[1,3],
abs(Multistate_model_best$Qmatrices$`timeperiod[13,15)`)[1,3],  abs(Multistate_model_best$Qmatrices$`timeperiod[15,Inf)`)[1,3])


plot(Qrate_State2,type="s",col="grey60",lwd=3
      ,ylab="Transition rate",las=1,xaxt = "n",xlab="Observed time intervals (in days) ",
    ylim=c(0,3.1))
lines(Qrate_State3,type="s",col="grey0",lwd=3)


axis(1, at=1:8, labels=c("1-3","3-5","5-7","7-9","9-11","11-13","13-15","15-17"))
text <- c("Fish alive with loss of infection (state 2)",
          "Fish dead (state 3)")
legend_order <- matrix(1:2,ncol=1,byrow = T)
legend(x = 1.3,y=3,legend = text[legend_order],
       col=c("grey60","grey0"),ncol=1, cex=1.1,title="Rate of transition by infected fish (from state 1)",box.lwd = 2,
       fill=c("grey60","grey0"))


#With parasite load as additional covariate
sojourn.msm(Multistate_model_best)

pnext.msm(Multistate_model_best)

totlos.msm(Multistate_model_best)

#male
Multistate_model_2=NULL
Multistate_model_2[[1]]=Multistate_model_best

i=1
hazard.msm(Multistate_model_2[[i]])$`Sex_fishMale fish`
Est1_2=log(hazard.msm(Multistate_model_2[[i]])$'Sex_fishMale fish'[1,1])
L1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$'Sex_fishMale fish'[1,2])
U1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$'Sex_fishMale fish'[1,3])
SE_1 =(U1_limit_1_2-L1_limit_1_2)/(2*1.96)
z_1=abs(Est1_2/SE_1)
P_value1=exp((−0.717*z_1)-(0.416*z_1^2))
print(paste("P-value: state 1-2=",P_value1))

Est1_3=log(hazard.msm(Multistate_model_2[[i]])$'Sex_fishMale fish'[2,1])
L2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$'Sex_fishMale fish'[2,2])
U2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$'Sex_fishMale fish'[2,3])
SE_2 =(U2_limit_1_2-L2_limit_1_2)/(2*1.96)
z_2=abs(Est1_3/SE_2)
P_value2=exp((−0.717*z_2)-(0.416*z_2^2))
print(paste("P-value: state 1-3=",P_value2))

#LA
i=1
hazard.msm(Multistate_model_2[[i]])$`Fish_populationLower Aripo`
Est1_2=log(hazard.msm(Multistate_model_2[[i]])$`Fish_populationLower Aripo`[1,1])
L1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`Fish_populationLower Aripo`[1,2])
U1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`Fish_populationLower Aripo`[1,3])
SE_1 =(U1_limit_1_2-L1_limit_1_2)/(2*1.96)
z_1=abs(Est1_2/SE_1)
P_value1=exp((−0.717*z_1)-(0.416*z_1^2))
print(paste("P-value: state 1-2=",P_value1))

Est1_3=log(hazard.msm(Multistate_model_2[[i]])$`Fish_populationLower Aripo`[2,1])
L2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`Fish_populationLower Aripo`[2,2])
U2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`Fish_populationLower Aripo`[2,3])
SE_2 =(U2_limit_1_2-L2_limit_1_2)/(2*1.96)
z_2=abs(Est1_3/SE_2)
P_value2=exp((−0.717*z_2)-(0.416*z_2^2))
print(paste("P-value: state 1-3=",P_value2))


#Ornamental
i=1
hazard.msm(Multistate_model_2[[i]])$Fish_populationOrnamental
Est1_2=log(hazard.msm(Multistate_model_2[[i]])$Fish_populationOrnamental[1,1])
L1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$Fish_populationOrnamental[1,2])
U1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$Fish_populationOrnamental[1,3])
SE_1 =(U1_limit_1_2-L1_limit_1_2)/(2*1.96)
z_1=abs(Est1_2/SE_1)
P_value1=exp((−0.717*z_1)-(0.416*z_1^2))
print(paste("P-value: state 1-2=",P_value1))

Est1_3=log(hazard.msm(Multistate_model_2[[i]])$Fish_populationOrnamental[2,1])
L2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$Fish_populationOrnamental[2,2])
U2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$Fish_populationOrnamental[2,3])
SE_2 =(U2_limit_1_2-L2_limit_1_2)/(2*1.96)
z_2=abs(Est1_3/SE_2)
P_value2=exp((−0.717*z_2)-(0.416*z_2^2))
print(paste("P-value: state 1-3=",P_value2))


#Gt
i=1
hazard.msm(Multistate_model_2[[i]])$Parasite_typeGt
Est1_2=log(hazard.msm(Multistate_model_2[[i]])$Parasite_typeGt[1,1])
L1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$Parasite_typeGt[1,2])
U1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$Parasite_typeGt[1,3])
SE_1 =(U1_limit_1_2-L1_limit_1_2)/(2*1.96)
z_1=abs(Est1_2/SE_1)
P_value1=exp((−0.717*z_1)-(0.416*z_1^2))
print(paste("P-value: state 1-2=",P_value1))

Est1_3=log(hazard.msm(Multistate_model_2[[i]])$Parasite_typeGt[2,1])
L2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$Parasite_typeGt[2,2])
U2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$Parasite_typeGt[2,3])
SE_2 =(U2_limit_1_2-L2_limit_1_2)/(2*1.96)
z_2=abs(Est1_3/SE_2)
P_value2=exp((−0.717*z_2)-(0.416*z_2^2))
print(paste("P-value: state 1-3=",P_value2))



#Gb
i=1
hazard.msm(Multistate_model_2[[i]])$Parasite_typeGbull
Est1_2=log(hazard.msm(Multistate_model_2[[i]])$Parasite_typeGbull[1,1])
L1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$Parasite_typeGbull[1,2])
U1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$Parasite_typeGbull[1,3])
SE_1 =(U1_limit_1_2-L1_limit_1_2)/(2*1.96)
z_1=abs(Est1_2/SE_1)
P_value1=exp((−0.717*z_1)-(0.416*z_1^2))
print(paste("P-value: state 1-2=",P_value1))

Est1_3=log(hazard.msm(Multistate_model_2[[i]])$Parasite_typeGbull[2,1])
L2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$Parasite_typeGbull[2,2])
U2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$Parasite_typeGbull[2,3])
SE_2 =(U2_limit_1_2-L2_limit_1_2)/(2*1.96)
z_2=abs(Est1_3/SE_2)
P_value2=exp((−0.717*z_2)-(0.416*z_2^2))
print(paste("P-value: state 1-3=",P_value2))

i=1
hazard.msm(Multistate_model_2[[i]])$Fish_size
Est1_2=log(hazard.msm(Multistate_model_2[[i]])$Fish_size[1,1])
L1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$Fish_size[1,2])
U1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$Fish_size[1,3])
SE_1 =(U1_limit_1_2-L1_limit_1_2)/(2*1.96)
z_1=abs(Est1_2/SE_1)
P_value1=exp((−0.717*z_1)-(0.416*z_1^2))
print(paste("P-value: state 1-2=",P_value1))

Est1_3=log(hazard.msm(Multistate_model_2[[i]])$Fish_size[2,1])
L2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$Fish_size[2,2])
U2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$Fish_size[2,3])
SE_2 =(U2_limit_1_2-L2_limit_1_2)/(2*1.96)
z_2=abs(Est1_3/SE_2)
P_value2=exp((−0.717*z_2)-(0.416*z_2^2))
print(paste("P-value: state 1-3=",P_value2))

i=1
hazard.msm(Multistate_model_2[[i]])$`timeperiod[3,5)`
Est1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[3,5)`[1,1])
L1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[3,5)`[1,2])
U1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[3,5)`[1,3])
SE_1 =(U1_limit_1_2-L1_limit_1_2)/(2*1.96)
z_1=abs(Est1_2/SE_1)
P_value1=exp((−0.717*z_1)-(0.416*z_1^2))
print(paste("P-value: state 1-2=",P_value1))

Est1_3=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[3,5)`[2,1])
L2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[3,5)`[2,2])
U2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[3,5)`[2,3])
SE_2 =(U2_limit_1_2-L2_limit_1_2)/(2*1.96)
z_2=abs(Est1_3/SE_2)
P_value2=exp((−0.717*z_2)-(0.416*z_2^2))
print(paste("P-value: state 1-3=",P_value2))

i=1
hazard.msm(Multistate_model_2[[i]])$`timeperiod[5,7)`
Est1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[5,7)`[1,1])
L1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[5,7)`[1,2])
U1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[5,7)`[1,3])
SE_1 =(U1_limit_1_2-L1_limit_1_2)/(2*1.96)
z_1=abs(Est1_2/SE_1)
P_value1=exp((−0.717*z_1)-(0.416*z_1^2))
print(paste("P-value: state 1-2=",P_value1))

Est1_3=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[5,7)`[2,1])
L2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[5,7)`[2,2])
U2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[5,7)`[2,3])
SE_2 =(U2_limit_1_2-L2_limit_1_2)/(2*1.96)
z_2=abs(Est1_3/SE_2)
P_value2=exp((−0.717*z_2)-(0.416*z_2^2))
print(paste("P-value: state 1-3=",P_value2))

i=1
hazard.msm(Multistate_model_2[[i]])$`timeperiod[7,9)`
Est1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[7,9)`[1,1])
L1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[7,9)`[1,2])
U1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[7,9)`[1,3])
SE_1 =(U1_limit_1_2-L1_limit_1_2)/(2*1.96)
z_1=abs(Est1_2/SE_1)
P_value1=exp((−0.717*z_1)-(0.416*z_1^2))
print(paste("P-value: state 1-2=",P_value1))

Est1_3=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[7,9)`[2,1])
L2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[7,9)`[2,2])
U2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[7,9)`[2,3])
SE_2 =(U2_limit_1_2-L2_limit_1_2)/(2*1.96)
z_2=abs(Est1_3/SE_2)
P_value2=exp((−0.717*z_2)-(0.416*z_2^2))
print(paste("P-value: state 1-3=",P_value2))

i=1
hazard.msm(Multistate_model_2[[i]])$`timeperiod[9,11)`
Est1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[9,11)`[1,1])
L1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[9,11)`[1,2])
U1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[9,11)`[1,3])
SE_1 =(U1_limit_1_2-L1_limit_1_2)/(2*1.96)
z_1=abs(Est1_2/SE_1)
P_value1=exp((−0.717*z_1)-(0.416*z_1^2))
print(paste("P-value: state 1-2=",P_value1))

Est1_3=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[9,11)`[2,1])
L2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[9,11)`[2,2])
U2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[9,11)`[2,3])
SE_2 =(U2_limit_1_2-L2_limit_1_2)/(2*1.96)
z_2=abs(Est1_3/SE_2)
P_value2=exp((−0.717*z_2)-(0.416*z_2^2))
print(paste("P-value: state 1-3=",P_value2))

i=1
hazard.msm(Multistate_model_2[[i]])$`timeperiod[11,13)`
Est1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[11,13)`[1,1])
L1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[11,13)`[1,2])
U1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[11,13)`[1,3])
SE_1 =(U1_limit_1_2-L1_limit_1_2)/(2*1.96)
z_1=abs(Est1_2/SE_1)
P_value1=exp((−0.717*z_1)-(0.416*z_1^2))
print(paste("P-value: state 1-2=",P_value1))

Est1_3=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[11,13)`[2,1])
L2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[11,13)`[2,2])
U2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[11,13)`[2,3])
SE_2 =(U2_limit_1_2-L2_limit_1_2)/(2*1.96)
z_2=abs(Est1_3/SE_2)
P_value2=exp((−0.717*z_2)-(0.416*z_2^2))
print(paste("P-value: state 1-3=",P_value2))

i=1
hazard.msm(Multistate_model_2[[i]])$`timeperiod[13,15)`
Est1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[13,15)`[1,1])
L1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[13,15)`[1,2])
U1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[13,15)`[1,3])
SE_1 =(U1_limit_1_2-L1_limit_1_2)/(2*1.96)
z_1=abs(Est1_2/SE_1)
P_value1=exp((−0.717*z_1)-(0.416*z_1^2))
print(paste("P-value: state 1-2=",P_value1))

Est1_3=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[13,15)`[2,1])
L2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[13,15)`[2,2])
U2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[13,15)`[2,3])
SE_2 =(U2_limit_1_2-L2_limit_1_2)/(2*1.96)
z_2=abs(Est1_3/SE_2)
P_value2=exp((−0.717*z_2)-(0.416*z_2^2))
print(paste("P-value: state 1-3=",P_value2))

i=1
hazard.msm(Multistate_model_2[[i]])$`timeperiod[15,Inf)`
Est1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[15,Inf)`[1,1])
L1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[15,Inf)`[1,2])
U1_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[15,Inf)`[1,3])
SE_1 =(U1_limit_1_2-L1_limit_1_2)/(2*1.96)
z_1=abs(Est1_2/SE_1)
P_value1=exp((−0.717*z_1)-(0.416*z_1^2))
print(paste("P-value: state 1-2=",P_value1))

Est1_3=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[15,Inf)`[2,1])
L2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[15,Inf)`[2,2])
U2_limit_1_2=log(hazard.msm(Multistate_model_2[[i]])$`timeperiod[15,Inf)`[2,3])
SE_2 =(U2_limit_1_2-L2_limit_1_2)/(2*1.96)
z_2=abs(Est1_3/SE_2)
P_value2=exp((−0.717*z_2)-(0.416*z_2^2))
print(paste("P-value: state 1-3=",P_value2))

#Extracting transition intensity given any covariate (eg. fish sex)
qmatrix.msm(Multistate_model_best,covariates=list(Sex_fish="Male fish"))

plot.survfit.msm(Multistate_model_best,mark.time=F)

#Without parasite load as additional covariate
options(digits=3)
Comparison_output=prevalence.msm(Multistate_model_best, times=seq(1,17,2),censtime=17
               ,ci=c("normal"),B=500,cores=4,interp="start")

#Comparison_output

#plot.prevalence.msm(Multistate_model_2[[1]],mintime=1,maxtime=17)

Lower_limit=as.data.frame(Comparison_output$`Expected percentages`$ci[, ,1])
names(Lower_limit)=c("LL:State1","LL: State2","LL: State3")
rownames(Lower_limit)=c(1,3,5,7,9,11,13,15,17)

Upper_limit=as.data.frame(Comparison_output$`Expected percentages`$ci[, ,2])
names(Upper_limit)=c("UL: State1","UL: State2","UL: State3")
rownames(Upper_limit)=c(1,3,5,7,9,11,13,15,17)
CI_intervals=cbind(Lower_limit,Upper_limit,as.data.frame(Comparison_output$`Expected percentages`$estimates))/100



Observed_percentages=as.data.frame(Comparison_output$"Observed percentages")/100
Expected_percentages=as.data.frame(Comparison_output$"Expected percentages")/100

#ignoring the initial probability of states 1, 2 and 3 respectively (ie: 1,0,0)
observed=c(as.vector(Observed_percentages[,1]),as.vector(Observed_percentages[,2]),as.vector(Observed_percentages[,3]))[-c(1,10,19)]
predicted=c(as.vector(Expected_percentages[,1]),as.vector(Expected_percentages[,2]),as.vector(Expected_percentages[,3]))[-c(1,10,19)]

chi2 = sum((observed- predicted)^2/ predicted)
paste("Chi-square=",chi2,";","p-value=", pchisq(chi2,df=1,lower.tail=FALSE)) 

na.zero <- function (x) {
    x[is.na(x)] <- 0
    return(x)
}


MAPE=function(x,y){(sum(na.zero(abs(as.vector(x)-as.vector(y))/as.vector(x)))*100)/length(as.vector(x))}


observed=c(as.vector(Observed_percentages[,1]),as.vector(Observed_percentages[,2]),as.vector(Observed_percentages[,3]))
predicted=c(as.vector(Expected_percentages[,1]),as.vector(Expected_percentages[,2]),as.vector(Expected_percentages[,3]))


MAPE(x=observed,y=predicted)
print(paste("The prediction accuracy=","",100-MAPE(x=observed,y=predicted),"%"))

Observed_data=Comparison_output$`Observed percentages`/100
Observed_data
Expected_data=Comparison_output$`Expected percentages`$estimates/100
Expected_data

o<-par(mar=c(0,4,2,2))#Run this before the code below


nf<-layout(matrix(1:3, nrow=3,ncol=1))
#layout.show(nf)
    

# rest parameters
par(o)#
o<-par(mar=c(0,4,2,2))
#o<-par(mar=c(0,4,2,2))
plot(Observed_data[,1],type="b",col="grey60",lwd=3,xaxt = "n",ylab="",xlab="",pch=24,cex.axis=1.2,ylim=c(0,1.1))
lines(Expected_data[,1],type="b",col="grey0",lwd=3,pch=19)
lines(CI_intervals[,1],type="l",col="grey0",lty=2,lwd=1.5)
lines(CI_intervals[,4],type="l",col="grey0",lty=2,lwd=1.5)
#axis(1, at=1:9, labels=c("1","3","5","7","9","11","13","15","17"))
text(5,1.1,"Fish remained infected",cex=1.5)





o<-par(mar=c(0,4,0,2))
plot(Observed_data[,2],type="b",col="grey60",lwd=3,xaxt = "n",ylab="Probability",xlab="",ylim=c(0,1.1),pch=24,cex.lab=1.35, cex.axis=1.2)
lines(Expected_data[,2],type="b",col="grey0",lwd=3,pch=19)
lines(CI_intervals[,2],type="l",col="grey0",lty=2,lwd=1.5)
lines(CI_intervals[,5],type="l",col="grey0",lty=2,lwd=1.5)
#axis(1, at=1:9, labels=c("1","3","5","7","9","11","13","15","17"))
text(5,1.1,"Fish alive with loss of infection",cex=1.5)

legend(x = 1.2,y=0.8,inset=0,
        legend = c("Observed","Predicted"), 
        col=c("grey60","grey0"), lwd=4, cex=1.35, horiz = TRUE,pch=c(24,19),pt.cex = 1,box.lwd = 2)


par(mar=c(4,4,0,2))

plot(Observed_data[,3],type="b",col="grey60",lwd=3,xaxt = "n",ylab="",xlab="Time (in days)",pch=24,cex.lab=1.35, cex.axis=1.2
    ,ylim=c(0,1.1))
lines(Expected_data[,3],type="b",col="grey0",lwd=3,pch=19)
lines(CI_intervals[,3],type="l",col="grey0",lty=2,lwd=1.5)
lines(CI_intervals[,6],type="l",col="grey0",lty=2,lwd=1.5)
axis(1, at=1:9, labels=c("1","3","5","7","9","11","13","15","17"))
text(5,1.1,"Fish dead",cex=1.5)

#mtext("Time (in days)", side =1,line = 0, outer=TRUE)
#mtext("Probability", side =2,line = 0, outer=TRUE, las=0)

par(o)






nf<-layout(matrix(1:3, nrow=3,ncol=1))
#layout.show(nf)
    

# rest parameters
par(o)#
o<-par(mar=c(0,4,2,2))
#o<-par(mar=c(0,4,2,2))
plot(Observed_data[,1],type="b",col="blue",lwd=3,xaxt = "n",ylab="",xlab="",pch=24,cex.axis=1.2,ylim=c(0,1.1))
lines(Expected_data[,1],type="b",col="red",lwd=3,pch=19)
lines(CI_intervals[,1],type="l",col="red",lty=2)
lines(CI_intervals[,4],type="l",col="red",lty=2)
#axis(1, at=1:9, labels=c("1","3","5","7","9","11","13","15","17"))
text(5,1.1,"Fish remained infected",cex=1.5)





o<-par(mar=c(0,4,0,2))
plot(Observed_data[,2],type="b",col="blue",lwd=3,xaxt = "n",ylab="Probability",xlab="",ylim=c(0,1.1),pch=24,cex.lab=1.35, cex.axis=1.2)
lines(Expected_data[,2],type="b",col="red",lwd=3,pch=19)
lines(CI_intervals[,2],type="l",col="red",lty=2)
lines(CI_intervals[,5],type="l",col="red",lty=2)
#axis(1, at=1:9, labels=c("1","3","5","7","9","11","13","15","17"))
text(5,1.1,"Fish alive with loss of infection",cex=1.5)

legend(x = 1.2,y=0.8,,inset = 0,
        legend = c("Observed","Predicted"), 
        col=c("blue","red"), lwd=4, cex=1.35, horiz = TRUE,pch=c(24,19),pt.cex = 1,box.lwd = 2)


par(mar=c(4,4,0,2))

plot(Observed_data[,3],type="b",col="blue",lwd=3,xaxt = "n",ylab="",xlab="Time (in days)",pch=24,cex.lab=1.35, cex.axis=1.2
    ,ylim=c(0,1.1))
lines(Expected_data[,3],type="b",col="red",lwd=3,pch=19)
lines(CI_intervals[,3],type="l",col="red",lty=2)
lines(CI_intervals[,6],type="l",col="red",lty=2)
axis(1, at=1:9, labels=c("1","3","5","7","9","11","13","15","17"))
text(5,1.1,"Fish dead",cex=1.5)

#mtext("Time (in days)", side =1,line = 0, outer=TRUE)
#mtext("Probability", side =2,line = 0, outer=TRUE, las=0)

par(o)






graph=function(){

nf<-layout(matrix(1:3, nrow=3,ncol=1))
#layout.show(nf)
    

# rest parameters
par(o)#
o<-par(mar=c(0,4,2,2))
#o<-par(mar=c(0,4,2,2))
plot(Observed_data[,1],type="b",col="blue",lwd=3,xaxt = "n",ylab="",xlab="",pch=24,cex.axis=1.2)
lines(Expected_data[,1],type="b",col="red",lwd=3,pch=19)
lines(CI_intervals[,1],type="l",col="red",lty=2)
lines(CI_intervals[,4],type="l",col="red",lty=2)
#axis(1, at=1:9, labels=c("1","3","5","7","9","11","13","15","17"))
text(5,0.9,"Fish remained infected",cex=1.5)


o<-par(mar=c(0,4,0,2))
plot(Observed_data[,2],type="b",col="blue",lwd=3,xaxt = "n",ylab="Probability",xlab="",ylim=c(0,.3),pch=24,cex.lab=1.35, cex.axis=1.2)
lines(Expected_data[,2],type="b",col="red",lwd=3,pch=19)
lines(CI_intervals[,2],type="l",col="red",lty=2)
lines(CI_intervals[,5],type="l",col="red",lty=2)
#axis(1, at=1:9, labels=c("1","3","5","7","9","11","13","15","17"))
text(5,.28,"Fish alive with loss of infection",cex=1.5)

par(mar=c(4,4,0,2))

plot(Observed_data[,3],type="b",col="blue",lwd=3,xaxt = "n",ylab="",xlab="Time (in days)",pch=24,cex.lab=1.35, cex.axis=1.2)
lines(Expected_data[,3],type="b",col="red",lwd=3,pch=19)
lines(CI_intervals[,3],type="l",col="red",lty=2)
lines(CI_intervals[,6],type="l",col="red",lty=2)
axis(1, at=1:9, labels=c("1","3","5","7","9","11","13","15","17"))
text(5,.69,"Fish dead",cex=1.5)

#mtext("Time (in days)", side =1,line = 0, outer=TRUE)
#mtext("Probability", side =2,line = 0, outer=TRUE, las=0)

par(o)

legend(x = 5.5,y=0.2,,inset = 0,
        legend = c("Observed","Predicted"), 
        col=c("blue","red"), lwd=2, cex=1.35, horiz = TRUE,pch=c(24,19),pt.cex = 1)

}


doc <- read_pptx()


doc <- add_slide(doc, 'Title and Content', 'Office Theme')
doc <- ph_with_vg(doc, code = graph(),type = "body")
#doc <- add_slide(doc, "Title and Content", "Office Theme")


# Write the document to a file
print(doc, target = 'Multistate_comparison.pptx')



#graph()
