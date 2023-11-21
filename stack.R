rm(list=ls())
library(nloptr)
library(haven)
library(datasets)
library(ggplot2)
library(data.table)
library(dplyr)
library(abind)
setwd("C:/Users/Chaoyi/Desktop/Random Coefficient")

## import data
sample<-read_dta("group1_new.dta")

observation<-data.table(sample)
observation<-observation[invent==0 & category>=0 & price>0 & price<5000 & obs_enemyhero>=0 & obs_is_win>=0 & num_session>=0 & among_inventory>=0  & purchase_item>=0 & sales>=0 & released_lastmonth>=0]


#count number of observation
num_obs<-nrow(observation[,.N, by=list(r,sessionid)])

# number of simulation
NS=100    
# create group identifier
identifier<-seq(1,num_obs)


# create groupid 
observation$identifier<-observation%>%group_by(r, sessionid)%>% group_indices
observation[,if_purchase:=max(purchase_item), by=list(identifier)]
observation1<-observation[if_purchase==1]
observation2<-observation[if_purchase==0]

# observation0 contain only the choice that has been purchased
observation0<-observation1[purchase_item==1]

#spt0 list of choice that has been purchased
observation0<-observation0[,c("identifier","num_skin","price")]
spt0<-split(observation0, observation0$identifier) 


#spt1 list of sessions that purchase happen 
observation1<-observation1[,c("identifier","num_skin","price")]
spt1<-split(observation1, observation1$identifier) 

#spt2 list of sessions that has no purchase 
observation2<-observation2[,c("identifier","num_skin","price")]
spt2<-split(observation2, observation2$identifier) 


# N1- number of purchase happen(count number of list)
N1<-length(spt1)

# N2- number of non-purchase happen
N2<-length(spt2)

#do.call("abind",c(spt1,list(along=3))) 

#Likelihood function for those who has purchased

Likelihood1<-function(parameter){
  vi1=matrix(rnorm(N1,mean=0,sd=1), 1, N1) 
  vi2=matrix(rnorm(N1,mean=0,sd=1), 1, N2) 
  v0<-array(0, dim=c(1,1,N1))
  v1<-array(vi1, dim=c(1,1,N1))
  v2<-array(vi2, dim=c(1,1,N1))
  #generate beta
  beta1<-parameter[1]+parameter[2]*v1
  beta2<-parameter[3]+parameter[4]*v2
  
  beta<-abind(v0,beta1,beta2,along=1)
  spt0<-lapply(spt0,FUN=function(x) as.matrix(x))
  spt1<-lapply(spt1,FUN=function(x) as.matrix(x))
  
  # calculate the utility for each choice
  choice<-lapply(seq_along(spt0),FUN= function(i) spt0[[i]]%*%beta[,,i]) 
  utility1<-lapply(seq_along(spt1),FUN=function(i) spt1[[i]]%*%beta[,,i])
  
  #utility take exp()
  choice<-lapply(choice,FUN=function(x) exp(as.numeric(x)))
  # utility1 represents the utility for each choice among the choice set
  
  #utility take exp()
  sum_utility1<-lapply(seq_along(utility1), FUN=function(i) sum(exp(as.numeric(utility1[[i]]))))
  li1<-lapply(seq_along(choice), FUN=function(i) log(choice[[i]]/(1+sum_utility1[[i]])))
  li1<-Reduce("+", li1) 
return(-li1)
}



#Likelihood function for those who didn't purchase

Likelihood2<-function(parameter){                          
  vi1=matrix(rnorm(N2,mean=0,sd=1), 1, N2) 
  vi2=matrix(rnorm(N2,mean=0,sd=1), 1, N2) 
  v0<-array(0, dim=c(1,1,N2))
  v1<-array(vi1, dim=c(1,1,N2))
  v2<-array(vi2, dim=c(1,1,N2))
  #generate beta
  beta1<-parameter[1]+parameter[2]*v1
  beta2<-parameter[3]+parameter[4]*v2
  
  beta<-abind(v0,beta1,beta2,along=1)
  spt2<-lapply(spt0,FUN=function(x) as.matrix(x))
  
  # calculate the utility for each choice
  utility2<-lapply(seq_along(spt2),FUN=function(i) spt2[[i]]%*%beta[,,i])
  
  #utility take exp()
  sum_utility2<-lapply(seq_along(utility2), FUN=function(i) sum(exp(as.numeric(utility2[[i]]))))
  li2<-lapply(seq_along(sum_utility2), FUN=function(i) log(1/(1+sum_utility2[[i]])))
  li2<-Reduce("+", li2) 
return(-li2)
}


SML<-function(parameter){
  L1<-Likelihood1(parameter)
  L2<-Likelihood2(parameter)
  return((L1+L2))
}


###  5)  Maximum Likelihood Estimation
# optim performs minimization, but it will maximize if objective is negative
# optim(), Starting value, Function to optimize, optimizing method
est<-optim(c(0.1, 0.03, -0.001, 0.03), SML, method ="BFGS")   
est$par


#  L=0  
#  library(doParallel)
#  cl <- makeCluster(8)
#  registerDoParallel(cl)
#  result<-foreach(i=1:10, .combine="+") %dopar%{  
#    library(data.table)

#  L=0  
#  library(doParallel)
#  cl <- makeCluster(8)
#  registerDoParallel(cl)
#  result<-foreach(i=1:10, .combine="+") %dopar%{  
#    library(data.table)

