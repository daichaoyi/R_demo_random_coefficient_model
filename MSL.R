rm(list=ls())
library(nloptr)
library(haven)
library(datasets)
library(ggplot2)
library(data.table)
library(dplyr)
library(abind)

setwd("C:/Users/daichaoyi1/Desktop/Random Coefficient")

## import data
sample<-read_dta("group1_indicator.dta")

observation<-data.table(sample)
observation<-observation[invent==0  &  category>=0  &  price>0  &  price<5000  &  obs_enemyhero>=0  &  obs_is_win>=0  &  num_session>=0  &  among_inventory>=0  &  purchase_item>=0  &  sales>=0  &  released_lastmonth>=0]
observation[,constant:=1]

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
observation0<-observation0[,c("identifier","constant","category","indicator","obs_enemyhero","obs_square","obs_is_win","is_win","num_session_before","among_inventory","num_skin","sales","released_lastmonth","price")]
spt0<-split(observation0, observation0$identifier) 


#spt1 list of sessions that purchase happen 
observation1<-observation1[,c("identifier","constant","category","indicator","obs_enemyhero","obs_square","obs_is_win","is_win","num_session_before","among_inventory","num_skin","sales","released_lastmonth","price")]
spt1<-split(observation1, observation1$identifier) 


#spt2 list of sessions that has no purchase 
observation2<-observation2[,c("identifier","constant","category","indicator","obs_enemyhero","obs_square","obs_is_win","is_win","num_session_before","among_inventory","num_skin","sales","released_lastmonth","price")]
spt2<-split(observation2, observation2$identifier) 


# N1- number of purchase happen(count number of list)
N1<-length(spt1)

# N2- number of non-purchase happen
N2<-length(spt2)


#shell for likelihood1
vi1=matrix(rnorm(N1*NS,mean=0,sd=1), N1, NS) 
vi2=matrix(rnorm(N1*NS,mean=0,sd=1), N1, NS) 
vi3=matrix(rnorm(N1*NS,mean=0,sd=1), N1, NS) 
vi4=matrix(rnorm(N1*NS,mean=0,sd=1), N1, NS) 
vi5=matrix(rnorm(N1*NS,mean=0,sd=1), N1, NS) 
vi6=matrix(rnorm(N1*NS,mean=0,sd=1), N1, NS) 
vi7=matrix(rnorm(N1*NS,mean=0,sd=1), N1, NS) 
vi8=matrix(rnorm(N1*NS,mean=0,sd=1), N1, NS) 
vi9=matrix(rnorm(N1*NS,mean=0,sd=1), N1, NS) 
vi10=matrix(rnorm(N1*NS,mean=0,sd=1), N1, NS)
vi11=matrix(rnorm(N1*NS,mean=0,sd=1), N1, NS)
vi12=matrix(rnorm(N1*NS,mean=0,sd=1), N1, NS)
vi13=matrix(rnorm(N1*NS,mean=0,sd=1), N1, NS)

v0<-array(0, dim=c(1,1,N1,NS))
v1<-array(vi1, dim=c(1,1,N1,NS))
v2<-array(vi2, dim=c(1,1,N1,NS))
v3<-array(vi3, dim=c(1,1,N1,NS))
v4<-array(vi4, dim=c(1,1,N1,NS))
v5<-array(vi5, dim=c(1,1,N1,NS))
v6<-array(vi6, dim=c(1,1,N1,NS))
v7<-array(vi7, dim=c(1,1,N1,NS))
v8<-array(vi8, dim=c(1,1,N1,NS))
v9<-array(vi9, dim=c(1,1,N1,NS))
v10<-array(vi10, dim=c(1,1,N1,NS))
v11<-array(vi11, dim=c(1,1,N1,NS))
v12<-array(vi12, dim=c(1,1,N1,NS))
v13<-array(vi13, dim=c(1,1,N1,NS))


#shell for likelihood2
si1=matrix(rnorm(N2*NS,mean=0,sd=1), N2, NS) 
si2=matrix(rnorm(N2*NS,mean=0,sd=1), N2, NS) 
si3=matrix(rnorm(N2*NS,mean=0,sd=1), N2, NS) 
si4=matrix(rnorm(N2*NS,mean=0,sd=1), N2, NS) 
si5=matrix(rnorm(N2*NS,mean=0,sd=1), N2, NS) 
si6=matrix(rnorm(N2*NS,mean=0,sd=1), N2, NS) 
si7=matrix(rnorm(N2*NS,mean=0,sd=1), N2, NS) 
si8=matrix(rnorm(N2*NS,mean=0,sd=1), N2, NS) 
si9=matrix(rnorm(N2*NS,mean=0,sd=1), N2, NS) 
si10=matrix(rnorm(N2*NS,mean=0,sd=1), N2, NS)
si11=matrix(rnorm(N2*NS,mean=0,sd=1), N2, NS)
si12=matrix(rnorm(N2*NS,mean=0,sd=1), N2, NS)
si13=matrix(rnorm(N2*NS,mean=0,sd=1), N2, NS)


s0<-array(0, dim=c(1,1,N2,NS))
s1<-array(si1, dim=c(1,1,N2,NS))
s2<-array(si2, dim=c(1,1,N2,NS))
s3<-array(si3, dim=c(1,1,N2,NS))
s4<-array(si4, dim=c(1,1,N2,NS))
s5<-array(si5, dim=c(1,1,N2,NS))
s6<-array(si6, dim=c(1,1,N2,NS))
s7<-array(si7, dim=c(1,1,N2,NS))
s8<-array(si8, dim=c(1,1,N2,NS))
s9<-array(si9, dim=c(1,1,N2,NS))
s10<-array(si10, dim=c(1,1,N2,NS))
s11<-array(si11, dim=c(1,1,N2,NS))
s12<-array(si12, dim=c(1,1,N2,NS))
s13<-array(si13, dim=c(1,1,N2,NS))


#Likelihood function for those who has purchased
Likelihood1<-function(parameter){
#  parameter<-c(-7.52,0.5,-0.43,0.04,0.01,0.001,0.5,0.05,0.01,0.001,-0.27,0.02,-0.055,0.005,-0.125,0.01,-0.0636,0.005,-0.452,0.05,0.424,0.05,-0.458,0.05,-0.02,0.005)
  #generate beta
  constant<-parameter[1]+parameter[2]*v1
  beta1<-parameter[3]+parameter[4]*v2
  beta2<-parameter[5]+parameter[6]*v3
  beta3<-parameter[7]+parameter[8]*v4
  beta4<-parameter[9]+parameter[10]*v5
  beta5<-parameter[11]+parameter[12]*v6
  beta6<-parameter[13]+parameter[14]*v7
  beta7<-parameter[15]+parameter[16]*v8
  beta8<-parameter[17]+parameter[18]*v9
  beta9<-parameter[19]+parameter[20]*v10
  beta10<-parameter[21]+parameter[22]*v11
  beta11<-parameter[23]+parameter[24]*v12
  beta12<-parameter[25]+parameter[26]*v13
  
  beta_n1<-abind(v0,constant,beta1,beta2,beta3,beta4,beta5,beta6,beta7,beta8,beta9,beta10,beta11,beta12,along=1)
  
  spt0<-lapply(spt0,FUN=function(x) as.matrix(x))
  spt1<-lapply(spt1,FUN=function(x) as.matrix(x))

  #calculate the utility for each choice
  choice<-lapply(seq_along(spt0),FUN=function(i) spt0[[i]]%*%beta_n1[,,i,]) 
  utility1<-lapply(seq_along(spt1),FUN=function(i) spt1[[i]]%*%beta_n1[,,i,])
  
  choice<-lapply(choice,FUN=function(x) as.matrix(x))
  utility1<-lapply(utility1,FUN=function(x) as.matrix(x))
  
  #take exponential for utility
  exp_choice<-lapply(seq_along(choice),FUN=function(i) exp(choice[[i]]))
  exp_utility1<-lapply(seq_along(utility1),FUN=function(i) exp(utility1[[i]]))
  
  #sum up the utility for each observation under a specified simulation result
  sum_utility1<-lapply(seq_along(utility1),FUN=function(i) 1+colSums(exp_utility1[[i]]))  

  #calculate probability for each simulation
  li1<-lapply(seq_along(exp_choice), FUN=function(i)  exp_choice[[i]]/sum_utility1[[i]])
  
  #take average for simulations
  mean_li1<-lapply(seq_along(li1), FUN=function(i)  mean(li1[[i]])) 
  
  #take log 
  log_li1<-lapply(seq_along(mean_li1), FUN=function(i) log(mean_li1[[i]]))
  
  result1<-Reduce("+", log_li1) 
return(-result1)
}





#Likelihood function for those who didn't purchase

Likelihood2<-function(parameter){ 
  #generate beta
#  parameter<-c(-7.52,0.5,-0.43,0.04,0.01,0.001,0.5,0.05,0.01,0.001,-0.27,0.02,-0.055,0.005,-0.125,0.01,-0.0636,0.005,-0.452,0.05,0.424,0.05,-0.458,0.05,-0.02,0.005)

  constant<-parameter[1]+parameter[2]*s1
  beta1<-parameter[3]+parameter[4]*s2
  beta2<-parameter[5]+parameter[6]*s3
  beta3<-parameter[7]+parameter[8]*s4
  beta4<-parameter[9]+parameter[10]*s5
  beta5<-parameter[11]+parameter[12]*s6
  beta6<-parameter[13]+parameter[14]*s7
  beta7<-parameter[15]+parameter[16]*s8
  beta8<-parameter[17]+parameter[18]*s9
  beta9<-parameter[19]+parameter[20]*s10
  beta10<-parameter[21]+parameter[22]*s11
  beta11<-parameter[23]+parameter[24]*s12
  beta12<-parameter[25]+parameter[26]*s13
  
  
  beta_n2<-abind(s0,constant,beta1,beta2,beta3,beta4,beta5,beta6,beta7,beta8,beta9,beta10,beta11,beta12,along=1)
  spt2<-lapply(spt2,FUN=function(x) as.matrix(x))
  
  # calculate the utility for each choice
  utility2<-lapply(seq_along(spt2),FUN=function(i) spt2[[i]]%*%beta_n2[,,i,])

  # utility take exp()
  utility2<-lapply(utility2,FUN=function(x) as.matrix(x))
  exp_utility2<-lapply(seq_along(utility2),FUN=function(i) exp(utility2[[i]]))
  sum_utility2<-lapply(seq_along(exp_utility2),  FUN=function(i)  1+colSums(exp_utility2[[i]])) 
  

  # calculate probability under each simulation
  li2<-lapply(seq_along(sum_utility2), FUN=function(i)  1/sum_utility2[[i]])
  
  # take mean for simulations 
  mean_li2<-lapply(seq_along(li2), FUN=function(i) mean(li2[[i]]))
  
  #take log 
  log_li2<-lapply(seq_along(mean_li2), FUN=function(i) log(mean_li2[[i]]))
  
  # sum over all observations
  result2<-Reduce("+",log_li2) 
return(-result2)
}


SML<-function(parameter){
  time1<-Sys.time()
  system.time({
  L1<-Likelihood1(parameter)
  L2<-Likelihood2(parameter)}
  )
  return((L1+L2))
}


###  5)  Maximum Simulated Likelihood Estimation
# optim performs minimization, but it will maximize if objective is negative
# optim(), Starting value, Function to optimize, optimizing method
est<-optim(c(-7.52,0.5,-0.43,0.04,0.01,0.001,0.5,0.05,0.01,0.001,-0.27,0.02,-0.055,0.005,-0.125,0.01,-0.0636,0.005,-0.452,0.05,0.424,0.05,-0.458,0.05,-0.02,0.005), SML, method ="BFGS")   
est$par



