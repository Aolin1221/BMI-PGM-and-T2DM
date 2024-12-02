rm(list=ls())
gc()

load("C:/Users/DELL/Desktop/UKB/data/datbq.RData")

####多重插补####
library(mice)

methods_list <- list("eid" = "pmm",
                     "region"="logreg",
                     "edu" = "polyreg",
                     "tdi" = "pmm",
                     "income" = "polyreg",
                     "smoke_3g" = "polyreg" ,
                     "alcohol_3g" = "polyreg",
                     "met" = "pmm",
                     "time_tv" = "pmm",
                     "time_computer" = "pmm",
                     "veg_2g" = "logreg",
                     "fruit_2g" = "logreg",
                     "fish_2g" = "logreg",
                     "meat_2g" = "logreg",
                     "healthy_diet2" ="polyreg",
                     "sleep_hours1" = "polyreg",
                     "has_hypertension" = "logreg",
                     "has_CVD" = "logreg",
                     "has_respir"="logreg",
                     "fam_diabetes" = "logreg",
                     "cancer_diag" = "logreg") 
pred<-quickpred(datbq[,c(1,4,6:21,23:25)])
impute_dat <- mice(datbq[,c(1,4,6:21,23:25)],m=5,maxit=5,method = methods_list,seed=123,pred=pred)


####选择其中的一个数据并合并####
dat<-complete(impute_dat,5)
complete_dat<-merge(dat,datbq[,c(1:3,5,22,26:35)],all.y=TRUE)
nrow(complete_dat)


####对连续变量进行分组####

#tdi_2g
median(complete_dat$tdi) #-1.19
complete_dat$tdi_2g<-ifelse(complete_dat$tdi<median(complete_dat$tdi),0,1)
complete_dat$tdi_2g<-factor(complete_dat$tdi_2g,labels = c("<-Median","≥Median"))


#tdi_5g
quantiles<-quantile(complete_dat$tdi,probs=seq(0,1,by=0.2),na.rm=TRUE)
complete_dat$tdi_5g<-cut( complete_dat$tdi,breaks=quantiles,
                          labels=c(0,1,2,3,4),include.lowest=TRUE)
complete_dat$tdi_5g<-factor(complete_dat$tdi_5g,labels = c("Q1","Q2","Q3","Q4","Q5"))

#PA
qwomen<-quantile(complete_dat[complete_dat$sex=="Women",]$met) 
#0%    25%    50%    75%   100% 
#0.00   8.71  17.09  30.73 133.02 
qmen<-quantile(complete_dat[complete_dat$sex=="Men",]$met) 
#0%    25%    50%    75%   100% 
#0.00  10.00  15.89  26.84 108.77 


for (i in 1:nrow(complete_dat)){
  if((complete_dat$sex[i]=="Women" & complete_dat$met[i]<qwomen[2])|
     (complete_dat$sex[i]=="Men" & complete_dat$met[i]<qmen[2])){
    complete_dat$pa[i]=1     
  }
  else if((complete_dat$sex[i]=="Women" & complete_dat$met[i]<qwomen[3])|
          (complete_dat$sex[i]=="Men" & complete_dat$met[i]<qmen[3])){
    complete_dat$pa[i]=2    
  }
  else if((complete_dat$sex[i]=="Women" & complete_dat$met[i]<qwomen[4])|
          (complete_dat$sex[i]=="Men" & complete_dat$met[i]<qmen[4])){
    complete_dat$pa[i]=3    
  } 
  else{
    complete_dat$pa[i]=4   
  }
}

complete_dat$pa<-factor(complete_dat$pa,labels = c("Q1","Q2","Q3","Q4"))

#sithours
complete_dat$tv_computer_hours=complete_dat$time_tv+complete_dat$time_computer

quantiles<-quantile(complete_dat$tv_computer_hours,probs=seq(0,1,by=1/3),na.rm=TRUE)
complete_dat$sithours<-cut( complete_dat$tv_computer_hours,breaks=quantiles,
                            labels=c(0,1,2),include.lowest=TRUE)
complete_dat$sithours<-factor(complete_dat$sithours,labels = c("Q1","Q2","Q3"))

#age_2g
complete_dat$age_2g<-ifelse(complete_dat$age<60,0,1)
complete_dat$age_2g<-factor(complete_dat$age_2g,labels = c("<60","≥60"))


####保存####
save(complete_dat,file="C:/Users/DELL/Desktop/UKB/data/complete_dat.RData")


####绘制Table1####
library(tableone)

columns<- c("age","sex","region","tdi","tdi_2g","tdi_5g","edu","income","smoke_3g","alcohol_3g","pa",
            "sithours","veg_2g","fruit_2g","fish_2g","meat_2g","healthy_diet2",
            "sleep_hours1","has_hypertension","has_CVD","has_respir",
            "cancer_diag","has_diabetes","fam_diabetes","bmi","SCORE_st")

tab_total<-CreateTableOne(vars=columns,strata=c("rank_qg"),data=complete_dat,addOverall=TRUE)
tab_total<-print(tab_total)
write.csv(tab_total,"C:/Users/DELL/Desktop/UKB/output/0.tableone/1.table_qg_all.csv")

