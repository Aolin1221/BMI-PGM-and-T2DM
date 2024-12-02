rm(list=ls())
gc()

load("C:/Users/LLLLL/Desktop/UKB/data/complete_dat.RData")
complete_dat$rank_diff_per_st<-scale(complete_dat$rank_diff_per)


####剔除基线患糖尿病的人####
complete_dat<-complete_dat[complete_dat$has_diabetes=="No",]


####敏感性分析1，剔除2年内发生dm结局的人####
complete_dat1<-complete_dat[-which(complete_dat$DM_diagnosis==1 & complete_dat$DM_time<=2),]
dim(complete_dat1) #94155

#计算随访时间和发病密度（补充表5）
table(complete_dat1$DM_diagnosis) #4627
sum(complete_dat1$DM_time) #1136402.136
round(nrow(complete_dat1[complete_dat1$DM_diagnosis==1,])*1000/sum(complete_dat1$DM_time),2) # 4.07


####敏感性分析2，剔除基线肿瘤患者####
complete_dat2<-complete_dat[complete_dat$cancer_diag=="No",]
dim(complete_dat2) #88029

#计算随访时间和发病密度（补充表5）
table(complete_dat2$DM_diagnosis) #4537 
sum(complete_dat2$DM_time) #1063592.55
round(nrow(complete_dat2[complete_dat2$DM_diagnosis==1,])*1000/sum(complete_dat2$DM_time),2) # 4.27



#####敏感性分析3，剔除基线呼吸系统疾病患者####
complete_dat3<-complete_dat[complete_dat$has_respir=="No",]
dim(complete_dat3) #53781

#计算随访时间和发病密度（补充表5）
table(complete_dat3$DM_diagnosis) #2248
sum(complete_dat3$DM_time) #652820.6489
round(nrow(complete_dat3[complete_dat3$DM_diagnosis==1,])*1000/sum(complete_dat3$DM_time),2) # 3.42


####敏感性分析4，mismatch按照三分位数分组####
q<-quantile(complete_dat$rank_diff_per,probs=seq(0,1,by=1/3),na.rm=TRUE)

for(i in 1:nrow(complete_dat)){
  if(complete_dat$rank_diff_per[i]<q[2]){
    complete_dat$rank_3qg[i]<-1
  }
  else if(complete_dat$rank_diff_per[i]>=q[2] & complete_dat$rank_diff_per[i]<q[3]){
    complete_dat$rank_3qg[i]<-0
  }
  else{
    complete_dat$rank_3qg[i]<-2
  }
}

complete_dat$rank_3qg<-factor(complete_dat$rank_3qg,labels = c("Medium","Low","High"))

#计算随访时间和发病密度（补充表5）
table(complete_dat$DM_diagnosis) #4870
sum(complete_dat$DM_time) #1136671
round(nrow(complete_dat[complete_dat$DM_diagnosis==1,])*1000/sum(complete_dat$DM_time),2) # 4.28



####敏感性分析5，mismatch按照五分位数分组####
q<-quantile(complete_dat$rank_diff_per,probs=seq(0,1,by=1/5),na.rm=TRUE)

for(i in 1:nrow(complete_dat)){
  if(complete_dat$rank_diff_per[i]<q[2]){
    complete_dat$rank_5qg[i]<-1
  }
  else if(complete_dat$rank_diff_per[i]>=q[2] & complete_dat$rank_diff_per[i]<q[5]){
    complete_dat$rank_5qg[i]<-0
  }
  else{
    complete_dat$rank_5qg[i]<-2
  }
}

complete_dat$rank_5qg<-factor(complete_dat$rank_5qg,labels = c("Medium","Low","High"))

#计算随访时间和发病密度（补充表5）
table(complete_dat$DM_diagnosis) #4870
sum(complete_dat$DM_time) #1136671
round(nrow(complete_dat[complete_dat$DM_diagnosis==1,])*1000/sum(complete_dat$DM_time),2) # 4.28


####敏感性分析6.1，剔除BMI<1%和＞99%的个体####
upper_99<-quantile(complete_dat$bmi,0.99,na.rm = TRUE)
lower_1<-quantile(complete_dat$bmi,0.01,na.rm = TRUE)
complete_dat4<-complete_dat[complete_dat$bmi>=lower_1&complete_dat$bmi<=upper_99,]
nrow(complete_dat4) #92551

#计算发病例数和发病率（补充表5）
table(complete_dat4$DM_diagnosis) #4669
sum(complete_dat4$DM_time) #1115414.87
round(nrow(complete_dat4[complete_dat4$DM_diagnosis==1,])*1000/sum(complete_dat4$DM_time),2) # 4.19


####敏感性分析6.2，剔除BMI超过±3个标准差的个体####
#计算BMI的均值和标准差
mean_bmi<-mean(complete_dat$bmi, na.rm = TRUE)
sd_bmi<-sd(complete_dat$bmi, na.rm = TRUE)
complete_dat5<-complete_dat[complete_dat$bmi>=(mean_bmi-3*sd_bmi)&complete_dat$bmi<=(mean_bmi+3*sd_bmi),]
nrow(complete_dat5) ##93304

#计算发病例数和发病率（补充表5）
table(complete_dat5$DM_diagnosis) #4651
sum(complete_dat5$DM_time) #1124546.64
round(nrow(complete_dat5[complete_dat5$DM_diagnosis==1,])*1000/sum(complete_dat5$DM_time),2) # 4.14


####不分层Cox循环####
#【需要修改coxph()函数中的data参数和自变量名称以及提取HR值得时候的标签名称】
library(survival)
library(dplyr)

##创建变量清单
models<-c("sex+edu+income",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi")


# 创建结果存储的数据框
result_df<-data.frame()

#Cox循环
for (i in c(1:length(models))) {
  cox_model<-coxph(as.formula(paste0("(Surv(age,time_out,DM_diagnosis))~
                                          rank_qg+strata(region_code)+
                                          strata(age_coxg)+",models[i])),data=complete_dat)
  cox_summary<-summary(cox_model)
  aHRl=round(cox_summary$conf.int[1,1],2)
  aHRll=round(cox_summary$conf.int[1,3],2)
  aHRlu=round(cox_summary$conf.int[1,4],2)
  aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
  aHRu=round(cox_summary$conf.int[2,1],2)
  aHRul=round(cox_summary$conf.int[2, 3],2)
  aHRuu=round(cox_summary$conf.int[2,4],2)
  aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
  result_df<-bind_rows(result_df,data.frame(Model=paste0("Model",i),aHRl=aHRl,
                                            aHRll=aHRll,aHRlu=aHRlu,aHRu=aHRu,
                                            aHRul=aHRul,aHRuu=aHRuu,
                                            "aHRl95%CI"=aHRlCI,
                                            "aHRu95%CI"=aHRuCI
  ))
}

write.csv(result_df,"C:/Users/LLLLL/Desktop/UKB/output/一修/DM_4qg_excluBMI2.csv",row.names = FALSE)


####不分层meta合并####
#【需要修改coxph()函数中的data参数和自变量名称以及HR值对应的标签名称】
library(survival)
library(dplyr)

##创建变量清单
models<-c("sex+edu+income",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi")


# 创建结果存储的数据框
result_df<-data.frame()

#Cox循环
for (i in c(1:length(models))) {
  cox_model<-coxph(as.formula(paste0("(Surv(age,time_out,DM_diagnosis))~
                                          rank_qg+strata(region_code)+
                                          strata(age_coxg)+",models[i])),data=complete_dat5)
  cox_summary<-summary(cox_model)
  aHRl=cox_summary$conf.int[1,1]
  sel=cox_summary$coefficients[1,3]
  aHRu=cox_summary$conf.int[2,1]
  seu=cox_summary$coefficients[2,3]
  result_df<-bind_rows(result_df,data.frame(Model=paste0("Model",i),aHRl=aHRl,
                                            sel=sel,aHRu=aHRu,seu=seu))
}

write.csv(result_df,"C:/Users/LLLLL/Desktop/UKB/output/一修/DM_4qg_meta_excluBMI2.csv",row.names = FALSE)


####不分层Cox_连续变量####
library(survival)
library(survminer)
library(dplyr)


##创建变量清单
models<-c("sex+edu+income",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi")


# 创建结果存储的数据框
result_df<-data.frame()

#Cox
for (i in c(1:4)){
  cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                     "rank_diff_per_st+","+strata(region_code)+
                                   strata(age_coxg)+",models[i])),
                   data=complete_dat4)
  cox_summary<-summary(cox_model)
  hr<-round(cox_summary$conf.int["rank_diff_per_st" , "exp(coef)"],2)
  hrlow<-round(cox_summary$conf.int["rank_diff_per_st" , "lower .95"],2)
  hrup<-round(cox_summary$conf.int["rank_diff_per_st" , "upper .95"],2)
  hrCI<-paste0(hr," (",hrlow,"-",hrup,")")
  result_df<-bind_rows(result_df,data.frame(Model=paste0("Model",i),hr=hr,
                                            hrlow=hrlow,hrup=hrup,
                                            hrCI=hrCI
  ))
  
}

write.csv(result_df,"C:/Users/LLLLL/Desktop/UKB/output/一修/DM_continuous_excluBMI2.csv",row.names = FALSE)


####不分层Cox——连续变量meta####
library(survival)
library(survminer)
library(dplyr)


##创建变量清单
models<-c("sex+edu+income",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi")


# 创建结果存储的数据框
result_df<-data.frame()

#Cox
for (i in c(1:4)){
  cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                     "rank_diff_per_st+","+strata(region_code)+
                                   strata(age_coxg)+",models[i])),
                   data=complete_dat4)
  cox_summary<-summary(cox_model)
  hr<-cox_summary$conf.int["rank_diff_per_st" , "exp(coef)"]
  se<-cox_summary$coefficients["rank_diff_per_st" , "se(coef)"]
  result_df<-bind_rows(result_df,data.frame(Model=paste0("Model",i),hr=hr,
                                            se=se))
  
}

write.csv(result_df,"C:/Users/LLLLL/Desktop/UKB/output/一修/DM_continuous_meta_excluBMI2.csv",row.names = FALSE)


