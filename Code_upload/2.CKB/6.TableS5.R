rm(list=ls())
gc()

# ssh node29   
# source /public/software/apps/env_install_R.sh
# R

load("/public/home/liaolin/BMI-PRS/CKB/data_for_paper/dbqbg.RData")
dee <- read.csv("/public/home/liaolin/BMI-PRS/CKB/DAR-2023-00295-V1/event_endpoints.csv", header=T)

load("C:/Users/LLLLL/Desktop/CKB/data_for_paper/dbqbg.RData")
dee <- read.csv("C:/Users/LLLLL/Desktop/CKB/DAR-2023-00295-V1/event_endpoints.csv", header=T)


####数据处理####

#剔除基线患有dm的人
dbq_DM<-dbqbg[dbqbg$has_diabetes==0,]
dim(dbq_DM) #59873   576


#保留dee中有疾病记录的个体
dee1 <- dee[dee$csid %in% dbq_DM$csid,]  
dim(dee1) #178656     12
length(unique(dee1$csid)) #25399


# 合并 "csid + 发病时间 + 疾病诊断" ,去除诊断重复的纪录（同一天，同一诊断）
dee1$datedeveloped_new <- as.Date(substr(dee1$datedeveloped,1,10))
dee1$date_fulldiag <- paste(dee1$csid,dee1$datedeveloped_new,dee1$diagnosis)  
dee2 <- dee1[!duplicated(dee1$date_fulldiag),]  
dim(dee2) #96204    14


# 剔除 "csid + 疾病诊断" 相同的行,同一个人只保留发病时间最早的记录
dee2 <- dee2[order(dee2$csid,dee2$datedeveloped_new),] 
dee2$date_fulldiag2 <- paste(dee2$csid,dee2$diagnosis) 
dee_norep1 <- dee2[!duplicated(dee2$date_fulldiag2),]  
dim(dee_norep1) #59642    15


# 生成随访患有dm的人E11 (不含HTN)
dee_norep1$disease_diag <- substr(dee_norep1$diagnosis,1,3)
dee_norep1$DM_diagnosis <- NA
dee_norep1$DM_diagnosis <-
  ifelse(
    ((dee_norep1$disease_diag=="E11")), 
    2, 1)   
table(dee_norep1$DM_diagnosis) # 1(57045)     2(2597) 


# 合并 "csid + 疾病诊断（1或者2）"
dee_norep1$disease_yn <- paste(dee_norep1$csid, dee_norep1$DM_diagnosis)  
dee_norep2 <- dee_norep1[!duplicated(dee_norep1$disease_yn),] 
dim(dee_norep2) # 26529    18
table(dee_norep2$DM_diagnosis) # 1(24764)     2(1765)


# 每个对象只保留一条记录，先降序排列，如果同一个人既有2又有1的记录，则保留2
dee_norep2 <- dee_norep2[order(dee_norep2$csid,dee_norep2$disease_yn,decreasing = TRUE),] 
dee_final <- dee_norep2[!duplicated(dee_norep2$csid),] 
dim(dee_final) # 25399    18
table(dee_final$DM_diagnosis) # 1 (23634)    2 (1765)


# 合并数据集
dat_DM <- merge(dbq_DM, dee_final, by="csid", all = TRUE)
dim(dat_DM) # 59873   593 
dat_DM$DM_diagnosis[is.na(dat_DM$DM_diagnosis)] <- 1
table(dat_DM$DM_diagnosis) # 1(58108) 2(1765) 


#生成随访时间
dat_DM$DM_followtime <- NA

# 发生结局的人：哪个时间先出现选哪个时间作终点时间
dat_DM[dat_DM$DM_diagnosis==2,]$DM_followtime <- apply(
  dat_DM[(dat_DM$DM_diagnosis==2),c("censoring_date","datedeveloped_new")], 
  1, min, na.rm =T)

# 未发生结局/censor的人，终点时间为删失时间
dat_DM[dat_DM$DM_diagnosis==1,]$DM_followtime <- 
  dat_DM[dat_DM$DM_diagnosis==1,c("censoring_date")]

#生成随访时间：删失时间 - 基线入组日期(dat_DM$study_date)
dat_DM$DM_followtime2 <- as.Date(substr(dat_DM$DM_followtime,1,10))
dat_DM$study_date_new2 <- as.Date(substr(dat_DM$study_date,1,10))
dat_DM$DM_time_day <- difftime(dat_DM$DM_followtime2, 
                               dat_DM$study_date_new2, units="days")
dat_DM$DM_time <- as.numeric(dat_DM$DM_time_day)/365.25

#以年龄为时间尺度
dat_DM$time_out<-dat_DM$age+dat_DM$DM_time

#按年龄分层

attach(dat_DM)

dat_DM$age_coxg[age >= 30 &  age < 35] <- 1
dat_DM$age_coxg[age >= 35 &  age < 40] <- 2
dat_DM$age_coxg[age >= 40 &  age < 45] <- 3
dat_DM$age_coxg[age >= 45 &  age < 50] <- 4
dat_DM$age_coxg[age >= 50 &  age < 55] <- 5
dat_DM$age_coxg[age >= 55 &  age < 60] <- 6
dat_DM$age_coxg[age >= 60 &  age < 65] <- 7
dat_DM$age_coxg[age >= 65 &  age < 70] <- 8
dat_DM$age_coxg[age >= 70 &  age < 75] <- 9
dat_DM$age_coxg[age >= 75 &  age < 80] <- 10

detach(dat_DM)


####因子化####
library(dplyr)   
datfac_DM <- dat_DM %>%
  mutate(is_female=factor(is_female,labels = c("Men","Women")),
         region=factor(region_is_urban,labels = c("Rural","Urban")),
         region_code=factor(region_code,labels = c("Qingdao","Harbin","Haikou","Suzhou","Liuzhou","Sichuan","Gansu","Henan","Zhejiang","Hunan")),
         edu=factor(edu,labels = c("Primary and below","Middle/high school","College degree or above")),
         income=factor(income,labels = c("<10,000","10,000-19,999",">20,000")),
         smoke_3g=factor(smoke_3g,labels= c("Never","Previous","Current")),
         alcohol_3g=factor(alcohol_3g,labels= c("Never","Previous","Current")),
         pa=factor(pa,labels = c("Q1","Q2","Q3","Q4")),
         sithours=factor(sithours,labels = c("Q1","Q2","Q3")),
         fish_2g=factor(fish_2g,labels = c("<1 d/wk","≥1 d/wk")),
         fruit_2g=factor(fruit_2g,labels = c("<1 d/wk","≥1 d/wk")),
         veg_2g=factor(veg_2g,labels = c("< Daily","Daily")),
         meat_2g=factor(meat_2g,labels = c("≥4 d/wk","<4 d/wk")),
         healthy_diet2=factor(healthy_diet2,labels = c("Unhealthy","Healthy")),
         sleep_hours1=factor(sleep_hours1,labels = c("<7","7-9","≥9")),
         has_hypertension=factor(has_hypertension,labels = c("No","Yes")),
         has_CVD=factor(has_CVD,labels = c("No","Yes")),
         has_respiratory_disease=factor(has_respiratory_disease,labels = c("No","Yes")),
         has_cancer=factor(has_cancer,labels = c("No","Yes")),
         fam_diabetes=factor(fam_diabetes,labels = c("No","Yes","Unknown")),
         rank_qg=factor(rank_qg,labels = c("-25%~+25%","<-25%","≥+25%")),
         bmi4g=factor(bmi4g,labels = c("Low","Normal","Overweight","Obesity")),
         bmi2g=factor(bmi2g,labels = c("Low and normal","Overweight and Obesity"))
  )

datfac_DM$rank_diff_per_st<-scale(datfac_DM$rank_diff_per)
####敏感性分析1，剔除2年内发生dm结局的人####
datfac_DM1<-datfac_DM[-which(datfac_DM$DM_diagnosis==2 & datfac_DM$DM_time<=2),]
dim(datfac_DM1) #59775

#计算发病例数和发病率（补充表5）
table(datfac_DM1$DM_diagnosis) #1667
sum(datfac_DM1$DM_time) #715875.62
round(nrow(datfac_DM1[datfac_DM1$DM_diagnosis==2,])*1000/sum(datfac_DM1$DM_time),2) # 2.33


####敏感性分析2，剔除基线肿瘤患者####
datfac_DM2<-datfac_DM[datfac_DM$has_cancer=="No",]
dim(datfac_DM2) #59584

#计算发病例数和发病率（补充表5）
table(datfac_DM2$DM_diagnosis) #1759
sum(datfac_DM2$DM_time) #712983.68
round(nrow(datfac_DM2[datfac_DM2$DM_diagnosis==2,])*1000/sum(datfac_DM2$DM_time),2) # 2.47


#####敏感性分析3，剔除基线呼吸系统疾病患者####
datfac_DM3<-datfac_DM[datfac_DM$has_respiratory_disease=="No",]
dim(datfac_DM3) #54570

#计算发病例数和发病率（补充表5）
table(datfac_DM3$DM_diagnosis) #1578
sum(datfac_DM3$DM_time) #655905.06
round(nrow(datfac_DM3[datfac_DM3$DM_diagnosis==2,])*1000/sum(datfac_DM3$DM_time),2) # 2.41


####敏感性分析4，mismatch按照三分位数分组####
q<-quantile(datfac_DM$rank_diff_per,probs=seq(0,1,by=1/3),na.rm=TRUE)

for(i in 1:nrow(datfac_DM)){
  if(datfac_DM$rank_diff_per[i]<q[2]){
    datfac_DM$rank_3qg[i]<-1
  }
  else if(datfac_DM$rank_diff_per[i]>=q[2] & datfac_DM$rank_diff_per[i]<q[3]){
    datfac_DM$rank_3qg[i]<-0
  }
  else{
    datfac_DM$rank_3qg[i]<-2
  }
}

datfac_DM$rank_3qg<-factor(datfac_DM$rank_3qg,labels = c("Medium","Low","High"))

#计算发病例数和发病率（补充表5）
table(datfac_DM$DM_diagnosis) #1765
sum(datfac_DM$DM_time) #716000.1
round(nrow(datfac_DM[datfac_DM$DM_diagnosis==2,])*1000/sum(datfac_DM$DM_time),2) # 2.47


####敏感性分析5，mismatch按照五分位数分组####
q<-quantile(datfac_DM$rank_diff_per,probs=seq(0,1,by=1/5),na.rm=TRUE)

for(i in 1:nrow(datfac_DM)){
  if(datfac_DM$rank_diff_per[i]<q[2]){
    datfac_DM$rank_5qg[i]<-1
  }
  else if(datfac_DM$rank_diff_per[i]>=q[2] & datfac_DM$rank_diff_per[i]<q[5]){
    datfac_DM$rank_5qg[i]<-0
  }
  else{
    datfac_DM$rank_5qg[i]<-2
  }
}

datfac_DM$rank_5qg<-factor(datfac_DM$rank_5qg,labels = c("Medium","Low","High"))

#计算发病例数和发病率（补充表5）
table(datfac_DM$DM_diagnosis) #1765
sum(datfac_DM$DM_time) #716000.1
round(nrow(datfac_DM[datfac_DM$DM_diagnosis==2,])*1000/sum(datfac_DM$DM_time),2) # 2.47





####敏感性分析6.1，剔除BMI<1%和＞99%的个体####
upper_99<-quantile(datfac_DM$bmi_calc,0.99,na.rm = TRUE)
lower_1<-quantile(datfac_DM$bmi_calc,0.01,na.rm = TRUE)
datfac_DM4<-datfac_DM[datfac_DM$bmi_calc>=lower_1&datfac_DM$bmi_calc<=upper_99,]
nrow(datfac_DM4) #58720

#计算发病例数和发病率（补充表5）
table(datfac_DM4$DM_diagnosis) #1698
sum(datfac_DM4$DM_time) #703038.20
round(nrow(datfac_DM4[datfac_DM4$DM_diagnosis==2,])*1000/sum(datfac_DM4$DM_time),2) # 2.42


####敏感性分析6.2，剔除BMI超过±3个标准差的个体####
#计算BMI的均值和标准差
mean_bmi<-mean(datfac_DM$bmi_calc, na.rm = TRUE)
sd_bmi<-sd(datfac_DM$bmi_calc, na.rm = TRUE)
datfac_DM5<-datfac_DM[datfac_DM$bmi_calc>=(mean_bmi-3*sd_bmi)&datfac_DM$bmi_calc<=(mean_bmi+3*sd_bmi),]
nrow(datfac_DM5) ##59535

#计算发病例数和发病率（补充表5）
table(datfac_DM5$DM_diagnosis) #1728
sum(datfac_DM5$DM_time) #712084.62
round(nrow(datfac_DM5[datfac_DM5$DM_diagnosis==2,])*1000/sum(datfac_DM5$DM_time),2) # 2.43


####不分层Cox循环（补充表5）####
#【需要修改coxph()函数中的data参数和自变量名称以及HR值对应的标签名称】

library(survival)
library(dplyr)

# 创建变量清单
models<-c("is_female+edu+income",
          "is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi_calc")

# 创建结果存储的数据框
result_df<-data.frame()

for (i in c(1:length(models))) {
  cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                     "rank_qg+","+strata(region_code)+
                                     strata(age_coxg)+",models[i])),
                   data=datfac_DM5)
  cox_summary<-summary(cox_model)
  aHRl=round(cox_summary$conf.int[1,1],2)
  aHRll=round(cox_summary$conf.int[1,3],2)
  aHRlu=round(cox_summary$conf.int[1,4],2)
  aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
  aHRu=round(cox_summary$conf.int[2,1],2)
  aHRul=round(cox_summary$conf.int[2, 3],2)
  aHRuu=round(cox_summary$conf.int[2,4],2)
  aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
  result_df<-bind_rows(result_df,data.frame(Model=paste0("Model",i),
                                            aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                            aHRu=aHRu,aHRul=aHRul,aHRuu=aHRuu,
                                            "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI
  ))
}

write.csv(result_df,"C:/Users/LLLLL/Desktop/CKB/output_paper/一修/DM_4qg_excluBMI2.csv",row.names = FALSE)


####不分层Cox循环meta####
library(survival)
library(survminer)
library(dplyr)

# 创建变量清单
models<-c("is_female+edu+income",
          "is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi_calc")


# 创建结果存储的数据框
result_df<-data.frame()

# Cox 回归循环
for (i in c(1:length(models))) {
  cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                     "rank_qg+","+strata(region_code)+
                                     strata(age_coxg)+",models[i])),
                   data=datfac_DM5)
  cox_summary<-summary(cox_model)
  aHRl=cox_summary$conf.int[1,1]
  sel=cox_summary$coefficients[1,3]
  aHRu=cox_summary$conf.int[2,1]
  seu=cox_summary$coefficients[2,3]
  result_df<-bind_rows(result_df,data.frame(Model=paste0("Model",i),aHRl=aHRl,
                                            sel=sel,aHRu=aHRu,seu=seu))
}

write.csv(result_df,"C:/Users/LLLLL/Desktop/CKB/output_paper/一修/DM_4qg_meta_excluBMI2.csv",row.names = FALSE)


####不分层Cox——连续变量####
library(survival)
library(survminer)
library(dplyr)

# 创建变量清单
models<-c("is_female+edu+income",
          "is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi_calc")

#创建结果储存数据框
result_df<-data.frame()

#Cox
for (i in c(1:4)){
  cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                     "rank_diff_per_st+","+strata(region_code)+
                                   strata(age_coxg)+",models[i])),
                   data=datfac_DM5)
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

write.csv(result_df,"C:/Users/LLLLL/Desktop/CKB/output_paper/一修/DM_continuous_excluBMI2.csv",row.names = FALSE)

####不分层Cox——meta####
library(survival)
library(survminer)
library(dplyr)

# 创建变量清单
models<-c("is_female+edu+income",
          "is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi_calc")

#创建结果储存数据框
result_df<-data.frame()

#Cox
for (i in c(1:4)){
  cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                     "rank_diff_per_st+","+strata(region_code)+
                                   strata(age_coxg)+",models[i])),
                   data=datfac_DM5)
  cox_summary<-summary(cox_model)
  hr<-cox_summary$conf.int["rank_diff_per_st" , "exp(coef)"]
  se<-cox_summary$coefficients["rank_diff_per_st" , "se(coef)"]
  result_df<-bind_rows(result_df,data.frame(Model=paste0("Model",i),hr=hr,
                                            se=se  ))
  
}

write.csv(result_df,"C:/Users/LLLLL/Desktop/CKB/output_paper/一修/DM_continous_meta_excluBMI2.csv",row.names = FALSE)
