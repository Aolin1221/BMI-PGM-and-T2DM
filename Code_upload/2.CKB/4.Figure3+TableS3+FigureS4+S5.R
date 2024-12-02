rm(list=ls())
gc()

# ssh node29   
# source /public/software/apps/env_install_R.sh
# R


load("/public/home/liaolin/BMI-PRS/CKB/data_for_paper/dbqbg.RData")
dee<-read.csv("/public/home/liaolin/BMI-PRS/CKB/data_for_paper/DAR-2023-00295-V1/event_endpoints.csv", header=T)

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


#计算随访时间均数、中位数、随访人年数、粗发病率
round(mean(dat_DM$DM_time),2) # 11.95865,11.96
round(median(dat_DM$DM_time),2) # 12.47365,12.47
round(sum(dat_DM$DM_time),2) # 716000.08
nrow(dat_DM[dat_DM$DM_diagnosis==2,]) #1765
round(nrow(dat_DM[dat_DM$DM_diagnosis==2,])*1000/sum(dat_DM$DM_time),2) # 2.465084, 2.47

#计算分组人年数（补充表3）
library(dplyr)
result <-dat_DM %>%
  group_by(rank_qg) %>%
  summarize(Sum_Value = round(sum(DM_time),2))
write.csv(result,"/public/home/liaolin/BMI-PRS/CKB/output_paper/followup_ckb.csv",row.names = FALSE)

#计算分组发病例数（补充表3）
table(dat_DM$rank_qg,dat_DM$DM_diagnosis==2)
#FALSE  TRUE
#0 29113   820
#1 15025   188
#2 13970   757


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


####不分层Cox循环（补充表3，图3）####
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

#Cox循环
for (i in c(1:length(models))) {
  cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                     "rank_qg+","+strata(region_code)+
                                     strata(age_coxg)+",models[i])),
                   data=datfac_DM)
  cox_summary<-summary(cox_model)
  aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
  aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
  aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
  aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
  P_valuel<-round(cox_summary$coefficients["rank_qg<-25%", "Pr(>|z|)"],3)
  aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
  aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
  aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
  aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
  P_valueu<-round(cox_summary$coefficients["rank_qg≥+25%", "Pr(>|z|)"],3)
  result_df<-bind_rows(result_df,data.frame(Model=paste0("Model",i),aHRl=aHRl,
                                            aHRll=aHRll,aHRlu=aHRlu,
                                            P_valuel=P_valuel,aHRu=aHRu,
                                            aHRul=aHRul,aHRuu=aHRuu,
                                            P_valueu=P_valueu,
                                            "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI
  ))
}

write.csv(result_df,"/public/home/liaolin/BMI-PRS/CKB/output_paper/2.Cox/DM_4qg.csv",row.names = FALSE)


####不分层Cox循环meta（后续和UKB的HR值进行meta的时候要用到）####
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
                   data=datfac_DM)
  cox_summary<-summary(cox_model)
  aHRl=cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"]
  sel=cox_summary$coefficients["rank_qg<-25%" , "se(coef)"]
  aHRu=cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"]
  seu=cox_summary$coefficients["rank_qg≥+25%" , "se(coef)"]
  result_df<-bind_rows(result_df,data.frame(Model=paste0("Model",i),aHRl=aHRl,
                                            aHRu=aHRu,sel=sel,seu=seu))
}

write.csv(result_df,"/public/home/liaolin/BMI-PRS/CKB/output_paper/2.Cox/DM_4qg_meta.csv",row.names = FALSE)


####不分层Cox——连续变量####
library(survival)
library(survminer)
library(dplyr)

datfac_DM$rank_diff_per_st<-scale(datfac_DM$rank_diff_per)

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
                 data=datfac_DM)
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

write.csv(result_df,"C:/Users/LLLLL/Desktop/CKB/output_paper/DM_continuous.csv",row.names = FALSE)


####不分层Cox——连续变量meta####
library(survival)
library(survminer)
library(dplyr)

datfac_DM$rank_diff_per_st<-scale(datfac_DM$rank_diff_per)

# 创建变量清单
cov<-c("is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi_calc")

#Cox
cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                   "rank_diff_per_st+","+strata(region_code)+
                                   strata(age_coxg)+",cov)),
                 data=datfac_DM)
cox_summary<-summary(cox_model)

hr<-cox_summary$conf.int["rank_diff_per_st" , "exp(coef)"]
se<-cox_summary$coefficients["rank_diff_per_st" , "se(coef)"]
hr #1.248896
se # 0.02856988


####年龄分层Cox循环（补充图4）####

#年龄分层变量
datfac_DM$age_2g<-ifelse(datfac_DM$age<60,0,1)
datfac_DM$age_2g<-factor(datfac_DM$age_2g,labels = c("<60","≥60"))

# 创建变量清单
models<-c("+is_female+edu+income",
          "+is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "+is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "+is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi_calc")


# 创建结果存储的数据框
result_df<-data.frame()

# Cox 回归循环
for (j in unique(datfac_DM$age_2g)) {
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                       "rank_qg","+strata(region_code)+
                                     strata(age_coxg)",models[i])),
                     data=subset(datfac_DM,age_2g==j))
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    P_valuel<-round(cox_summary$coefficients["rank_qg<-25%", "Pr(>|z|)"],3)
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    P_valueu<-round(cox_summary$coefficients["rank_qg≥+25%", "Pr(>|z|)"],3)
    result_df<-bind_rows(result_df,data.frame(Age=j,Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              P_valuel=P_valuel,aHRu=aHRu,
                                              aHRul=aHRul,aHRuu=aHRuu,
                                              P_valueu=P_valueu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}

# 输出结果
write.csv(result_df,"/public/home/liaolin/BMI-PRS/CKB/output_paper/2.Cox/DM_4qg_byAge.csv",row.names = FALSE)


####城乡分层Cox循环（补充图4）####

# 创建变量清单
models<-c("+is_female+edu+income",
          "+is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "+is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "+is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi_calc")

# 创建结果存储的数据框
result_df<-data.frame()

# Cox 回归循环
for (j in unique(datfac_DM$region)) {
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                       "rank_qg","+strata(region_code)+
                                     strata(age_coxg)",models[i])),
                     data=subset(datfac_DM,region==j))
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    P_valuel<-round(cox_summary$coefficients["rank_qg<-25%", "Pr(>|z|)"],3)
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    P_valueu<-round(cox_summary$coefficients["rank_qg≥+25%", "Pr(>|z|)"],3)
    result_df<-bind_rows(result_df,data.frame(Region=j,Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              P_valuel=P_valuel,aHRu=aHRu,
                                              aHRul=aHRul,aHRuu=aHRuu,
                                              P_valueu=P_valueu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}

# 输出结果
write.csv(result_df,"/public/home/liaolin/BMI-PRS/CKB/output_paper/2.Cox/DM_4qg_byRegion.csv",row.names = FALSE)


####性别分层Cox循环（补充图4）####

# 创建变量清单
models<-c("+edu+income",
          "+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi_calc")


# 创建结果存储的数据框
result_df<-data.frame()

# Cox 回归循环
for (gender in unique(datfac_DM$is_female)) {
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                       "rank_qg","+strata(region_code)+
                                     strata(age_coxg)",models[i])),
                     data=subset(datfac_DM,is_female==gender))
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    P_valuel<-round(cox_summary$coefficients["rank_qg<-25%", "Pr(>|z|)"],3)
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    P_valueu<-round(cox_summary$coefficients["rank_qg≥+25%", "Pr(>|z|)"],3)
    result_df<-bind_rows(result_df,data.frame(Gender=gender,Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              P_valuel=P_valuel,aHRu=aHRu,
                                              aHRul=aHRul,aHRuu=aHRuu,
                                              P_valueu=P_valueu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}

# 输出结果
write.csv(result_df,"/public/home/liaolin/BMI-PRS/CKB/output_paper/2.Cox/DM_4qg_bygender.csv",row.names = FALSE)


####教育分层Cox循环（补充图4）####
# 创建变量清单
models<-c("+is_female+income",
          "+is_female+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "+is_female+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "+is_female+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi_calc")

# 创建结果存储的数据框
result_df<-data.frame()

# Cox 回归循环
for (j in unique(datfac_DM$edu)) {
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                       "rank_qg","+strata(region_code)+
                                     strata(age_coxg)",models[i])),
                     data=subset(datfac_DM,edu==j))
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    P_valuel<-round(cox_summary$coefficients["rank_qg<-25%", "Pr(>|z|)"],3)
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    P_valueu<-round(cox_summary$coefficients["rank_qg≥+25%", "Pr(>|z|)"],3)
    result_df<-bind_rows(result_df,data.frame(Edu=j,Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              P_valuel=P_valuel,aHRu=aHRu,
                                              aHRul=aHRul,aHRuu=aHRuu,
                                              P_valueu=P_valueu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}

# 输出结果
write.csv(result_df,"/public/home/liaolin/BMI-PRS/CKB/output_paper/2.Cox/DM_4qg_byedu.csv",row.names = FALSE)


####收入分层Cox循环（补充图4）####
# 创建变量清单
models<-c("+is_female+edu",
          "+is_female+edu+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "+is_female+edu+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "+is_female+edu+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi_calc")

# 创建结果存储的数据框
result_df<-data.frame()

# Cox 回归循环
for (j in unique(datfac_DM$income)) {
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                       "rank_qg","+strata(region_code)+
                                     strata(age_coxg)",models[i])),
                     data=subset(datfac_DM,income==j))
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    P_valuel<-round(cox_summary$coefficients["rank_qg<-25%", "Pr(>|z|)"],3)
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    P_valueu<-round(cox_summary$coefficients["rank_qg≥+25%", "Pr(>|z|)"],3)
    result_df<-bind_rows(result_df,data.frame(Income=j,Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              P_valuel=P_valuel,aHRu=aHRu,
                                              aHRul=aHRul,aHRuu=aHRuu,
                                              P_valueu=P_valueu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}

# 输出结果
write.csv(result_df,"/public/home/liaolin/BMI-PRS/CKB/output_paper/2.Cox/DM_4qg_byincome.csv",row.names = FALSE)


####吸烟分层Cox循环（补充图4）####

# 创建变量清单
models<-c("+is_female+edu+income",
          "+is_female+edu+income+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "+is_female+edu+income+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "+is_female+edu+income+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi_calc")

# 创建结果存储的数据框
result_df<-data.frame()

# Cox 回归循环
for (j in unique(datfac_DM$smoke_3g)) {
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                       "rank_qg","+strata(region_code)+
                                     strata(age_coxg)",models[i])),
                     data=subset(datfac_DM,smoke_3g==j))
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    P_valuel<-round(cox_summary$coefficients["rank_qg<-25%", "Pr(>|z|)"],3)
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    P_valueu<-round(cox_summary$coefficients["rank_qg≥+25%", "Pr(>|z|)"],3)
    result_df<-bind_rows(result_df,data.frame(Smoke=j,Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              P_valuel=P_valuel,aHRu=aHRu,
                                              aHRul=aHRul,aHRuu=aHRuu,
                                              P_valueu=P_valueu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}

# 输出结果
write.csv(result_df,"/public/home/liaolin/BMI-PRS/CKB/output_paper/2.Cox/DM_4qg_bysmoke.csv",row.names = FALSE)


####饮酒分层Cox循环（补充图4）####
# 创建变量清单
models<-c("+is_female+edu+income",
          "+is_female+edu+income+smoke_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "+is_female+edu+income+smoke_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "+is_female+edu+income+smoke_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi_calc")

# 创建结果存储的数据框
result_df<-data.frame()

# Cox 回归循环
for (j in unique(datfac_DM$alcohol_3g)) {
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                       "rank_qg","+strata(region_code)+
                                     strata(age_coxg)",models[i])),
                     data=subset(datfac_DM,alcohol_3g==j))
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    P_valuel<-round(cox_summary$coefficients["rank_qg<-25%", "Pr(>|z|)"],3)
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    P_valueu<-round(cox_summary$coefficients["rank_qg≥+25%", "Pr(>|z|)"],3)
    result_df<-bind_rows(result_df,data.frame(Alcohol=j,Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              P_valuel=P_valuel,aHRu=aHRu,
                                              aHRul=aHRul,aHRuu=aHRuu,
                                              P_valueu=P_valueu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}

# 输出结果
write.csv(result_df,"/public/home/liaolin/BMI-PRS/CKB/output_paper/2.Cox/DM_4qg_byalcohol.csv",row.names = FALSE)


####体力活动分层Cox循环（补充图4）####

# 创建变量清单
models<-c("+is_female+edu+income",
          "+is_female+edu+income+smoke_3g+alcohol_3g+healthy_diet2+sithours+sleep_hours1",
          "+is_female+edu+income+smoke_3g+alcohol_3g+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "+is_female+edu+income+smoke_3g+alcohol_3g+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi_calc")

# 创建结果存储的数据框
result_df<-data.frame()

# Cox 回归循环
for (j in unique(datfac_DM$pa)) {
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                       "rank_qg","+strata(region_code)+
                                     strata(age_coxg)",models[i])),
                     data=subset(datfac_DM,pa==j))
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    P_valuel<-round(cox_summary$coefficients["rank_qg<-25%", "Pr(>|z|)"],3)
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    P_valueu<-round(cox_summary$coefficients["rank_qg≥+25%", "Pr(>|z|)"],3)
    result_df<-bind_rows(result_df,data.frame(PA=j,Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              P_valuel=P_valuel,aHRu=aHRu,
                                              aHRul=aHRul,aHRuu=aHRuu,
                                              P_valueu=P_valueu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}

# 输出结果
write.csv(result_df,"/public/home/liaolin/BMI-PRS/CKB/output_paper/2.Cox/DM_4qg_bypa.csv",row.names = FALSE)


####静坐分层Cox循环（补充图4）####

# 创建变量清单
models<-c("+is_female+edu+income",
          "+is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sleep_hours1",
          "+is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sleep_hours1+fam_diabetes",
          "+is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sleep_hours1+fam_diabetes+bmi_calc")


# 创建结果存储的数据框
result_df<-data.frame()

# Cox 回归循环
for (j in unique(datfac_DM$sithours)) {
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                       "rank_qg","+strata(region_code)+
                                     strata(age_coxg)",models[i])),
                     data=subset(datfac_DM,sithours==j))
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    P_valuel<-round(cox_summary$coefficients["rank_qg<-25%", "Pr(>|z|)"],3)
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    P_valueu<-round(cox_summary$coefficients["rank_qg≥+25%", "Pr(>|z|)"],3)
    result_df<-bind_rows(result_df,data.frame(Sedentary=j,Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              P_valuel=P_valuel,aHRu=aHRu,
                                              aHRul=aHRul,aHRuu=aHRuu,
                                              P_valueu=P_valueu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}

# 输出结果
write.csv(result_df,"/public/home/liaolin/BMI-PRS/CKB/output_paper/2.Cox/DM_4qg_bysedentary.csv",row.names = FALSE)


####饮食分层Cox循环（补充图4）####

# 创建变量清单
models<-c("+is_female+edu+income",
          "+is_female+edu+income+smoke_3g+alcohol_3g+pa+sithours+sleep_hours1",
          "+is_female+edu+income+smoke_3g+alcohol_3g+pa+sithours+sleep_hours1+fam_diabetes",
          "+is_female+edu+income+smoke_3g+alcohol_3g+pa+sithours+sleep_hours1+fam_diabetes+bmi_calc")

# 创建结果存储的数据框
result_df<-data.frame()

# Cox 回归循环
for (j in unique(datfac_DM$healthy_diet2)) {
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                       "rank_qg","+strata(region_code)+
                                     strata(age_coxg)",models[i])),
                     data=subset(datfac_DM,healthy_diet2==j))
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    P_valuel<-round(cox_summary$coefficients["rank_qg<-25%", "Pr(>|z|)"],3)
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    P_valueu<-round(cox_summary$coefficients["rank_qg≥+25%", "Pr(>|z|)"],3)
    result_df<-bind_rows(result_df,data.frame(Diet=j,Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              P_valuel=P_valuel,aHRu=aHRu,
                                              aHRul=aHRul,aHRuu=aHRuu,
                                              P_valueu=P_valueu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}

# 输出结果
write.csv(result_df,"/public/home/liaolin/BMI-PRS/CKB/output_paper/2.Cox/DM_4qg_bydiet.csv",row.names = FALSE)


####睡眠分层Cox循环（补充图4）####

# 创建变量清单
models<-c("+is_female+edu+income",
          "+is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours",
          "+is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+fam_diabetes",
          "+is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+fam_diabetes+bmi_calc")

# 创建结果存储的数据框
result_df<-data.frame()

# Cox 回归循环
for (j in unique(datfac_DM$sleep_hours1)) {
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                       "rank_qg","+strata(region_code)+
                                     strata(age_coxg)",models[i])),
                     data=subset(datfac_DM,sleep_hours1==j))
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    P_valuel<-round(cox_summary$coefficients["rank_qg<-25%", "Pr(>|z|)"],3)
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    P_valueu<-round(cox_summary$coefficients["rank_qg≥+25%", "Pr(>|z|)"],3)
    result_df<-bind_rows(result_df,data.frame(Sleep=j,Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              P_valuel=P_valuel,aHRu=aHRu,
                                              aHRul=aHRul,aHRuu=aHRuu,
                                              P_valueu=P_valueu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}

# 输出结果
write.csv(result_df,"/public/home/liaolin/BMI-PRS/CKB/output_paper/2.Cox/DM_4qg_bySleep.csv",row.names = FALSE)


####交互作用Cox循环（补充图4）####
library(lmtest)

#无交互项的模型
cox_model0<-coxph(Surv(age,time_out,DM_diagnosis)~rank_diff_per+is_female+edu+income+
                    smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+
                    fam_diabetes+bmi_calc+strata(region_code)+strata(age_coxg),
                  data=datfac_DM)


#交互项变量清单
cov<-c("age_2g","is_female","region","edu","income","smoke_3g","alcohol_3g","pa",
       "healthy_diet2","sithours","sleep_hours1","fam_diabetes")

#模型协变量
models<-c("+is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi_calc")

#结果储存数据框
result_df<-data.frame()

#循环
for(i in cov){
  cox_model<-coxph(as.formula(paste0("Surv(age,time_out,DM_diagnosis)~rank_qg+strata(region_code)+
                                     strata(age_coxg)+rank_qg",":",i,models)),
                   data=datfac_DM)
  P_value<-round(lrtest(cox_model,cox_model0)[2,5],3)
  result_df<-bind_rows(result_df,data.frame(Var=i,P_value=P_value))
  
}

write.csv(result_df,"/public/home/liaolin/BMI-PRS/CKB/output_paper/2.Cox/DM_4qg_Intera.csv")


####PRS分层（图3）####

#根据PRS三分位数分为3组
q<-quantile(datfac_DM$SCOREst,probs=seq(0,1,by=1/3),na.rm=TRUE)

for(i in 1:nrow(datfac_DM)){
  if(datfac_DM$SCOREst[i]<q[2]){
    datfac_DM$prs3g[i]<-0
  }
  else if(datfac_DM$SCOREst[i]>=q[2] & datfac_DM$SCOREst[i]<q[3]){
    datfac_DM$prs3g[i]<-1
  }
  else{
    datfac_DM$prs3g[i]<-2
  }
}

datfac_DM$prs3g<-factor(datfac_DM$prs3g,labels = c("Low","Medium","High"))

# 创建变量清单
models<-c("is_female+edu+income",
          "is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi_calc")

#创建结果储存数据框
result_df<-data.frame()

# Cox 回归循环
for (prs in unique(datfac_DM$prs3g)) {
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                       "rank_qg+","+strata(region_code)+
                                     strata(age_coxg)+",models[i])),
                     data=subset(datfac_DM,prs3g==prs))
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    result_df<-bind_rows(result_df,data.frame(PRS3g=prs,Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              aHRu=aHRu,aHRul=aHRul,aHRuu=aHRuu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}

write.csv(result_df,"/public/home/liaolin/BMI-PRS/CKB/output_paper/2.Cox/DM_4qg_byPRS3g.csv",row.names = FALSE)

####PRS分层Cox循环meta（后续和UKB的HR值进行meta的时候要用到）####
models<-c("is_female+edu+income",
          "is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi_calc")

#Cox循环

result_df<-data.frame()

# Cox 回归循环
for (prs in unique(datfac_DM$prs3g)) {
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                       "rank_qg+","+strata(region_code)+
                                     strata(age_coxg)+",models[i])),
                     data=subset(datfac_DM,prs3g==prs))
    cox_summary<-summary(cox_model)
    aHRl=cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"]
    sel=cox_summary$coefficients["rank_qg<-25%" , "se(coef)"]
    aHRu=cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"]
    seu=cox_summary$coefficients["rank_qg≥+25%" , "se(coef)"]
    result_df<-bind_rows(result_df,data.frame(PRS3g=prs,Model=paste0("Model",i),
                                              aHRl=aHRl,aHRu=aHRu,sel=sel,seu=seu))
  }
}

write.csv(result_df,"C:/Users/DELL/Desktop/CKB/output_paper/9.Cox/DM_4qg_byPRS3g_meta.csv",row.names = FALSE)# 输出结果

####PRS分层Cox——连续变量####
library(survival)
library(survminer)
library(dplyr)

#数据分层
group_data<-split(datfac_DM, datfac_DM$prs3g)

# 创建变量清单
cov<-c("is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi_calc")

#创建结果储存数据框
result_df<-data.frame()

#Cox
for(i in 1:length(levels(datfac_DM$prs3g))){
  cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                     "rank_diff_per_st+","+strata(region_code)+
                                     strata(age_coxg)+",cov)),
                   data=group_data[[i]])
  cox_summary<-summary(cox_model)
  hr<-round(cox_summary$conf.int["rank_diff_per_st" , "exp(coef)"],2)
  hrlow<-round(cox_summary$conf.int["rank_diff_per_st" , "lower .95"],2)
  hrup<-round(cox_summary$conf.int["rank_diff_per_st" , "upper .95"],2)
  hrCI<-paste0(hr," (",hrlow,"-",hrup,")")
  pvalue<-round(cox_summary$coefficients["rank_diff_per_st", "Pr(>|z|)"],3)
  result_df<-bind_rows(result_df,data.frame(PRS3g=levels(datfac_DM$prs3g)[i],
                                          hr=hr,hrlow=hrlow,hrup=hrup,
                                          hrCI=hrCI,pvalue=pvalue))
}
result_df

#   PRS3g   hr hrlow hrup             hrCI pvalue
#1    Low 1.25  1.00 1.55    1.25 (1-1.55)  0.050
#2 Medium 1.27  1.04 1.56 1.27 (1.04-1.56)  0.021
#3   High 1.39  1.13 1.71 1.39 (1.13-1.71)  0.002

####PRS分层Cox——连续变量meta####
library(survival)
library(survminer)
library(dplyr)

#数据分层
group_data<-split(datfac_DM, datfac_DM$prs3g)

# 创建变量清单
cov<-c("is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi_calc")

#创建结果储存数据框
result_df<-data.frame()

#Cox
for(i in 1:length(levels(datfac_DM$prs3g))){
  cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                     "rank_diff_per_st+","+strata(region_code)+
                                     strata(age_coxg)+",cov)),
                   data=group_data[[i]])
  cox_summary<-summary(cox_model)
  hr<-cox_summary$conf.int["rank_diff_per_st" , "exp(coef)"]
  se<-cox_summary$coefficients["rank_diff_per_st" , "se(coef)"]
  result_df<-bind_rows(result_df,data.frame(PRS3g=levels(datfac_DM$prs3g)[i],
                                            hr=hr,se=se))
}

result_df

write.csv(result_df,"C:/Users/LLLLL/Desktop/Continuous_byPRS3g_CKB_meta.csv",row.names = FALSE)


####BMI分层Cox循环####
# 创建变量清单
models<-c("is_female+edu+income",
          "is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi_calc")

#创建结果储存数据框
result_df<-data.frame()

# Cox 回归循环
for (bmi in unique(datfac_DM$bmi4g)) {
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                       "rank_qg+","+strata(region_code)+
                                     strata(age_coxg)+",models[i])),
                     data=subset(datfac_DM,bmi4g==bmi))
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    result_df<-bind_rows(result_df,data.frame(bmi4g=bmi,Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              aHRu=aHRu,aHRul=aHRul,aHRuu=aHRuu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}

write.csv(result_df,"C:/Users/LLLLL/Desktop/CKB/output_paper/9.Cox/DM_4qg_bybmi4g.csv",row.names = FALSE)

####bmi分层Cox循环meta（后续和UKB的HR值进行meta的时候要用到）####
library(survival)
library(survminer)
library(dplyr)

models<-c("is_female+edu+income",
          "is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi_calc")

#Cox循环

result_df<-data.frame()

# Cox 回归循环
for (bmi in unique(datfac_DM$bmi4g)) {
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                       "rank_qg+","+strata(region_code)+
                                     strata(age_coxg)+",models[i])),
                     data=subset(datfac_DM,bmi4g==bmi))
    cox_summary<-summary(cox_model)
    aHRl=cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"]
    sel=cox_summary$coefficients["rank_qg<-25%" , "se(coef)"]
    aHRu=cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"]
    seu=cox_summary$coefficients["rank_qg≥+25%" , "se(coef)"]
    result_df<-bind_rows(result_df,data.frame(bmi4g=bmi,Model=paste0("Model",i),
                                              aHRl=aHRl,sel=sel,aHRu=aHRu,seu=seu))
  }
}

write.csv(result_df,"C:/Users/LLLLL/Desktop/CKB/output_paper/9.Cox/DM_4qg_bybmi4g_meta.csv",row.names = FALSE)# 输出结果

####bmi分层Cox——连续变量####
library(survival)
library(survminer)
library(dplyr)

#数据分层
group_data<-split(datfac_DM, datfac_DM$bmi4g)

# 创建变量清单
cov<-c("is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes")

#创建结果储存数据框
result_df<-data.frame()

#Cox
for(i in 1:length(levels(datfac_DM$bmi4g))){
  cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                     "rank_diff_per_st+","+strata(region_code)+
                                     strata(age_coxg)+",cov)),
                   data=group_data[[i]])
  cox_summary<-summary(cox_model)
  hr<-round(cox_summary$conf.int["rank_diff_per_st" , "exp(coef)"],2)
  hrlow<-round(cox_summary$conf.int["rank_diff_per_st" , "lower .95"],2)
  hrup<-round(cox_summary$conf.int["rank_diff_per_st" , "upper .95"],2)
  hrCI<-paste0(hr," (",hrlow,"-",hrup,")")
  pvalue<-round(cox_summary$coefficients["rank_diff_per_st", "Pr(>|z|)"],3)
  result_df<-bind_rows(result_df,data.frame(bmi4g=levels(datfac_DM$bmi4g)[i],
                                            hr=hr,hrlow=hrlow,hrup=hrup,
                                            hrCI=hrCI,pvalue=pvalue))
}
result_df

#        bmi4g   hr hrlow hrup             hrCI pvalue
#1        Low 1.31  0.75 2.28 1.31 (0.75-2.28)  0.337
#2     Normal 1.44  1.29 1.59 1.44 (1.29-1.59)  0.000
#3 Overweight 1.27  1.17 1.38 1.27 (1.17-1.38)  0.000
#4    Obesity 1.20  1.07 1.35  1.2 (1.07-1.35)  0.003

####bmi分层Cox——连续变量meta####
library(survival)
library(survminer)
library(dplyr)

#数据分层
group_data<-split(datfac_DM, datfac_DM$bmi4g)

# 创建变量清单
cov<-c("is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes")

#创建结果储存数据框
result_df<-data.frame()

#Cox
for(i in 1:length(levels(datfac_DM$bmi4g))){
  cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                     "rank_diff_per_st+","+strata(region_code)+
                                     strata(age_coxg)+",cov)),
                   data=group_data[[i]])
  cox_summary<-summary(cox_model)
  hr<-cox_summary$conf.int["rank_diff_per_st" , "exp(coef)"]
  se<-cox_summary$coefficients["rank_diff_per_st" , "se(coef)"]
  result_df<-bind_rows(result_df,data.frame(bmi4g=levels(datfac_DM$bmi4g)[i],
                                            hr=hr,se=se))
}

result_df

write.csv(result_df,"C:/Users/LLLLL/Desktop/Continuous_bybmi4g_CKB_meta.csv",row.names = FALSE)



####计算BMI分层的病例数和发病密度####
#计算不同bmi4g水平下的发病例数
case_count<-datfac_DM%>%
  filter(DM_diagnosis==2) %>%
  group_by(bmi4g) %>%
  summarise(cases=n())

#计算不同bmi4g水平下的随访人年数
person_years<-datfac_DM %>%
  group_by(bmi4g) %>%
  summarise(person_years=sum(DM_time, na.rm=TRUE))

#整合发病例数和随访人年数
result<-case_count %>%
  left_join(person_years, by="bmi4g")

#计算不同bmi4g水平下的发病密度
result <- result %>%
  mutate(incidence_density=round(cases*1000/person_years,2))

#   bmi4g      cases person_years incidence_density
#  <fct>      <int>        <dbl>             <dbl>
#1 Low           33       29435.              1.12
#2 Normal       501      365322.              1.37
#3 Overweight   780      241487.              3.23
#4 Obesity      451       79756.              5.65

####绘制残差图-all####
library(survminer)
m<-coxph(Surv(age,time_out,DM_diagnosis)~rank_qg+strata(age_coxg)+strata(region_code),
         data=datfac_DM)
mzph<-cox.zph(m)

cairo_pdf("/public/home/liaolin/BMI-PRS/CKB/output_paper/3.残差图/1.All.pdf",width=10,height=6)
ggcoxzph(mzph,var="rank_qg",ylab="Mimatch groups",point.size = 0.05)
dev.off()



