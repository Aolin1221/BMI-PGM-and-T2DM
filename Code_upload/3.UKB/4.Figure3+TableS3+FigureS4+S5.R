rm(list=ls())
gc()

load("C:/Users/LLLLL/Desktop/UKB/data/complete_dat.RData")


####剔除基线患糖尿病的人####
complete_dat<-complete_dat[complete_dat$has_diabetes=="No",]
nrow(complete_dat) #94398

####计算随访人年数、发病率####
sum(complete_dat$DM_time) #1136670.905
round(median(complete_dat$DM_time),2) #12.40
nrow(complete_dat[complete_dat$DM_diagnosis==1,]) #4870
4870*1000/sum(complete_dat$DM_time)  #4.28%

####计算分组人年数（补充表3）####
library(dplyr)
result <-complete_dat %>%
  group_by(rank_qg) %>%
  summarize(Sum_Value = round(sum(DM_time),2))
write.csv(result,"C:/Users/DELL/Desktop/UKB/output/随访时间/followup_ukb.csv",row.names = FALSE)


#####计算分组发病例数（补充表3）####
table(complete_dat$rank_qg,complete_dat$DM_diagnosis==1)
#         FALSE  TRUE
#-25%~+25% 44826  2402
#<-25%     23872   594
#≥+25%     20830  1874


####不分层Cox循环####
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
  aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
  aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
  aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
  aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
  aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
  aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
  aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
  aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
  result_df<-bind_rows(result_df,data.frame(Model=paste0("Model",i),aHRl=aHRl,
                                            aHRll=aHRll,aHRlu=aHRlu,aHRu=aHRu,
                                            aHRul=aHRul,aHRuu=aHRuu,
                                            "aHRl95%CI"=aHRlCI,
                                            "aHRu95%CI"=aHRuCI
  ))
}

write.csv(result_df,"C:/Users/DELL/Desktop/UKB/output/2.Cox/DM_4qg.csv",row.names = FALSE)


####不分层meta合并（后续和CKB的HR值进行meta的时候要用到）####
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
  aHRl=cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"]
  sel=cox_summary$coefficients["rank_qg<-25%" , "se(coef)"]
  aHRu=cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"]
  seu=cox_summary$coefficients["rank_qg≥+25%" , "se(coef)"]
  result_df<-bind_rows(result_df,data.frame(Model=paste0("Model",i),aHRl=aHRl,
                                            sel=sel,aHRu=aHRu,seu=seu))
}

write.csv(result_df,"C:/Users/DELL/Desktop/UKB/output/2.Cox/DM_4qg_meta.csv",row.names = FALSE)


####不分层Cox——连续变量####
library(survival)
library(survminer)
library(dplyr)

complete_dat$rank_diff_per_st<-scale(complete_dat$rank_diff_per)

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
                   data=complete_dat)
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

write.csv(result_df,"C:/Users/LLLLL/Desktop/UKB/output/DM_continuous.csv",row.names = FALSE)

####不分层Cox——连续变量meta####
library(survival)
library(survminer)
library(dplyr)

complete_dat$rank_diff_per_st<-scale(complete_dat$rank_diff_per)

# 创建变量清单
cov<-c("sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi")

#Cox
cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                   "rank_diff_per_st+","+strata(region_code)+
                                   strata(age_coxg)+",cov)),
                 data=complete_dat)
cox_summary<-summary(cox_model)

hr<-cox_summary$conf.int["rank_diff_per_st" , "exp(coef)"]
se<-cox_summary$coefficients["rank_diff_per_st" , "se(coef)"]
hr #1.127901
se #0.01690175


####PRS分层（图3）####
library(survival)
library(dplyr)

##prs根据三分位数分成3组
q<-quantile(complete_dat$SCORE_st,probs=seq(0,1,by=1/3),na.rm=TRUE)

for(i in 1:length(complete_dat$eid)){
  if(complete_dat$SCORE_st[i]<q[2]){
    complete_dat$prs3g[i]<-0
  }
  else if(complete_dat$SCORE_st[i]>=q[2] & complete_dat$SCORE_st[i]<q[3]){
    complete_dat$prs3g[i]<-1
  }
  else{
    complete_dat$prs3g[i]<-2
  }
}

complete_dat$prs3g<-factor(complete_dat$prs3g,labels = c("Low","Medium","High"))

# 创建变量清单
models<-c("sex+edu+income",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi")

#创建结果储存数据框
result_df<-data.frame()

#数据分层
group_data <- split(complete_dat, complete_dat$prs3g)

#Cox循环
for(j in 1:length(levels(complete_dat$prs3g))){
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("(Surv(age,time_out,DM_diagnosis))~
                                          rank_qg+strata(region_code)+
                                          strata(age_coxg)+",models[i])),
                     data=group_data[[j]])
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    result_df<-bind_rows(result_df,data.frame(PRS3g=levels(complete_dat$prs3g)[j],
                                              Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              aHRu=aHRu,aHRul=aHRul,aHRuu=aHRuu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}


write.csv(result_df,"C:/Users/DELL/Desktop/UKB/output/2.Cox/DM_4qg_byPRS3g.csv",row.names = FALSE)# 输出结果


####PRS分层meta合并（后续和UKB的HR值进行meta的时候要用到）####
result_df<-data.frame()

for(j in 1:length(levels(complete_dat$prs3g))){
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("(Surv(age,time_out,DM_diagnosis))~
                                          rank_qg+strata(region_code)+
                                          strata(age_coxg)+",models[i])),
                     data=group_data[[j]])
    cox_summary<-summary(cox_model)
    aHRl=cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"]
    sel=cox_summary$coefficients["rank_qg<-25%" , "se(coef)"]
    aHRu=cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"]
    seu=cox_summary$coefficients["rank_qg≥+25%" , "se(coef)"]
    result_df<-bind_rows(result_df,data.frame(PRS3g=levels(complete_dat$prs3g)[j],
                                              Model=paste0("Model",i),
                                              aHRl=aHRl,sel=sel,aHRu=aHRu,seu=seu))
  }
}


write.csv(result_df,"C:/Users/DELL/Desktop/UKB/output/2.Cox/DM_4qg_byPRS3g_meta.csv",row.names = FALSE)


####PRS分层——连续变量####
library(survival)
library(survminer)
library(dplyr)

#数据分层
group_data<-split(complete_dat, complete_dat$prs3g)

# 创建变量清单
cov<-c("sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi")

#创建结果储存数据框
result_df<-data.frame()

#Cox
for(i in 1:length(levels(complete_dat$prs3g))){
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
  result_df<-bind_rows(result_df,data.frame(PRS3g=levels(complete_dat$prs3g)[i],
                                            hr=hr,hrlow=hrlow,hrup=hrup,
                                            hrCI=hrCI,pvalue=pvalue))
}
result_df

#    PRS3g   hr hrlow hrup             hrCI pvalue
#1    Low 1.10  0.99 1.22  1.1 (0.99-1.22)  0.068
#2 Medium 1.27  1.17 1.38 1.27 (1.17-1.38)  0.000
#3   High 1.42  1.31 1.53 1.42 (1.31-1.53)  0.000

####PRS分层——连续变量meta####
library(survival)
library(survminer)
library(dplyr)

#数据分层
group_data<-split(complete_dat, complete_dat$prs3g)

# 创建变量清单
cov<-c("sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi")

#创建结果储存数据框
result_df<-data.frame()

#Cox
for(i in 1:length(levels(complete_dat$prs3g))){
  cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                     "rank_diff_per_st+","+strata(region_code)+
                                     strata(age_coxg)+",cov)),
                   data=group_data[[i]])
  cox_summary<-summary(cox_model)
  hr<-cox_summary$conf.int["rank_diff_per_st" , "exp(coef)"]
  se<-cox_summary$coefficients["rank_diff_per_st" , "se(coef)"]
  result_df<-bind_rows(result_df,data.frame(PRS3g=levels(complete_dat$prs3g)[i],
                                            hr=hr,se=se))
}

result_df

write.csv(result_df,"C:/Users/LLLLL/Desktop/Continuous_byPRS3g_UKB_meta.csv",row.names = FALSE)



####BMI分层（图3）####
library(survival)
library(dplyr)


# 创建变量清单
models<-c("sex+edu+income",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi")

#创建结果储存数据框
result_df<-data.frame()

#数据分层
group_data <- split(complete_dat, complete_dat$bmi4g)

#Cox循环
for(j in 1:length(levels(complete_dat$bmi4g))){
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("(Surv(age,time_out,DM_diagnosis))~
                                          rank_qg+strata(region_code)+
                                          strata(age_coxg)+",models[i])),
                     data=group_data[[j]])
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    result_df<-bind_rows(result_df,data.frame(bmi4g=levels(complete_dat$bmi4g)[j],
                                              Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              aHRu=aHRu,aHRul=aHRul,aHRuu=aHRuu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}


write.csv(result_df,"C:/Users/LLLLL/Desktop/UKB/output/6.Cox/DM_4qg_bybmi4g.csv",row.names = FALSE)# 输出结果


####BMI分层meta合并（后续和UKB的HR值进行meta的时候要用到）####
result_df<-data.frame()

for(j in 1:length(levels(complete_dat$bmi4g))){
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("(Surv(age,time_out,DM_diagnosis))~
                                          rank_qg+strata(region_code)+
                                          strata(age_coxg)+",models[i])),
                     data=group_data[[j]])
    cox_summary<-summary(cox_model)
    aHRl=cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"]
    sel=cox_summary$coefficients["rank_qg<-25%" , "se(coef)"]
    aHRu=cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"]
    seu=cox_summary$coefficients["rank_qg≥+25%" , "se(coef)"]
    result_df<-bind_rows(result_df,data.frame(bmi4g=levels(complete_dat$bmi4g)[j],
                                              Model=paste0("Model",i),
                                              aHRl=aHRl,sel=sel,aHRu=aHRu,seu=seu))
  }
}


write.csv(result_df,"C:/Users/LLLLL/Desktop/UKB/output/6.Cox/DM_4qg_bybmi4g_meta.csv",row.names = FALSE)


####BMI分层——连续变量####
library(survival)
library(survminer)
library(dplyr)

#数据分层
group_data<-split(complete_dat, complete_dat$bmi4g)

# 创建变量清单
cov<-c("sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes")

#创建结果储存数据框
result_df<-data.frame()

#Cox
for(i in 1:length(levels(complete_dat$bmi4g))){
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
  result_df<-bind_rows(result_df,data.frame(bmi4g=levels(complete_dat$bmi4g)[i],
                                            hr=hr,hrlow=hrlow,hrup=hrup,
                                            hrCI=hrCI,pvalue=pvalue))
}
result_df

#        bmi4g   hr hrlow hrup             hrCI pvalue
#1        Low 0.73  0.31 1.73 0.73 (0.31-1.73)  0.477
#2     Normal 1.10  0.99 1.23  1.1 (0.99-1.23)  0.069
#3 Overweight 1.15  1.09 1.21 1.15 (1.09-1.21)  0.000
#4    Obesity 1.08  1.03 1.14 1.08 (1.03-1.14)  0.002

####BMI分层——连续变量meta####
library(survival)
library(survminer)
library(dplyr)

#数据分层
group_data<-split(complete_dat, complete_dat$bmi4g)

# 创建变量清单
cov<-c("sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes")

#创建结果储存数据框
result_df<-data.frame()

#Cox
for(i in 1:length(levels(complete_dat$bmi4g))){
  cox_model<-coxph(as.formula(paste0("Surv(","age,","time_out,","DM_diagnosis)~",
                                     "rank_diff_per_st+","+strata(region_code)+
                                     strata(age_coxg)+",cov)),
                   data=group_data[[i]])
  cox_summary<-summary(cox_model)
  hr<-cox_summary$conf.int["rank_diff_per_st" , "exp(coef)"]
  se<-cox_summary$coefficients["rank_diff_per_st" , "se(coef)"]
  result_df<-bind_rows(result_df,data.frame(bmi4g=levels(complete_dat$bmi4g)[i],
                                            hr=hr,se=se))
}

result_df

write.csv(result_df,"C:/Users/LLLLL/Desktop/Continuous_bybmi4g_UKB_meta.csv",row.names = FALSE)




####计算BMI分层的病例数和发病密度####
#计算不同bmi4g水平下的发病例数
case_count<-complete_dat%>%
  filter(DM_diagnosis==1) %>%
  group_by(bmi4g) %>%
  summarise(cases=n())

#计算不同bmi4g水平下的随访人年数
person_years<-complete_dat %>%
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
#1 Low           22       30238.              0.73
#2 Normal       532      366735.              1.45
#3 Overweight  1870      486012.              3.85
#4 Obesity     2446      253686.              9.64


####年龄分层Cox循环（补充图5）####
library(survival)
library(dplyr)

# 创建变量清单
models<-c("sex+edu+income",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi")

#创建结果储存数据框
result_df<-data.frame()

#数据分层
group_data <- split(complete_dat, complete_dat$age_2g)

#Cox循环
for(j in 1:length(levels(complete_dat$age_2g))){
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("(Surv(age,time_out,DM_diagnosis))~
                                          rank_qg+strata(region_code)+
                                          strata(age_coxg)+",models[i])),
                     data=group_data[[j]])
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    result_df<-bind_rows(result_df,data.frame(Age=levels(complete_dat$age_2g)[j],
                                              Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              aHRu=aHRu,aHRul=aHRul,aHRuu=aHRuu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}


write.csv(result_df,"C:/Users/DELL/Desktop/UKB/output/2.Cox/DM_4qg_byAge.csv",row.names = FALSE)


####性别分层2（补充图5）####
library(survival)
library(dplyr)

# 创建变量清单
models<-c("edu+income",
          "edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi")

#创建结果储存数据框
result_df<-data.frame()

#数据分层
group_data <- split(complete_dat, complete_dat$sex)

#Cox循环
for(j in 1:length(levels(complete_dat$sex))){
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("(Surv(age,time_out,DM_diagnosis))~
                                          rank_qg+strata(region_code)+
                                          strata(age_coxg)+",models[i])),
                     data=group_data[[j]])
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    result_df<-bind_rows(result_df,data.frame(Sex=levels(complete_dat$sex)[j],
                                              Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              aHRu=aHRu,aHRul=aHRul,aHRuu=aHRuu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}

write.csv(result_df,"C:/Users/DELL/Desktop/UKB/output/2.Cox/DM_4qg_bySex.csv",row.names = FALSE)

####regon分2层（补充图5）####
library(survival)
library(dplyr)

# 创建变量清单
models<-c("sex+edu+income",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi")

#创建结果储存数据框
result_df<-data.frame()

#数据分层
group_data <- split(complete_dat, complete_dat$region)

#Cox循环
for(j in 1:length(levels(complete_dat$region))){
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("(Surv(age,time_out,DM_diagnosis))~
                                          rank_qg+strata(region_code)+
                                          strata(age_coxg)+",models[i])),
                     data=group_data[[j]])
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    result_df<-bind_rows(result_df,data.frame(TDI=levels(complete_dat$region)[j],
                                              Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              aHRu=aHRu,aHRul=aHRul,aHRuu=aHRuu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}


write.csv(result_df,"C:/Users/DELL/Desktop/UKB/output/2.Cox/DM_4qg_byRegion.csv",row.names = FALSE)


####edu分层（补充图5）####
library(survival)
library(dplyr)

# 创建变量清单
models<-c("sex+income",
          "sex+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "sex+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "sex+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi")

#创建结果储存数据框
result_df<-data.frame()

#数据分层
group_data <- split(complete_dat, complete_dat$edu)

#Cox循环
for(j in 1:length(levels(complete_dat$edu))){
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("(Surv(age,time_out,DM_diagnosis))~
                                          rank_qg+strata(region_code)+
                                          strata(age_coxg)+",models[i])),
                     data=group_data[[j]])
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    result_df<-bind_rows(result_df,data.frame(Edu=levels(complete_dat$edu)[j],
                                              Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              aHRu=aHRu,aHRul=aHRul,aHRuu=aHRuu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}

write.csv(result_df,"C:/Users/DELL/Desktop/UKB/output/2.Cox/DM_4qg_byEdu.csv",row.names = FALSE)


####收入分层（补充图5）####
library(survival)
library(dplyr)

# 创建变量清单
models<-c("sex+edu",
          "sex+edu+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "sex+edu+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "sex+edu+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi")

#创建结果储存数据框
result_df<-data.frame()

#数据分层
group_data <- split(complete_dat, complete_dat$income)

#Cox循环
for(j in 1:length(levels(complete_dat$income))){
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("(Surv(age,time_out,DM_diagnosis))~
                                          rank_qg+strata(region_code)+
                                          strata(age_coxg)+",models[i])),
                     data=group_data[[j]])
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    result_df<-bind_rows(result_df,data.frame(Income=levels(complete_dat$income)[j],
                                              Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              aHRu=aHRu,aHRul=aHRul,aHRuu=aHRuu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}

write.csv(result_df,"C:/Users/DELL/Desktop/UKB/output/2.Cox/DM_4qg_byIncome.csv",row.names = FALSE)


####吸烟分层（补充图5）####
library(survival)
library(dplyr)

# 创建变量清单
models<-c("sex+edu+income",
          "sex+edu+income+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "sex+edu+income+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "sex+edu+income+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi")

#创建结果储存数据框
result_df<-data.frame()

#数据分层
group_data <- split(complete_dat, complete_dat$smoke_3g)

#Cox循环
for(j in 1:length(levels(complete_dat$smoke_3g))){
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("(Surv(age,time_out,DM_diagnosis))~
                                          rank_qg+strata(region_code)+
                                          strata(age_coxg)+",models[i])),
                     data=group_data[[j]])
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    result_df<-bind_rows(result_df,data.frame(Smoke=levels(complete_dat$smoke_3g)[j]
                                              ,Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              aHRu=aHRu,aHRul=aHRul,aHRuu=aHRuu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}

write.csv(result_df,"C:/Users/DELL/Desktop/UKB/output/2.Cox/DM_4qg_bySmoke.csv",row.names = FALSE)

####饮酒分层（补充图5）####
library(survival)
library(dplyr)

# 创建变量清单
models<-c("sex+edu+income",
          "sex+edu+income+smoke_3g+pa+healthy_diet2+sithours+sleep_hours1",
          "sex+edu+income+smoke_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "sex+edu+income+smoke_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi")

#创建结果储存数据框
result_df<-data.frame()

#数据分层
group_data <- split(complete_dat, complete_dat$alcohol_3g)

#Cox循环
for(j in 1:length(levels(complete_dat$alcohol_3g))){
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("(Surv(age,time_out,DM_diagnosis))~
                                          rank_qg+strata(region_code)+
                                          strata(age_coxg)+",models[i])),
                     data=group_data[[j]])
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    result_df<-bind_rows(result_df,data.frame(Alcohol=levels(complete_dat$alcohol_3g)[j],
                                              Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              aHRu=aHRu,aHRul=aHRul,aHRuu=aHRuu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}

write.csv(result_df,"C:/Users/DELL/Desktop/UKB/output/2.Cox/DM_4qg_byAlcohol.csv",row.names = FALSE)


####体力活动分层（补充图5）####
library(survival)
library(dplyr)

# 创建变量清单
models<-c("sex+tdi_5g+edu+income",
          "sex+tdi_5g+edu+income+smoke_3g+alcohol_3g+healthy_diet2+sithours+sleep_hours1",
          "sex+tdi_5g+edu+income+smoke_3g+alcohol_3g+healthy_diet2+sithours+sleep_hours1+fam_diabetes",
          "sex+tdi_5g+edu+income+smoke_3g+alcohol_3g+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi")

#创建结果储存数据框
result_df<-data.frame()

#数据分层
group_data <- split(complete_dat, complete_dat$pa)

#Cox循环
for(j in 1:length(levels(complete_dat$pa))){
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("(Surv(age,time_out,DM_diagnosis))~
                                          rank_qg+strata(region_code)+
                                          strata(age_coxg)+",models[i])),
                     data=group_data[[j]])
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    result_df<-bind_rows(result_df,data.frame(PA=levels(complete_dat$pa)[j],
                                              Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              aHRu=aHRu,aHRul=aHRul,aHRuu=aHRuu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}

write.csv(result_df,"C:/Users/DELL/Desktop/UKB/output/2.Cox/DM_4qg_byPA.csv",row.names = FALSE)


####静坐分层（补充图5）####
library(survival)
library(dplyr)

# 创建变量清单
models<-c("sex+edu+income",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sleep_hours1",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sleep_hours1+fam_diabetes",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sleep_hours1+fam_diabetes+bmi")

#创建结果储存数据框
result_df<-data.frame()

#数据分层
group_data <- split(complete_dat, complete_dat$sithours)

#Cox循环
for(j in 1:length(levels(complete_dat$sithours))){
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("(Surv(age,time_out,DM_diagnosis))~
                                          rank_qg+strata(region_code)+
                                          strata(age_coxg)+",models[i])),
                     data=group_data[[j]])
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    result_df<-bind_rows(result_df,data.frame(Sedentary=levels(complete_dat$sithours)[j]
                                              ,Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              aHRu=aHRu,aHRul=aHRul,aHRuu=aHRuu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}

write.csv(result_df,"C:/Users/DELL/Desktop/UKB/output/2.Cox/DM_4qg_bySedentary.csv",row.names = FALSE)


####饮食分层（补充图5）####
library(survival)
library(dplyr)

# 创建变量清单
models<-c("sex+edu+income",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+sithours+sleep_hours1",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+sithours+sleep_hours1+fam_diabetes",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+sithours+sleep_hours1+fam_diabetes+bmi")

#创建结果储存数据框
result_df<-data.frame()

#数据分层
group_data <- split(complete_dat, complete_dat$healthy_diet2)

#Cox循环
for(j in 1:length(levels(complete_dat$healthy_diet2))){
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("(Surv(age,time_out,DM_diagnosis))~
                                          rank_qg+strata(region_code)+
                                          strata(age_coxg)+",models[i])),
                     data=group_data[[j]])
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    result_df<-bind_rows(result_df,data.frame(Diet=levels(complete_dat$healthy_diet2)[j],
                                              Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              aHRu=aHRu,aHRul=aHRul,aHRuu=aHRuu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}

write.csv(result_df,"C:/Users/DELL/Desktop/UKB/output/2.Cox/DM_4qg_byDiet.csv",row.names = FALSE)


####睡眠分层（补充图5）####
library(survival)
library(dplyr)

# 创建变量清单
models<-c("sex+edu+income",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+fam_diabetes",
          "sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+fam_diabetes+bmi")

#创建结果储存数据框
result_df<-data.frame()

#数据分层
group_data <- split(complete_dat, complete_dat$sleep_hours1)

#Cox循环
for(j in 1:length(levels(complete_dat$sleep_hours1))){
  for (i in c(1:length(models))) {
    cox_model<-coxph(as.formula(paste0("(Surv(age,time_out,DM_diagnosis))~
                                          rank_qg+strata(region_code)+
                                          strata(age_coxg)+",models[i])),
                     data=group_data[[j]])
    cox_summary<-summary(cox_model)
    aHRl=round(cox_summary$conf.int["rank_qg<-25%" , "exp(coef)"],2)
    aHRll=round(cox_summary$conf.int["rank_qg<-25%" , "lower .95"],2)
    aHRlu=round(cox_summary$conf.int["rank_qg<-25%" , "upper .95"],2)
    aHRlCI=paste0(aHRl," (",aHRll,"-",aHRlu,")")
    aHRu=round(cox_summary$conf.int["rank_qg≥+25%" , "exp(coef)"],2)
    aHRul=round(cox_summary$conf.int["rank_qg≥+25%" , "lower .95"],2)
    aHRuu=round(cox_summary$conf.int["rank_qg≥+25%" , "upper .95"],2)
    aHRuCI=paste0(aHRu," (",aHRul,"-",aHRuu,")")
    result_df<-bind_rows(result_df,data.frame(Sleep=levels(complete_dat$sleep_hours1)[j],
                                              Model=paste0("Model",i),
                                              aHRl=aHRl,aHRll=aHRll,aHRlu=aHRlu,
                                              aHRu=aHRu,aHRul=aHRul,aHRuu=aHRuu,
                                              "aHRl95%CI"=aHRlCI,"aHRu95%CI"=aHRuCI))
  }
}

write.csv(result_df,"C:/Users/DELL/Desktop/UKB/output/2.Cox/DM_4qg_bySleep.csv",row.names = FALSE)

####交互作用Cox循环（补充图5）####
library(lmtest)
library(survival)

#无交互项的模型
cox_model0<-coxph(Surv(age,time_out,DM_diagnosis)~rank_qg+sex+edu+income+
                    smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+
                    fam_diabetes+bmi+strata(region_code)+strata(age_coxg),
                  data=complete_dat)

#交互项变量清单
cov<-c("age_2g","sex","edu","region","income","smoke_3g","alcohol_3g","pa",
       "healthy_diet2","sithours","sleep_hours1","fam_diabetes")

#模型协变量
models<-c("+sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi")

#结果储存数据框
result_df<-data.frame()

#循环
for(i in cov){
  cox_model<-coxph(as.formula(paste0("Surv(age,time_out,DM_diagnosis)~rank_qg+strata(region_code)+
strata(age_coxg)+rank_qg",":",i,models)),
                   data=complete_dat)
  P_value<-round(lrtest(cox_model,cox_model0)$`Pr(>Chisq)`,3)
  # 将结果添加到结果数据框
  result_df<-bind_rows(result_df,data.frame(Var=i,P_value=P_value))
  
}

write.csv(result_df,"C:/Users/DELL/Desktop/UKB/output/2.Cox/DM_4qg_Intera_2.csv")


####绘制残差图-all####
library(survminer)
m<-coxph((Surv(age,time_out,DM_diagnosis))~rank_qg+strata(region_code)+
           strata(age_coxg),data = complete_dat)
mzph<-cox.zph(m)

cairo_pdf("C:/Users/DELL/Desktop/UKB/output/3.残差图/1.All.pdf",width=10,height=6)
ggcoxzph(mzph,var="rank_qg",ylab="Mimatch groups",point.size = 0.05)
dev.off()

