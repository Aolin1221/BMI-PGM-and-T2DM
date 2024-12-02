rm(list=ls())
gc()

####配色方案####
library(RColorBrewer)
ckb<-brewer.pal(9,"Reds")[6]
ukb<-brewer.pal(9,"Blues")[6]
line<-brewer.pal(8,"Set2")[2]


####ckb####
load("/public/home/liaolin/BMI-PRS/CKB/data_for_paper/dbqbg.RData")

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

dat_DM$age_coxg<-as.factor(dat_DM$age_coxg)

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
         rank_qg=factor(rank_qg,labels = c("concordant BMI-PGM","discordantly low BMI-PGM","discordantly high BMI-PGM")),
         bmi4g=factor(bmi4g,labels = c("Low","Normal","Overweight","Obesity")),
         bmi2g=factor(bmi2g,labels = c("Low and normal","Overweight and Obesity")),
  )



####限制性立方样条图_CKB####
library(survival)
library(Hmisc)
library(plyr)
library(ggplot2)
library(rms)


dat_m <- subset(datfac_DM, 
                select = c("age","time_out","DM_time","DM_diagnosis","rank_qg",
                           "is_female","edu","income","smoke_3g","alcohol_3g",
                           "pa","healthy_diet2","sithours","sleep_hours1",
                           "fam_diabetes","bmi_calc", "region_code",
                           "age_coxg","rank_diff_per"))
ddm <- datadist(dat_m)
options(datadist='ddm')

fitm_5knots_stra <- cph(Surv(DM_time,DM_diagnosis) ~ rcs(rank_diff_per,5)+is_female+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi_calc+strat(region_code)+ strat(age_coxg), data=dat_m)
# 非线性检验：P<0.05为存在非线性关系
anova(fitm_5knots_stra) # P-Nonlinear 0.5637


# ddm$limits$children[2] <- 3 # 设置参考点，也就是HR为1的点，常见的为中位数或者临床有意义的点
# fitm_5knots_stra <- update(fitm_5knots_stra)


HRm_5knots_stra <- Predict(fitm_5knots_stra,rank_diff_per,fun=exp,ref.zero = T) #预测HR值

P1m_5knots_stra <- ggplot(HRm_5knots_stra)
P1m_5knots_stra

P2m_5knots_stra <- ggplot() + geom_line(data=HRm_5knots_stra, aes(rank_diff_per,yhat),linetype="solid",linewidth=2, colour=ckb) + 
  geom_ribbon(data=HRm_5knots_stra, aes(rank_diff_per,ymin=lower,ymax=upper), alpha=0.1,fill=ckb) 
P2m_5knots_stra

P3m_5knots_stra <- P2m_5knots_stra + theme_classic() + geom_hline(yintercept=1, linetype=2,size=1.5)+
  labs(title="A",x="BMI-PGM", y="HR (95%CI)")+ 
  scale_x_continuous(breaks=seq(-100, 100, 25)) + # X轴每隔几个单位显示一个刻度
  scale_y_log10(limits = c(0.22,2.5), breaks=c(0.5,1.0,1.5,2.0,2.5))+ 
  theme(text = element_text(family = "serif"))+
  theme(plot.title = element_text(size=27,face="bold"))+
  theme(axis.title.x= element_text(size = 25))+
  theme(axis.title.y= element_text(size = 25))+
  theme(axis.text.x = element_text(size = 22))+ 
  theme(axis.text.y = element_text(size = 22)) # 调整横纵坐标字体大小
P3m_5knots_stra


prcs_c <- P3m_5knots_stra + geom_vline(xintercept = 0, color="purple",linetype=2,size=1.5)
prcs_c


####ukb####
load("C:/Users/LLLLL/Desktop/UKB/data/complete_dat.RData")

####剔除基线患糖尿病的人####
complete_dat<-complete_dat[complete_dat$has_diabetes=="No",]
nrow(complete_dat) #94398


####限制性立方样条图_UKB####
library(survival)
library(Hmisc)
library(plyr)
library(ggplot2)
library(rms)

complete_dat$age_coxg<-as.factor(complete_dat$age_coxg)

dat_m <- subset(complete_dat, 
                select = c("age","time_out","DM_time","DM_diagnosis","rank_qg",
                           "sex","edu","income","smoke_3g","alcohol_3g",
                           "pa","healthy_diet2","sithours","sleep_hours1",
                           "fam_diabetes","bmi", "region_code",
                           "age_coxg","rank_diff_per","array"))
ddm <- datadist(dat_m)
options(datadist='ddm')

fitm_5knots_stra <- cph(Surv(DM_time,DM_diagnosis) ~ rcs(rank_diff_per,5)+sex+edu+income+smoke_3g+alcohol_3g+pa+healthy_diet2+sithours+sleep_hours1+fam_diabetes+bmi+strat(region_code)+ strat(age_coxg), data=dat_m)
# 非线性检验：P<0.05为存在非线性关系
anova(fitm_5knots_stra) # P-Nonlinear 0.0006


# ddm$limits$children[2] <- 3 # 设置参考点，也就是HR为1的点，常见的为中位数或者临床有意义的点
# fitm_5knots_stra <- update(fitm_5knots_stra)


HRm_5knots_stra <- Predict(fitm_5knots_stra,rank_diff_per,fun=exp,ref.zero = T) #预测HR值

P1m_5knots_stra <- ggplot(HRm_5knots_stra)
P1m_5knots_stra

P2m_5knots_stra <- ggplot() + geom_line(data=HRm_5knots_stra, aes(rank_diff_per,yhat),linetype="solid",linewidth=2,colour=ukb) + 
  geom_ribbon(data=HRm_5knots_stra, aes(rank_diff_per,ymin=lower,ymax=upper), alpha=0.1,fill=ukb) 
P2m_5knots_stra

P3m_5knots_stra <- P2m_5knots_stra + theme_classic() + geom_hline(yintercept=1, linetype=2,size=1.5)+
  labs(title="B",x="BMI-PGM", y="HR (95%CI)")+ 
  scale_x_continuous(breaks=seq(-100, 100, 25)) + # X轴每隔几个单位显示一个刻度
  scale_y_log10(limits = c(0.22,2.5), breaks=c(0.5,1.0,1.5,2.0,2.5))+ 
  theme(text = element_text(family = "serif"))+
  theme(plot.title = element_text(size=27,face="bold"))+
  theme(axis.title.x= element_text(size = 25))+
  theme(axis.title.y= element_text(size = 25))+
  theme(axis.text.x = element_text(size = 22)) + 
  theme(axis.text.y = element_text(size = 22)) # 调整横纵坐标字体大小
P3m_5knots_stra


prcs_u <- P3m_5knots_stra + geom_vline(xintercept = 0, color="purple",linetype=2,size=1.5)
prcs_u


####整合图片####
library(patchwork)
p<-(prcs_c|prcs_u)

cairo_pdf("C:/Users/LLLLL/Desktop/rcs.pdf",width = 12,height = 6)
p
dev.off()
