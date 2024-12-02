rm(list = ls())
gc()

library(survival)
library(survminer)
library(RColorBrewer)


####配色方案####
library(RColorBrewer)
ckb<-brewer.pal(9,"Reds")[c(4,5,6)]
ukb<-brewer.pal(9,"Blues")[c(4,5,6)]


####CKB数据处理####
load("C:/Users/LLLLL/Desktop/CKB/data_for_paper/dbqbg.RData")
dee <- read.csv("C:/Users/LLLLL/Desktop/CKB/DAR-2023-00295-V1/event_endpoints.csv", header=T)

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

#因子化
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
         rank_qg=factor(rank_qg,labels = c("Concordant","Discordantly low","Discordantly high")),
         bmi4g=factor(bmi4g,labels = c("Low","Normal","Overweight","Obesity")),
         bmi2g=factor(bmi2g,labels = c("Low and normal","Overweight and Obesity"))
  )

datfac_DM$rank_qg<-relevel(datfac_DM$rank_qg,ref = "Discordantly low")


####CKB绘图####
fit<-survfit((Surv(age,time_out,DM_diagnosis))~rank_qg,
             data = datfac_DM)

p1<-ggsurvplot(
  fit, 
  fun="cumhaz",
  conf.int=TRUE, 
  conf.int.style="ribbon", 
  palette = ckb, 
  pval=F, pval.method=F,
  risk.table=T,risk.table.height=0.35,
  title="C",
  xlab="Age (years)",
  legend.labs=c("Discordantly low","Concordant","Discordantly high"),
  legend.title="BMI-PGM",legend="top",
  surv.scale="percent",ylim=c(0,0.35),break.y.by=0.05,xlim=c(30,80),break.x.by=5,
  ggtheme = theme_classic()+
    theme(text = element_text(family = "serif"),
          plot.title = element_text(size=27,face="bold"),
          axis.title.x = element_blank(),
          axis.title.y=element_text(size=25),
          axis.text  = element_text(size=22),
          legend.title = element_text(size=22),
          legend.text = element_text(size=22)),
  tables.theme = theme_survminer()+
    theme(text = element_text(family = "serif"),
          title = element_text(size=25),
          axis.title.x= element_text(size = 25),
          axis.title.y=element_blank(),
          axis.text.x= element_text(size=22),
          axis.text.y= element_text(size=22),
          ),
  font.family="serif",fontsize=7
)


####UKB数据处理####
load("C:/Users/LLLLL/Desktop/UKB/data/complete_dat.RData")
complete_dat<-complete_dat[complete_dat$has_diabetes=="No",]

complete_dat$rank_qg<-relevel(complete_dat$rank_qg,ref = "<-25%")


####UKB绘图####
fit<-survfit((Surv(age,time_out,DM_diagnosis))~rank_qg,
             data = complete_dat)

p2<-ggsurvplot(
  fit, 
  fun="cumhaz",
  conf.int=TRUE, 
  conf.int.style="ribbon", 
  palette = ukb, 
  pval=F, pval.method=F,
  risk.table=T,risk.table.height=0.35,
  title="D",
  xlab="Age (years)",
  legend.labs=c("Discordantly low","Concordant","Discordantly high"),
  legend.title="BMI-PGM",legend="top",
  surv.scale="percent",ylim=c(0,0.35),break.y.by=0.05,xlim=c(30,80),break.x.by=5,
  ggtheme = theme_classic()+
    theme(text = element_text(family = "serif"),
          axis.title.x = element_blank(),
          axis.title.y=element_text(size=25),
          axis.text  = element_text(size=22),
          legend.title = element_text(size=22),
          legend.text = element_text(size=22),
          plot.title = element_text(size=27,face="bold")),
  tables.theme = theme_survminer()+
    theme(text = element_text(family = "serif"),
          title = element_text(size=25),
          axis.title.x= element_text(size = 25),
          axis.title.y=element_blank(),
          axis.text.x= element_text(size=20),
          axis.text.y= element_text(size=20)
          ),
  font.family="serif",fontsize=7
)


####整合图片####
library(patchwork)

p3<-p1$plot/p1$table+plot_layout(heights = c(4,1)) 
p4<-p2$plot/p2$table+plot_layout(heights = c(4,1)) 
p5<-p3|p4

cairo_pdf("C:/Users/LLLLL/Desktop/RCS_KM.pdf",width = 26, height = 20)
(prcs_c|prcs_u)/(p3|p4)
dev.off()











