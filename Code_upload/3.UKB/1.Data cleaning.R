rm(list = ls())
gc()

####整合UKB数据####

##死亡日期的数据中有重复的，保留重复的eid中死亡日期最小的
a<-read.csv("C:/Users/LLLLL/Desktop/UKB/死亡日期.csv",header=TRUE)
a$date_death<-as.Date(a$date_death)

library(dplyr)

a<-a%>%
  group_by(eid) %>%
  arrange(date_death) %>%
  slice(1)

nrow(a) #44499

##整合UKB数据
mypath<-"C:/Users/LLLLL/Desktop/UKB/rawdata/"

multmerge = function(mypath){
  filenames=list.files(path=mypath, pattern = ".csv",full.names=TRUE)
  datalist = lapply(filenames, function(x){read.csv(file=x)})
  Reduce(function(x,y) {merge(x,y,by="eid",all=T)}, datalist)
}

dbq<-multmerge(mypath)
dim(dbq) #502364 81

dbq<-merge(dbq,a,by="eid",all=T)
dim(dbq)


####选择验证集分析####
testset<-subset(dbq,select = c("eid","age","sex","assessment_center","urban_rural_satus",
                               "qualifications","tdi","household_income",
                               "smoking_status","alcohol_drinker_status","met",
                               "time_tv","time_computer","cooked_vegetable_intake",
                               "salad_raw_veg_intake","fresh_fruit_intake",
                               "oily_fish_intake","non_oily_fish_intake",
                               "processed_meat_intake","beef_intake",
                               "lamb_mutton_intake","pork_intake","sleep_duration",
                               "vascular_heart_problems_diag","sbp_auto_0",
                               "sbp_auto_1","sbp_m_0","sbp_m_1","dbp_auto_0",
                               "dbp_auto_1","dbp_m_0","dbp_m_1","mediacation_dm_bp",
                               "diabetes_diag","hba1c","illnesses_of_father",
                               "illnesses_of_mother","illnesses_of_siblings",
                               "bmi","SCORE_st","SCORE_eh","national_pc01",
                               "national_pc02","national_pc03","national_pc04",
                               "national_pc05","national_pc06","national_pc07",
                               "national_pc08","national_pc09","national_pc10",
                               "array","test_group","date_attending_assessment_centre",
                               "date_lost_to_follow_up","date_dm","date_death",
                               "blood_clot_DVT_bronchitis_emphysema_asthma_rhinitis_eczema_allergy_diag",
                               "copd_diag","tuber_diag","cancer_diag"))

testset$test_group[is.na(testset$test_group)]<-999
dat<-subset(dbq,test_group==1) #保留104592名研究对象
dim(dat)


####剔除bmi缺失值####
dat<-dat[!is.na(dat$bmi),] 
nrow(dat) #102514


####构建残差差值排名百分比####
#因子化
dat$sex<-factor(dat$sex,labels = c("Women","Men"))
dat$region_code<-factor(dat$assessment_center)
dat$array<-factor(dat$array)

#线性回归
library(lmtest)

mbmi<- lm(bmi~age+I(age^2)+sex+region_code+age:sex+
            age:region_code+I(age^2):sex+I(age^2):region_code+
            sex:region_code+age:sex:region_code+
            I(age^2):sex:region_code, data = dat)

dat$rebmi <- resid(mbmi)

#下面一步会花很长时间，我跑了两天左右，所以我建议数据清理这部分的代码重点检查逻辑就行，不用跑出来
mprs<- lm(SCORE_st~region_code+national_pc01+national_pc02+national_pc03+
            national_pc04+national_pc05+national_pc06+national_pc07+
            national_pc08+national_pc09+national_pc10+array+region_code:array+
            region_code:national_pc01+region_code:national_pc02+
            region_code:national_pc03+region_code:national_pc04+
            region_code:national_pc05+region_code:national_pc06+
            region_code:national_pc07+region_code:national_pc08+
            region_code:national_pc09+region_code:national_pc10+
            array:national_pc01+array:national_pc02+
            array:national_pc03+array:national_pc04+
            array:national_pc05+array:national_pc06+
            array:national_pc07+array:national_pc08+
            array:national_pc09+array:national_pc10+
            region_code:array:national_pc01+region_code:array:national_pc02+
            region_code:array:national_pc03+region_code:array:national_pc04+
            region_code:array:national_pc05+region_code:array:national_pc06+
            region_code:array:national_pc07+region_code:array:national_pc08+
            region_code:array:national_pc09+region_code:array:national_pc10
          , data = dat) 

dat$reprs <- resid(mprs)


####获得个体BMI残差值和BMI-PRS的残差值的差值的排名百分比####
dat$rank_re_per <- rank(dat$rebmi,ties.method = "average") / length(dat$rebmi) * 100
dat$rank_prs_per <- rank(dat$reprs, ties.method = "average") / length(dat$reprs) * 100
dat$rank_diff_per<-dat$rank_re_per-dat$rank_prs_per


####将排名百分比根据四分位数分成3组####
#查看排位分布的差值的分布
q<-quantile(dat$rank_diff_per)
#0%         25%         50%         75%        100% 
#-98.9181965 -24.7090641   0.2448446  24.6120042  98.6236026 

#根据四分位数对因变量分组
dat$rank_qg[(dat$rank_diff_per>=q[2])&(dat$rank_diff_per<q[4])]<-0
dat$rank_qg[dat$rank_diff_per<q[2]]<-1
dat$rank_qg[dat$rank_diff_per>=q[4]]<-2

dat$rank_qg<-factor(dat$rank_qg,labels=c("-25%~+25%","<-25%","≥+25%"))


####判断基线是否患糖尿病####
#处理日期变量
dat$date_lost_to_follow_up<-as.Date(dat$date_lost_to_follow_up)
dat$date_attending_assessment_centre<-as.Date(dat$date_attending_assessment_centre)
dat$date_dm<-as.Date(dat$date_dm)
dat$date_e10<-as.Date(dat$date_e10)
dat$date_e12<-as.Date(dat$date_e12)
dat$date_e13<-as.Date(dat$date_e13)
dat$date_e14<-as.Date(dat$date_e14)
dat$date_o24<-as.Date(dat$date_o24)
dat$diabetes_diag[is.na(dat$diabetes_diag)]<-9999
dat$hba1c[is.na(dat$hba1c)]<--9999
dat$mediacation_dm_bp[is.na(dat$mediacation_dm_bp)]<-9999

#逻辑：
#1.基线DM患者：有诊断史/用药史/糖化血红蛋白≥48.0/随访发生了糖尿病（ICD-10代码为E10-E14以及024）但发生日期早于入组日期
#2.如果这些变量都是缺失值，则无法判断
#3.其余为基线无DM着
#注意：
#1.用药史中-1和-3也视为缺失值，
#2.发病日期相关的变量，只要有日期，就说明发生了结局
#3.date_dm这个变量就是ICD-10代码为E11这个结局的发病日期，也就是本文关注的结局
for(i in 1:length(dat$eid)){
  if(
    (dat$diabetes_diag[i]==1) || 
    (dat$hba1c[i]>=48.0) || 
    (grepl("3",dat$mediacation_dm_bp[i]) & 
     (!grepl("-3",dat$mediacation_dm_bp[i]))) || 
    (!is.na(dat$date_dm[i]) & (!is.na(dat$date_attending_assessment_centre[i])) &
     dat$date_dm[i]<=dat$date_attending_assessment_centre[i]) ||
    (!is.na(dat$date_e10[i]) & (!is.na(dat$date_attending_assessment_centre[i])) &
     dat$date_e10[i]<=dat$date_attending_assessment_centre[i]) ||
    (!is.na(dat$date_e12[i]) & (!is.na(dat$date_attending_assessment_centre[i])) &
     dat$date_e12[i]<=dat$date_attending_assessment_centre[i]) ||
    (!is.na(dat$date_e13[i]) & (!is.na(dat$date_attending_assessment_centre[i])) &
     dat$date_e13[i]<=dat$date_attending_assessment_centre[i]) ||
    (!is.na(dat$date_e14[i]) & (!is.na(dat$date_attending_assessment_centre[i])) &
     dat$date_e14[i]<=dat$date_attending_assessment_centre[i]) ||
    (!is.na(dat$date_o24[i]) & (!is.na(dat$date_attending_assessment_centre[i])) &
     dat$date_o24[i]<=dat$date_attending_assessment_centre[i])
  ){
    dat$has_diabetes[i]=1
  }
    else if(
    (dat$diabetes_diag[i] %in% c(-1,-3)) &
    (dat$diabetes_diag[i]==9999) & 
    (dat$hba1c[i]==-9999) & 
    (dat$mediacation_dm_bp[i]==9999 || dat$medication_dm_bp[i]==-1 || dat$medication_dm_bp[i]==-3)
  ){
    dat$has_diabetes[i]=NA
  }

  else{
    dat$has_diabetes[i]=0
  }
}

table(dat$has_diabetes) #0:94398,1:8116（没有缺失值）
dat$has_diabetes<-factor(dat$has_diabetes,labels = c("No","Yes"))


####判断是否发生糖尿病结局####

#逻辑：
#1.date_dm（E11发病日期）只要有日期且晚于入组日期就说明在随访期间发生了T2DM
#2.如果不是空值且早于入组日期，以被归到上面的基线糖尿病患者里，Cox回归时会剔除这部分人
#3.如果是空值，随访期间就没有发生T2DM
dat$DM_diagnosis<-ifelse(
  !is.na(dat$date_dm) & dat$date_dm>dat$date_attending_assessment_centre,
  1,0) #date_dm<=date_attending_assessment_centre的这部分人在后面的分析时会剔除，所以先赋值为0也没关系
table(dat$DM_diagnosis) #新发8154例

####计算随访时间####

##生成随访截止日期的变量date_ddl，根据英格兰、威尔士和苏格兰三个地区赋值

#UKB地区的变量只给了城市的变量，需要自己将不同的市归类到英格兰、威尔士和苏格兰三类中
library(dplyr)

dat <- dat %>%
  mutate(region = case_when(
    assessment_center %in% c(11012, 11021, 11011, 11008, 11024, 11020, 11018, 11010, 11016, 11001, 11017, 11009, 11013, 11002, 11007, 11014, 10003, 11006, 11025, 11026, 11027, 11028) ~ 0,
    assessment_center %in% c(11003, 11022, 11023) ~ 1,
    assessment_center %in% c(11005, 11004) ~ 2,
    TRUE ~ NA_real_
  ))
dat$region<-factor(dat$region,labels = c("England","Wales","Scotland"))

#不同地区随访截止时间不同，根据地区赋值
dat$date_ddl<-NA
dat$date_ddl[dat$region=="England"]<-"2021/9/30"
dat$date_ddl[dat$region=="Scotland"]<-"2021/7/31"
dat$date_ddl[dat$region=="Wales"]<-"2021/2/28"
dat$date_ddl<-as.Date(dat$date_ddl)

##判断date_ddl、date_lost_follow_up、date_death哪一个最先发生，赋值给date_censor
dat$date_death<-as.Date(dat$date_death)
dat$date_lost_to_follow_up<-as.Date(dat$date_lost_to_follow_up)
dat$date_censor<-as.Date(pmin(dat$date_ddl,dat$date_lost_to_follow_up,dat$date_death,na.rm = TRUE))

##生成随访时间
dat$DM_followtime <- as.Date(NA)

##发生结局的人，终点时间为删失时间和发生结局的时间中的最小的时间
dat[dat$DM_diagnosis==1,]$DM_followtime<-apply(
  dat[(dat$DM_diagnosis==1),c("date_censor","date_dm")], 
  1, min, na.rm =T)

##未发生结局的人，终点时间为删失时间
dat[dat$DM_diagnosis==0,]$DM_followtime = 
  dat[dat$DM_diagnosis==0,]$date_censor

#计算随访年数
dat$DM_time_day <- difftime(dat$DM_followtime, 
                               dat$date_attending_assessment_centre, 
                               units="days")
dat$DM_time <- as.numeric(dat$DM_time_day)/365.25


###以年龄为时间尺度####
dat$time_out<-dat$age+dat$DM_time


####按年龄分层####

attach(dat)

dat$age_coxg[age >= 35 &  age < 40] <- 1
dat$age_coxg[age >= 40 &  age < 45] <- 2
dat$age_coxg[age >= 45 &  age < 50] <- 3
dat$age_coxg[age >= 50 &  age < 55] <- 4
dat$age_coxg[age >= 55 &  age < 60] <- 5
dat$age_coxg[age >= 60 &  age < 65] <- 6
dat$age_coxg[age >= 65 &  age < 70] <- 7
dat$age_coxg[age >= 70 &  age < 75] <- 8

detach(dat)


####变量分组####

#urbanrural
dat$urban_rural_status[dat$urban_rural_status==9]<-NA
dat$region<-ifelse(dat$urban_rural_status %in% c(2,3,4,6,7,8,13,14,15,16,17,18),1,0)
table(dat$region)
dat$region<-factor(dat$region,labels = c("Urban","Rural"))

#qualifications
dat$high_edu<-grepl("1", dat$qualifications, ignore.case = TRUE)
dat$middle_edu<-(grepl("2|3|4|5", dat$qualifications, ignore.case = TRUE)) & (!grepl("-3",dat$qualifications, ignore.case = TRUE))
dat$low_edu<-grepl("6|-7", dat$qualifications, ignore.case = TRUE)

for (i in 1:nrow(dat)) {
  if(dat$high_edu[i]==TRUE){
    dat$edu[i]=2
  }
  else if(
    dat$middle_edu[i]==TRUE)
  {
    dat$edu[i]=1
  }
  else if(dat$low_edu[i]==TRUE){
    dat$edu[i]=0
  }
  else{
    dat$edu[i]=NA
  }
}
table(dat$edu)
dat$edu<-factor(dat$edu,labels = c("Low","Middle","High"))


#income
for (i in 1:length(dat$eid)) {
  if (is.na(dat$household_income[i])){
    dat$income[i] = NA
  }
  else if (dat$household_income[i] >= 4) {
    dat$income[i] = 2
  } 
  else if (dat$household_income[i] == 3) {
    dat$income[i] = 1
  } 
  else if (dat$household_income[i] %in% c(1, 2)) {
    dat$income[i] = 0
  } 
  else {
    dat$income[i] = NA
  }
}

table(dat$income)

dat$income<-factor(dat$income,labels = c("Low","Middle","High"))


#smoke
dat$smoke_3g<-dat$smoking_status
dat$smoke_3g[dat$smoke_3g==-3]<-NA
dat$smoke_3g<-factor(dat$smoke_3g,labels = c("Never","Previous","Current"))


#alcohol
dat$alcohol_3g<-dat$alcohol_drinker_status
dat$alcohol_3g[dat$alcohol_3g==-3]<-NA
dat$alcohol_3g<-factor(dat$alcohol_3g,labels = c("Never","Previous","Current"))


#fruit_2g
dat$fruit_2g<-NA
for (i in 1:length(dat$eid)) {
  if (is.na(dat$fresh_fruit_intake[i])){
    dat$fruit_2g[i] = NA
  }
  else if (dat$fresh_fruit_intake[i] %in% c(4, 5)) {
    dat$fruit_2g[i] = 1
  } 
  else if (dat$fresh_fruit_intake[i] %in% c(0, 1, 2, 3)) {
    dat$fruit_2g[i] = 0
  } 
  else if (dat$fresh_fruit_intake[i] %in% c(-1, -3)) {
    dat$fruit_2g[i] = NA
  }
}


#vegetables
dat$raw_veg<-NA
for(i in 1:length(dat$eid)){
  if (is.na(dat$salad_raw_veg_intake[i])){
    dat$raw_veg[i] = NA
  }
  else if(dat$salad_raw_veg_intake[i]==1){
    dat$raw_veg[i]=0.5
  }
  else if(dat$salad_raw_veg_intake[i]==2){
    dat$raw_veg[i]=1
  }
  else if(dat$salad_raw_veg_intake[i]==4){
    dat$raw_veg[i]=5.5
  }
  else if(dat$salad_raw_veg_intake[i]==5){
    dat$raw_veg[i]=7
  }
  else if(dat$salad_raw_veg_intake[i] %in% c(-1,-3)){
    dat$raw_veg[i]=NA
  }
}

dat$cooked_veg<-NA
for(i in 1:length(dat$eid)){
  if (is.na(dat$cooked_vegetable_intake[i])){
    dat$cooked_veg[i] = NA
  }
  else if(dat$cooked_vegetable_intake[i]==1){
    dat$cooked_veg[i]=0.5
  }
  else if(dat$cooked_vegetable_intake[i]==2){
    dat$cooked_veg[i]=1
  }
  else if(dat$cooked_vegetable_intake[i]==4){
    dat$cooked_veg[i]=5.5
  }
  else if(dat$cooked_vegetable_intake[i]==5){
    dat$cooked_veg[i]=7
  }
  else if(dat$cooked_vegetable_intake[i] %in% c(-1,-3)){
    dat$cooked_veg[i]=NA
  }
}

dat$veg=dat$cooked_veg+dat$raw_veg
dat$veg_2g[complete.cases(dat$veg) & dat$veg >= 13.5] <- 1
dat$veg_2g[complete.cases(dat$veg) & dat$veg < 13.5] <- 0


#fish
for(i in 1:length(dat$eid)){
  if (is.na(dat$oily_fish_intake[i])){
    dat$oily_fish[i] = NA
  }
  else if(dat$oily_fish_intake[i]==1){
    dat$oily_fish[i]=0.5
  }
  else if(dat$oily_fish_intake[i]==2){
    dat$oily_fish[i]=1
  }
  else if(dat$oily_fish_intake[i]==4){
    dat$oily_fish[i]=5.5
  }
  else if(dat$oily_fish_intake[i]==5){
    dat$oily_fish[i]=7
  }
  else if(dat$oily_fish_intake[i] %in% c(-1,-3)){
    dat$oily_fish[i]=NA
  }
}

for(i in 1:length(dat$eid)){
  if(is.na(dat$non_oily_fish_intake[i])){
    dat$non_oily_fish[i]=NA
  }
  else if(dat$non_oily_fish_intake[i]==1){
    dat$non_oily_fish[i]=0.5
  }
  else if(dat$non_oily_fish_intake[i]==2){
    dat$non_oily_fish[i]=1
  }
  else if(dat$non_oily_fish_intake[i]==4){
    dat$non_oily_fish[i]=5.5
  }
  else if(dat$non_oily_fish_intake[i]==5){
    dat$non_oily_fish[i]=7
  }
  else if(dat$non_oily_fish_intake[i] %in% c(-1,-3)){
    dat$non_oily_fish[i]=NA
  }
}

dat$fish=dat$oily_fish+dat$non_oily_fish

dat$fish_2g[complete.cases(dat$fish) & dat$fish>=2]<-1
dat$fish_2g[complete.cases(dat$fish) & dat$fish<2]<-0


#meat_2g
for(i in 1:length(dat$eid)){
  if(is.na(dat$beef_intake[i])){
    dat$beef[i]=NA
  }
  else if(dat$beef_intake[i]==1){
    dat$beef[i]=0.5
  }
  else if(dat$beef_intake[i]==2){
    dat$beef[i]=1
  }
  else if(dat$beef_intake[i]==4){
    dat$beef[i]=5.5
  }
  else if(dat$beef_intake[i]==5){
    dat$beef[i]=7
  }
  else if(dat$beef_intake[i] %in% c(-1,-3)){
    dat$beef[i]=NA
  }
}

for(i in 1:length(dat$eid)){
  if(is.na(dat$lamb_mutton_intake[i])){
    dat$lamb_mutton[i]=NA
  }
  else if(dat$lamb_mutton_intake[i]==1){
    dat$lamb_mutton[i]=0.5
  }
  else if(dat$lamb_mutton_intake[i]==2){
    dat$lamb_mutton[i]=1
  }
  else if(dat$lamb_mutton_intake[i]==4){
    dat$lamb_mutton[i]=5.5
  }
  else if(dat$lamb_mutton_intake[i]==5){
    dat$lamb_mutton[i]=7
  }
  else if(dat$lamb_mutton_intake[i] %in% c(-1,-3)){
    dat$lamb_mutton[i]=NA
  }
}

for(i in 1:length(dat$eid)){
  if(is.na(dat$pork_intake[i])){
    dat$pork[i]=NA
  }
  else if(dat$pork_intake[i]==1){
    dat$pork[i]=0.5
  }
  else if(dat$pork_intake[i]==2){
    dat$pork[i]=1
  }
  else if(dat$pork_intake[i]==4){
    dat$pork[i]=5.5
  }
  else if(dat$pork_intake[i]==5){
    dat$pork[i]=7
  }
  else if(dat$pork_intake[i] %in% c(-1,-3)){
    dat$pork[i]=NA
  }
}

dat$red_meat=dat$beef+dat$lamb_mutton+dat$pork

for(i in 1:length(dat$eid)){
  if((!is.na(dat$processed_meat_intake[i]) & dat$processed_meat_intake[i] %in% c(0,1,2))|
     (!is.na(dat$red_meat[i]) & dat$red_meat[i]<=5)){
    dat$meat_2g[i]=1
  }
  else if((!is.na(dat$processed_meat_intake[i]) & dat$processed_meat_intake[i] %in% c(3,4,5))|
          (!is.na(dat$red_meat[i]) & dat$red_meat[i]>5)){
    dat$meat_2g[i]=0
  }
  else{
    dat$meat_2g[i]=NA
  }
}


#healthy_diet2
dat$diet_item<-apply(dat[,c("fruit_2g","veg_2g","fish_2g",
                            "meat_2g")], 1, sum, na.rm =T)
for(i in 1:length(dat$eid)){
  if(dat$diet_item[i]>=2){
    dat$healthy_diet2[i]=1
  }
  else if(
    dat$diet_item[i]<2 & 
    (is.na(dat$veg_2g[i])|is.na(dat$fruit_2g[i])|
     is.na(dat$meat_2g[i])|is.na(dat$fish_2g[i]))){
    dat$healthy_diet2[i]=NA
  }
  else{
    dat$healthy_diet2[i]=0
  }
}
table(dat$healthy_diet2)
dat$healthy_diet2<-factor(dat$healthy_diet2,labels=c("Unhealthy","Healthy"))

#因子化
dat$fruit_2g<-factor(dat$fruit_2g,labels = c("<4.5 pieces/wk",">=4.5 pieces/wk"))
dat$veg_2g<-factor(dat$veg_2g,labels = c("<4.5 servings/wk","≥4.5 servings/wk"))
dat$fish_2g<-factor(dat$fish_2g,labels = c("<2 times/wk","≥2 times/wk"))
dat$meat_2g<-factor(dat$meat_2g,labels = c("Non-excess","Excess"))
  

#sleep_hours1
dat$sleep_duration[dat$sleep_duration %in% c(-1, -3)]<-NA
dat$sleep_hours1<-NA
dat$sleep_hours1[complete.cases(dat$sleep_duration) & dat$sleep_duration<7]<-0
dat$sleep_hours1[complete.cases(dat$sleep_duration) & (dat$sleep_duration>=7 & dat$sleep_duration<9)]<-1
dat$sleep_hours1[complete.cases(dat$sleep_duration) & dat$sleep_duration>=9]<-2

dat$sleep_hours1<-factor(dat$sleep_hours1,labels = c("<7","7-9",">=9"))


#BP
dat$sbp_auto<-rowMeans(dat[,c("sbp_auto_0","sbp_auto_1")])
dat$sbp_m<-rowMeans(dat[,c("sbp_m_0","sbp_m_1")])
dat$dbp_auto<-rowMeans(dat[,c("dbp_auto_0","dbp_auto_1")])
dat$dbp_m<-rowMeans(dat[,c("dbp_m_0","dbp_m_1")])
dat$sbp_mean<-ifelse(is.na(dat$sbp_auto) | is.na(dat$sbp_m),
                     ifelse(is.na(dat$sbp_auto), as.character(dat$sbp_m), as.character(dat$sbp_auto)),
                     paste(dat$sbp_auto, dat$sbp_m, sep = ""))
dat$dbp_mean<-ifelse(is.na(dat$dbp_auto) | is.na(dat$dbp_m),
                    ifelse(is.na(dat$dbp_auto), as.character(dat$dbp_m), as.character(dat$dbp_auto)),
                    paste(dat$dbp_auto, dat$dbp_m, sep = ""))
dat$sbp_mean <- as.numeric(dat$sbp_mean)
dat$dbp_mean <- as.numeric(dat$dbp_mean)

for(i in 1:length(dat$eid)){
  if(
    (!is.na(dat$vascular_heart_problems_diag[i]) & grepl("4",dat$vascular_heart_problems_diag[i])) || 
    (!is.na(dat$sbp_mean[i]) & dat$sbp_mean[i]>=140) || 
    (!is.na(dat$dbp_mean[i]) & dat$dbp_mean[i]>=90) || 
    (!is.na(dat$mediacation_dm_bp[i]) & grepl("2",dat$mediacation_dm_bp[i]))
    ){
    dat$has_hypertension[i]=1
  }
  else if((is.na(dat$vascular_heart_problems_diag[i]) || 
           grepl("-3",dat$vascular_heart_problems_diag[i])) & 
          is.na(dat$sbp_mean[i]) & is.na(dat$dbp_mean[i]) & 
          (is.na(dat$mediacation_dm_bp[i])) || 
          grepl("-3",dat$mediacation_dm_bp[i])){
    dat$has_hypertension[i]=NA
  }
  else{
    dat$has_hypertension[i]=0
  }
}

dat$has_hypertension<-factor(dat$has_hypertension,labels = c("No","Yes"))


#CVD
for(i in 1:length(dat$eid)){
  if(
    !is.na(dat$vascular_heart_problems_diag[i]) & 
    (grepl("1|2|3",dat$vascular_heart_problems_diag[i]) &
     !grepl("-3",dat$vascular_heart_problems_diag[i]))
    ){
    dat$has_CVD[i]=1
  }
  else if(grepl("-3",dat$vascular_heart_problems_diag[i])|| 
          is.na(dat$vascular_heart_problems_diag[i])){
    dat$has_CVD[i]=NA
  }
  else{
    dat$has_CVD[i]=0
  }
}

dat$has_CVD<-factor(dat$has_CVD,labels = c("No","Yes"))


#生成肺气肿+支气管炎+哮喘变量
for(i in 1:length(dat$eid)){
  if(
    !is.na(dat$blood_clot_DVT_bronchitis_emphysema_asthma_rhinitis_eczema_allergy_diag[i]) & 
    (grepl("6|8",dat$blood_clot_DVT_bronchitis_emphysema_asthma_rhinitis_eczema_allergy_diag[i]))){
    dat$emph_bronchi_asthma[i]=1
  }
  else if(grepl("-3",dat$blood_clot_DVT_bronchitis_emphysema_asthma_rhinitis_eczema_allergy_diag[i])|| 
          is.na(dat$blood_clot_DVT_bronchitis_emphysema_asthma_rhinitis_eczema_allergy_diag[i])){
    dat$emph_bronchi_asthma[i]=NA
  }
  else{
    dat$emph_bronchi_asthma[i]=0
  }
}


#缺失值先赋值为999便于识别
dat$emph_bronchi_asthma[is.na(dat$emph_bronchi_asthma)]<-999
dat$copd_diag[is.na(dat$copd_diag)]<-999
dat$tuber_diag[is.na(dat$tuber_diag)]<-999

#生成呼吸系统疾病（包括肺气肿、支气管炎、哮喘、copd和肺结核）变量
for(i in 1:length(dat$eid)){
  if(dat$emph_bronchi_asthma[i]==1 | dat$copd_diag[i]==1 | dat$tuber_diag[i]==1){
    dat$has_respir[i]=1
  }
  else if(dat$emph_bronchi_asthma[i]==0 & dat$copd_diag[i]==0 & dat$tuber_diag[i]==0){
    dat$has_respir[i]=0
  }
  else{
    dat$has_respir[i]=NA
  }
}

#肿瘤中-1，-3定义为缺失值
for(i in 1:length(dat$eid)){
  if(!is.na(dat$cancer_diag[i]) & dat$cancer_diag[i] %in% c(-1,-3)){
    dat$cancer_diag[i]<-NA
  } 
}

#因子化
dat$has_respir<-factor(dat$has_respir,labels = c("No","Yes"))
dat$cancer_diag<-factor(dat$cancer_diag,labels = c("No","Yes"))


#fam_diabetes
#定义缺失值
dat$illnesses_of_father[is.na(dat$illnesses_of_father)|grepl("-11|-13",dat$illnesses_of_father)]<-"888"
dat$illnesses_of_mother[is.na(dat$illnesses_of_mother)|grepl("-11|-13",dat$illnesses_of_mother)]<-"888"
dat$illnesses_of_siblings[is.na(dat$illnesses_of_siblings)|grepl("-11|-13",dat$illnesses_of_siblings)]<-"888"

for(i in 1:length(dat$eid)){
  if(grepl("9",dat$illnesses_of_father[i]) | grepl("9",dat$illnesses_of_mother[i]) | grepl("9",dat$illnesses_of_siblings[i])){
    dat$fam_diabetes[i]=1
  } 
  else if(grepl("888",dat$illnesses_of_father[i]) | grepl("888",dat$illnesses_of_mother[i]) | grepl("888",dat$illnesses_of_siblings[i])){
    dat$fam_diabetes[i]=NA
  }
  else{
    dat$fam_diabetes[i]=0
  }
}

dat$fam_diabetes<-factor(dat$fam_diabetes,labels = c("No","Yes"))


#bmi2g
dat$bmi2g<-ifelse(dat$bmi>=25.0,1,0)
dat$bmi2g<-factor(dat$bmi2g,labels = c("Low and Normal","Overweight and obesity"))


#bmi4g
dat$bmi4g[dat$bmi<20.0]<-0
dat$bmi4g[dat$bmi>=20.0 & dat$bmi<25.0]<-1
dat$bmi4g[dat$bmi>=25.0 & dat$bmi<30.0]<-2
dat$bmi4g[dat$bmi>=30.0]<-3
dat$bmi4g<-factor(dat$bmi4g,labels = c("Low","Normal","Overweight","Obesity"))


save(dat,file = "C:/Users/DELL/Desktop/UKB/data/dat.RData")


####生成新的数据集####
datbq<-subset(dat,select = c("eid","age","sex","region","region_code","edu","tdi",
                             "income","smoke_3g","alcohol_3g","met","time_tv",
                             "time_computer","veg_2g","fruit_2g","fish_2g",
                             "meat_2g","healthy_diet2","sleep_hours1",
                             "has_hypertension","has_CVD","has_diabetes",
                             "has_respir","cancer_diag","fam_diabetes","bmi",
                             "bmi2g","bmi4g","SCORE_st","rank_diff_per","rank_qg",
                             "DM_diagnosis","DM_time","time_out","age_coxg"))

save(datbq,file = "C:/Users/DELL/Desktop/UKB/data/datbq.RData")

load("C:/Users/LLLLL/Desktop/UKB/data/datbq.RData")
a<-read.csv("C:/Users/LLLLL/Desktop/UKB/rawdata/血统.csv",h=T)
a$eur[is.na(a$eur)]<-999

b<-read.csv("C:/Users/LLLLL/Desktop/UKB/rawdata/种族.csv",h=T)
table(b$ethnic)
b$ethnic[is.na(b$ethnic)]<-999

c<-merge(a,b,by="eid")

dat<-merge(datbq,b,by="eid")
table(dat$ethnic)
dim(dat) #31694

datbq2<-dat
save(datbq2,file = "C:/Users/LLLLL/Desktop/UKB/data/datbq2.RData")


