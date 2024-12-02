rm(list=ls())
gc()


# ssh node29   
# source /public/software/apps/env_install_R.sh
# R


load("/public/home/liaolin/BMI-PRS/CKB/data_for_paper/testset.RData")
dat<-testset


####变量分组####

#age
dat$age<-dat$age_at_study_date_x100/100


#edu
for (i in 1:nrow(dat)){
  if(dat$highest_education[i]<=1){
    dat$edu[i]=1     # 小学及以下
  }
  else if(dat$highest_education[i]<=3){
    dat$edu[i]=2   # 初高中
  }
  else{
    dat$edu[i]=3   # 大专及以上
  }
}   


#income
for (i in 1:nrow(dat)){
  if(dat$household_income[i]<=2){
    dat$income[i]=1   # 小于10000
  }
  else if(dat$household_income[i]==3){
    dat$income[i]=2   # 10000-19999
  } 
  else{
    dat$income[i]=3   #大于20000
  }
}


#smoke_3g
dat$smoke_3g[dat$smoking_category==1]<-0 #never
dat$smoke_3g[dat$smoking_category==3]<-1 #ex
dat$smoke_3g[dat$smoking_category==4|dat$smoking_category==2]<-2 #current


#alcohol_3g
dat$alcohol_3g[dat$alcohol_category==1]<-0 #never
dat$alcohol_3g[dat$alcohol_category==2|dat$alcohol_category==5]<-1 #ex
dat$alcohol_3g[dat$alcohol_category==4|dat$alcohol_category==6|dat$alcohol_category==3]<-2 #current


#physical activity_四分位数

quantile(dat[dat$is_female==0,]$met) 
#0%    25%    50%    75%   100% 
#0.00   8.71  17.09  30.73 133.02 
quantile(dat[dat$is_female==1,]$met) 
#0%    25%    50%    75%   100% 
#0.00  10.00  15.89  26.84 108.77 


for (i in 1:nrow(dat)){
  if((dat$is_female[i]==0 & dat$met[i]<8.71)|
     (dat$is_female[i]==1 & dat$met[i]<10.00)){
    dat$pa[i]=1     
  }
  else if((dat$is_female[i]==0 & dat$met[i]<17.09)|
          (dat$is_female[i]==1 & dat$met[i]<15.89)){
    dat$pa[i]=2    
  }
  else if((dat$is_female[i]==0 & dat$met[i]<30.73)|
          (dat$is_female[i]==1 & dat$met[i]<26.84)){
    dat$pa[i]=3    
  } 
  else{
    dat$pa[i]=4   
  }
}


#sithours_三分位数
q<-quantile(dat$tv_reading_hours,probs=seq(0,1,1/3)) 
for(i in 1:length(dat$tv_reading_hours)){
  if(dat$tv_reading_hours[i]<q[2]){
    dat$sithours[i]<-0
  }
  else if((dat$tv_reading_hours[i]>=q[2])&(dat$tv_reading_hours[i]<q[3])){
    dat$sithours[i]<-1
  }
  else if(dat$tv_reading_hours[i]>=q[3]){
    dat$sithours[i]<-2
  }
}


#fish_2g
dat$fish_2g<-ifelse(dat$diet_freq_fish<=2,1,0)


#fruit_2g
dat$fruit_2g<-ifelse(dat$diet_freq_fresh_fruit<=2,1,0)

#veg_2g
dat$veg_2g<-ifelse(dat$diet_freq_fresh_veg==0,1,0)

#meat_2g
dat$meat_2g<-ifelse(dat$diet_freq_meat>=2,1,0)

#healthy_diet2
dat$diet_item<-apply(dat[,c("fish_2g","fruit_2g","veg_2g",
                            "meat_2g")], 1, sum, na.rm =T)
dat$healthy_diet2<-ifelse((dat$diet_item>=2), 1, 0)


#sleeping hours

for (i in 1:nrow(dat)){
  if(dat$sleep_hours[i]<7){
    dat$sleep_hours1[i]=1     
  }
  else if(dat$sleep_hours[i]<9){
    dat$sleep_hours1[i]=2    
  }
  else{
    dat$sleep_hours1[i]=3   
  }
}


####生成家族史变量（填补缺失值-保证可以识别）####
dat$siblings[is.na(dat$siblings)]<-999 
dat$mother_diabetes[is.na(dat$mother_diabetes)]<-999
dat$father_diabetes[is.na(dat$father_diabetes)]<-999   
dat$siblings_diabetes[is.na(dat$siblings_diabetes)]<-999 

# diabetes家族史：
for (i in 1:nrow(dat)){
  if(dat$mother_diabetes[i]==1|dat$father_diabetes[i]==1|
     (dat$siblings_diabetes[i]>0 & dat$siblings_diabetes[i]<999)){
    dat$fam_diabetes[i]=1     # 有家族史
  }
  else if(dat$mother_diabetes[i]==0 & dat$father_diabetes[i]==0 &
          (dat$siblings[i]==0|dat$siblings_diabetes[i]==0)){
    dat$fam_diabetes[i]=0    # 没有家族史
  }  
  else{
    dat$fam_diabetes[i]=2   #不清楚有没有家族史
  }
}


####疾病史生成####
dat$has_hypertension <-
  ifelse(
    (dat$hypertension_diag==1)|(dat$sbp_mean>=140)|
      (dat$dbp_mean>=90)|(dat$used_blood_pressure_drugs==1),   
    1, 0)

dat$has_CVD <-
  ifelse(
    (dat$chd_diag==1)| (dat$stroke_or_tia_diag==1),   
    1, 0)


dat$has_respiratory_disease <-
  ifelse(
    (dat$has_copd==1)|(dat$emph_bronc_diag==1)|(dat$asthma_diag==1)|(dat$tb_diag==1),   
    1, 0)

dat$has_cancer <-
  ifelse(
    (dat$cancer_diag==1) ,   
    1, 0)


####bmi分组####
#分2组
dat$bmi2g[dat$bmi_calc<24.0]<-0
dat$bmi2g[dat$bmi_calc>=24.0]<-1

#分4组
dat$bmi4g[dat$bmi_calc<18.5]<-0
dat$bmi4g[dat$bmi_calc>=18.5 & dat$bmi_calc<24.0]<-1
dat$bmi4g[dat$bmi_calc>=24.0 & dat$bmi_calc<28.0]<-2
dat$bmi4g[dat$bmi_calc>=28.0]<-3


####匹配prs####
prsbg<-read.table("C:/Users/DELL/Desktop/CKB/prscsx/prs.txt",header = TRUE)
prsbg$SCOREst<-scale(prsbg$SCORE)
dbqbg<-merge(dat,prsbg,by="FID")


####获得bmi残差和bmi-prs的残差####
library(lmtest)

#将分类变量因子化
dbqbg$region_code<-factor(dbqbg$region_code)
dbqbg$is_female<-factor(dbqbg$is_female)
dbqbg$array.x<-factor(dbqbg$array.x)

#分别对BMI和BMI-PRS线性回归并获得残差
mre<- lm(bmi_calc~age+I(age^2)+is_female+region_code+age:is_female+
           age:region_code+I(age^2):is_female+I(age^2):region_code+
           is_female:region_code+age:is_female:region_code+
           I(age^2):is_female:region_code, data = dbqbg)
residuals <- resid(mre)

prsre<- lm(SCOREst~region_code+national_pc01+national_pc02+national_pc03+
             national_pc04+national_pc05+national_pc06+national_pc07+
             national_pc08+national_pc09+national_pc10+array.x+region_code:array.x+
             region_code:national_pc01+region_code:national_pc02+
             region_code:national_pc03+region_code:national_pc04+
             region_code:national_pc05+region_code:national_pc06+
             region_code:national_pc07+region_code:national_pc08+
             region_code:national_pc09+region_code:national_pc10+
             array.x:national_pc01+array.x:national_pc02+
             array.x:national_pc03+array.x:national_pc04+
             array.x:national_pc05+array.x:national_pc06+
             array.x:national_pc07+array.x:national_pc08+
             array.x:national_pc09+array.x:national_pc10+
             region_code:array.x:national_pc01+region_code:array.x:national_pc02+
             region_code:array.x:national_pc03+region_code:array.x:national_pc04+
             region_code:array.x:national_pc05+region_code:array.x:national_pc06+
             region_code:array.x:national_pc07+region_code:array.x:national_pc08+
             region_code:array.x:national_pc09+region_code:array.x:national_pc10
           , data = dbqbg)
prsresiduals <- resid(prsre)


####获得个体BMI残差值和BMI-PRS的残差值的差值的排名百分比####
dbqbg$bmi_re= residuals
dbqbg$prs_re=prsresiduals
dbqbg$rank_re_per <- rank(dbqbg$bmi_re, ties.method = "average") / length(dbqbg$bmi_re) * 100
dbqbg$rank_prs_per <- rank(dbqbg$prs_re, ties.method = "average") / length(dbqbg$SCOREst) * 100
dbqbg$rank_diff_per<-dbqbg$rank_re_per-dbqbg$rank_prs_per


####将排名百分比根据四分位数分成3组####
#查看排位分布的差值的分布
q<-quantile(dbqbg$rank_diff_per)
#0%          25%          50%          75%         100% 
#-97.88686200 -23.88330106   0.02186543  23.99887549  96.23445992 

#根据四分位数对因变量分组
dbqbg$rank_qg[(dbqbg$rank_diff_per>=q[2])&(dbqbg$rank_diff_per<q[4])]<-0
dbqbg$rank_qg[dbqbg$rank_diff_per<q[2]]<-1
dbqbg$rank_qg[dbqbg$rank_diff_per>=q[4]]<-2


####保存####
save(dbqbg,file = "/public/home/liaolin/BMI-PRS/CKB/data_for_paper/dbqbg.RData")

load("C:/Users/LLLLL/Desktop/CKB/data_for_paper/dbqbg.RData")
a<-dbqbg

load("C:/Users/LLLLL/Desktop/CKB/datav3/dbqbg.RData")
b<-dbqbg

nrow(a)
nrow(b)

c<-b[,c("ccvid","diff","diff_3qg","diff_4qg","diff_5qg")]
d<-merge(a,c,by="ccvid")
dim(d)

dbqbg4<-d
save(dbqbg4,file = "C:/Users/LLLLL/Desktop/CKB/data_for_paper/dbqbg4.RData")


  
