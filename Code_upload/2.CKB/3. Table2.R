rm(list = ls())
gc()

# ssh node29   
# source /public/software/apps/env_install_R.sh
# R


load("/public/home/liaolin/BMI-PRS/CKB/data_for_paper/dbqbg.RData")

load("C:/Users/LLLLL/Desktop/CKB/data_for_paper/dbqbg.RData")

#性别之前因子化了，这里去因子化
dbqbg$is_female<-as.numeric(dbqbg$is_female)

####敏感性分析6.1，剔除BMI<1%和＞99%的个体####
upper_99<-quantile(dbqbg$bmi_calc,0.99,na.rm = TRUE)
lower_1<-quantile(dbqbg$bmi_calc,0.01,na.rm = TRUE)
dbqbg4<-dbqbg[dbqbg$bmi_calc>=lower_1&dbqbg$bmi_calc<=upper_99,]
nrow(dbqbg4) #62802


####敏感性分析6.2，剔除BMI超过±3个标准差的个体####
#计算BMI的均值和标准差
mean_bmi<-mean(dbqbg$bmi_calc, na.rm = TRUE)
sd_bmi<-sd(dbqbg$bmi_calc, na.rm = TRUE)
dbqbg5<-dbqbg[dbqbg$bmi_calc>=(mean_bmi-3*sd_bmi)&dbqbg$bmi_calc<=(mean_bmi+3*sd_bmi),]
nrow(dbqbg5) ##63666


####单变量线性趋势检验（Table2 趋势性检验结果）####

#变量清单
var<-c("+age+is_female+region_is_urban+edu+income+smoke_3g+alcohol_3g+pa+sithours+
veg_2g+fruit_2g+fish_2g+meat_2g+sleep_hours1+has_hypertension+has_CVD+
       has_respiratory_disease+has_diabetes")

#线性回归并提取P值
lm_model<-lm(as.formula(paste0("rank_diff_per~",var)),data=dbqbg5)
lm_summary<-summary(lm_model)
coef<-lm_summary$coefficients
coef[,4]<-round(coef[,4],3)

write.csv(coef,"/public/home/liaolin/BMI-PRS/CKB/output_paper/1.lm/1.lm_ptrend.csv")


####因子化####
library(dplyr)   
bgfac <- dbqbg %>%
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
         has_diabetes=factor(has_diabetes,labels = c("No","Yes")),
         fam_diabetes=factor(fam_diabetes,labels = c("No","Yes","Unknown")),
         rank_qg=factor(rank_qg,labels = c("-25%~+25%","<-25%","≥+25%")),
         bmi4g=factor(bmi4g,labels = c("Low","Normal","Overweight","Obesity")),
         bmi2g=factor(bmi2g,labels = c("Low and normal","Overweight and Obesity"))
  )

#is_female 改成以女性为参照
bgfac$is_female<-relevel(bgfac$is_female,ref = "Women")


####敏感性分析6.1，剔除BMI<1%和＞99%的个体####
upper_99<-quantile(bgfac$bmi_calc,0.99,na.rm = TRUE)
lower_1<-quantile(bgfac$bmi_calc,0.01,na.rm = TRUE)
bgfac4<-bgfac[bgfac$bmi_calc>=lower_1&bgfac$bmi_calc<=upper_99,]
nrow(bgfac4) #62802


####敏感性分析6.2，剔除BMI超过±3个标准差的个体####
#计算BMI的均值和标准差
mean_bmi<-mean(bgfac$bmi_calc, na.rm = TRUE)
sd_bmi<-sd(bgfac$bmi_calc, na.rm = TRUE)
bgfac5<-bgfac[bgfac$bmi_calc>=(mean_bmi-3*sd_bmi)&bgfac$bmi_calc<=(mean_bmi+3*sd_bmi),]
nrow(bgfac5) ##63666


####影响因素线性回归（Table2）####
library(lmtest)
library(epiDisplay)

#变量清单
var<-c("age+is_female+region+edu+income+smoke_3g+alcohol_3g+pa+sithours+
veg_2g+fruit_2g+fish_2g+meat_2g+sleep_hours1")

cov<-c("+has_hypertension+has_CVD+has_respiratory_disease+has_cancer+has_diabetes")

#mismatch线性回归
lm_model<-lm(as.formula(paste0("rank_diff_per~",var,cov)),data=bgfac5)
lm_summary<-summary(lm_model)
tab<-regress.display(lm_model)

write.csv(tab,"C:/Users/LLLLL/Desktop/CKB/output_paper/一修/lm_excluBMI2.csv")

write.csv(tab,"/public/home/liaolin/BMI-PRS/CKB/output_paper/1.lm/1.lm_qg.csv")


####多重共线性检验####
library(car)
vif(lm_model)
#                            GVIF Df GVIF^(1/(2*Df))
#age                     1.657069  1        1.287272
#is_female               3.038247  1        1.743057
#region                  1.779354  1        1.333924
#edu                     1.633970  2        1.130605
#income                  1.353628  2        1.078636
#smoke_3g                3.030783  2        1.319437
#alcohol_3g              1.391177  2        1.086040
#pa                      1.347402  3        1.050952
#sithours                1.108716  2        1.026136
#veg_2g                  1.041655  1        1.020615
#fruit_2g                1.216757  1        1.103067
#fish_2g                 1.528333  1        1.236257
#meat_2g                 1.262019  1        1.123396
#sleep_hours1            1.071979  2        1.017528
#has_hypertension        1.155433  1        1.074911
#has_CVD                 1.090402  1        1.044223
#has_respiratory_disease 1.042057  1        1.020812
#has_cancer              1.004151  1        1.002074
#has_diabetes            1.052538  1        1.025933










