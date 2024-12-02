rm(list = ls())
gc()

load("C:/Users/LLLLL/Desktop/UKB/data/complete_dat.RData")


####线性回归趋势性检验（Table2趋势性检验P值）####

#构造去因子化函数
defactorize <- function(x) {
  if (is.factor(x)) {
    return(as.numeric(x))
  } else {
    return(x)
  }
}

# 使用lapply对数据框进行去因子化
bgunfac <- as.data.frame(lapply(complete_dat4, defactorize))

bgunfac <- as.data.frame(lapply(complete_dat5, defactorize))

var<-c("+age+sex+region+edu+income+smoke_3g+alcohol_3g+pa+sithours+
veg_2g+fruit_2g+fish_2g+meat_2g+sleep_hours1+has_hypertension+has_CVD+
  has_respir+has_diabetes")

lm_model<-lm(as.formula(paste0("rank_diff_per~",var)),data=bgunfac)
lm_summary<-summary(lm_model)
coef<-lm_summary$coefficients
coef[,4]<-round(coef[,4],3)

write.csv(coef,"C:/Users/DELL/Desktop/UKB/output/1.lm/lm_ptrend.csv")


####敏感性分析6.1，剔除BMI<1%和＞99%的个体####
upper_99<-quantile(complete_dat$bmi,0.99,na.rm = TRUE)
lower_1<-quantile(complete_dat$bmi,0.01,na.rm = TRUE)
complete_dat4<-complete_dat[complete_dat$bmi>=lower_1&complete_dat$bmi<=upper_99,]
nrow(complete_dat4) #100564


####敏感性分析6.2，剔除BMI超过±3个标准差的个体####
#计算BMI的均值和标准差
mean_bmi<-mean(complete_dat$bmi, na.rm = TRUE)
sd_bmi<-sd(complete_dat$bmi, na.rm = TRUE)
complete_dat5<-complete_dat[complete_dat$bmi>=(mean_bmi-3*sd_bmi)&complete_dat$bmi<=(mean_bmi+3*sd_bmi),]
nrow(complete_dat5) ##101285


####影响因素线性回归（Table2）####

#线性回归
library(lmtest)
library(epiDisplay)

var<-c("age+sex+region+edu+income+smoke_3g+alcohol_3g+pa+sithours+veg_2g+
       fruit_2g+fish_2g+meat_2g+sleep_hours1")
cov<-c("+has_hypertension+has_CVD+has_respir+cancer_diag+has_diabetes")


#mismatch线性回归求置信区间
lm_model<-lm(as.formula(paste0("rank_diff_per~",var,cov)),data=complete_dat5)
lm_summary<-summary(lm_model)
tab<-regress.display(lm_model)
write.csv(tab,"C:/Users/LLLLL/Desktop/UKB/output/一修/lm_excluBMI2.csv")


####多重共线性检验####
library(car)
car::vif(lm_model)

#                     GVIF Df GVIF^(1/(2*Df))
#age              1.271650  1        1.127675
#sex              1.072740  1        1.035732
#region           1.018755  1        1.009334
#edu              1.254730  2        1.058370
#income           1.278926  2        1.063436
#smoke_3g         1.107600  2        1.025878
#alcohol_3g       1.115236  2        1.027642
#pa               1.051191  3        1.008355
#sithours         1.059140  2        1.014468
#veg_2g           1.016244  1        1.008089
#fruit_2g         1.026070  1        1.012951
#fish_2g          1.039685  1        1.019650
#meat_2g          1.015876  1        1.007907
#sleep_hours1     1.031468  2        1.007776
#has_hypertension 1.158100  1        1.076151
#has_CVD          1.062501  1        1.030777
#has_respir       1.078321  1        1.038422
#cancer_diag      1.020353  1        1.010125
#has_diabetes     1.066455  1        1.032693

