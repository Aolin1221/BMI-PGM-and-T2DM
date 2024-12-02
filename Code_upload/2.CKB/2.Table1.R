rm(list = ls())
gc()


# ssh node29   
# source /public/software/apps/env_install_R.sh
# R


load("/public/home/liaolin/BMI-PRS/CKB/data_for_paper/dbqbg.RData")


####因子化####
library(dplyr)   
bgsub <- dbqbg %>%
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


bgsub<- subset(bgsub,select=c("age","is_female","region","region_code","edu",
                              "income","smoke_3g","alcohol_3g","pa","sithours",
                              "fish_2g","fruit_2g","veg_2g","meat_2g",
                              "healthy_diet2","sleep_hours1","has_hypertension",
                              "has_CVD","has_respiratory_disease","has_cancer",
                              "has_diabetes","fam_diabetes","bmi_calc",
                              "rank_diff_per","rank_qg","bmi4g","bmi2g","SCOREst"))

bgsub$SCOREst<-as.numeric(bgsub$SCOREst)


####绘制Table1####
library(tableone)

columns<- c("age","is_female","region","region_code","edu","income","smoke_3g",
            "alcohol_3g","pa","sithours","fish_2g","fruit_2g","veg_2g","meat_2g",
            "healthy_diet2","sleep_hours1","has_hypertension","has_CVD",
            "has_diabetes","fam_diabetes","bmi_calc","SCOREst")
tab_total<-CreateTableOne(vars=columns,strata=c("rank_qg"),data=bgsub,addOverall=TRUE)
tab_total<-print(tab_total)

write.csv(tab_total,"/public/home/liaolin/BMI-PRS/CKB/output_paper/0.tableone/1.table_qg.csv")


####求每组PRS平均值和标准差####
round(mean(bgsub[bgsub$rank_qg=="<-25%",]$SCOREst),2) #0.73
round(sd(bgsub[bgsub$rank_qg=="<-25%",]$SCOREst),2) #0.70

round(mean(bgsub[bgsub$rank_qg=="-25%~+25%",]$SCOREst),2) #-0.01
round(sd(bgsub[bgsub$rank_qg=="-25%~+25%",]$SCOREst),2) #0.98

round(mean(bgsub[bgsub$rank_qg=="≥+25%",]$SCOREst),2) #-0.75
round(sd(bgsub[bgsub$rank_qg=="≥+25%",]$SCOREst),2) #0.69

