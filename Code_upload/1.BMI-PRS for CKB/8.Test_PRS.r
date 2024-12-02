#-------------------------------检验PRS----------------------------------#
#=======================================================================#
trainprsice<-read.table("/public/home/liaolin/BMIPRS/prsice/trainprsice.txt",header = TRUE)
prs<-read.table("/public/home/liaolin/BMIPRS/prscsx/prs/prs.txt",header=TRUE)
prs$SCOREst<-scale(prs$SCORE)
datprs<-merge(trainprsice,prs,by="FID")
model<-lm(bmi_calc~SCOREst+national_pc01+national_pc02+national_pc03+national_pc04
           +national_pc05+national_pc06+national_pc07+national_pc08+national_pc09
           +national_pc10+age+I(age)^2+is_female+array.x,data = dat)
modelnull<-lm(bmi_calc~national_pc01+national_pc02+national_pc03+national_pc04
               +national_pc05+national_pc06+national_pc07+national_pc08+national_pc09
               +national_pc10+age+I(age)^2+is_female+array.x,data = dat)
m<-summary(model)
null<-summary(modelnull)
m$r.squared-null$r.squared #0.05747916
m$coefficients #0.830412057

anova(model,modelnull) #4241.5

