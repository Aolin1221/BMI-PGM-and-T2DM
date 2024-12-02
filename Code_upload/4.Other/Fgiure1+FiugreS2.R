rm(list = ls())
gc()

library(ggplot2)

####配色方案####
library(RColorBrewer)
ckb<-brewer.pal(9,"Reds")[c(4,5,6)]
ukb<-brewer.pal(9,"Blues")[c(4,5,6)]


####加载数据####
load("/public/home/liaolin/BMI-PRS/CKB/data_for_paper/dbqbg.RData") #ckb

load("C:/Users/LLLLL/Desktop/CKB/data_for_paper/dbqbg.RData") #ckb

load("C:/Users/LLLLL/Desktop/UKB/data/datbq.RData") #ukb

####CKB数据因子化####
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
         bmi4g=factor(bmi4g,labels = c("Underweight","Normal weight","Overweight","Obesity")),
         bmi2g=factor(bmi2g,labels = c("Low and normal","Overweight and Obesity"))
  )


####BMI正态分布图_CKB####
pc<-ggplot(data=dbqbg, aes(x=bmi_calc)) +
  geom_histogram(aes(y=after_stat(density)), colour="white", fill=ckb[3]) +
  geom_density(alpha=0.5, colour="black",linewidth=0.7) +
  labs(title="A",x="BMI", y="Frequency") +
  scale_x_continuous(limits=c(10,60),breaks = seq(10,60.0,by = 10)) +
  scale_y_continuous(limits=c(0.00,0.12),breaks = seq(0.00, 0.12, by = 0.04)) +
  theme_gray()+
  theme(
    plot.title = element_text(size=18,family = "serif",face = "bold"),
    axis.text=element_text(size=15,family  = "serif"),
    axis.title=element_text(size=16,family = "serif"))

summary(dbqbg$bmi_calc)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#12.60   21.40   23.60   23.87   26.00   47.10


####PRS正态分布图_CKB####
pcc<-ggplot(data=dbqbg, aes(x=SCOREst)) +
  geom_histogram(aes(y=after_stat(density)), colour="white", fill=ckb[3]) +
  geom_density(alpha=0.5, colour="black",linewidth=0.7) +
  labs(title="C",x="BMI-PRS", y="Frequency") +
  scale_x_continuous(breaks = seq(-4.5, 4.5, by = 1.5)) +
  scale_y_continuous(breaks = seq(0.00,0.40,by=0.10))+
  theme_gray()+
  theme(
    plot.title = element_text(size=18,family = "serif",face = "bold"),
    axis.text=element_text(size=15,family  = "serif"),
    axis.title=element_text(size=16,family = "serif"))
summary(dbqbg$SCOREst)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-4.22115 -0.68900   -0.01407   -0.01027  0.66442  4.65757 



####mimatch正态分布图_CKB####
pccc<-ggplot(data=dbqbg, aes(x=rank_diff_per)) +
  geom_histogram(aes(y=after_stat(density)), colour="white", fill=ckb[3]) +
  geom_density(alpha=0.5, colour="black",linewidth=0.7) +
  labs(title="E",x="BMI-PGM", y="Frequency") +
  scale_x_continuous(breaks = seq(-100, 100, by = 20)) +
  scale_y_continuous(breaks = seq(0.000, 0.016, by = 0.004)) +
  theme_gray()+
  theme(
    plot.title = element_text(size=18,family = "serif",face = "bold"),
    axis.text=element_text(size=15,family  = "serif"),
    axis.title=element_text(size=16,family = "serif"))

summary(dbqbg$rank_diff_per)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#-97.88686 -23.88330   0.02187   0.00000  23.99888  96.23446 
pccc<-pccc+annotate('text',x=)
annotate('text',x=1,y=6.6,
         label=round(pi,digits=2),
         size=8,color='red') 

####BMI正态分布图_UKB####
pu<-ggplot(data=datbq, aes(x=bmi)) +
  geom_histogram(aes(y=after_stat(density)), colour="white", fill=ukb[3]) +
  geom_density(alpha=0.5, colour="black",linewidth=0.7) +
  labs(x="BMI") +
  scale_x_continuous(limits=c(10,60),breaks = seq(10,60.0,by = 10)) +
  scale_y_continuous(limits=c(0.00,0.12),breaks = seq(0.00, 0.12, by = 0.04)) +
  theme_gray()+
  theme(
    text=element_text(family="serif"),
    axis.text=element_text(size=15,family  = "serif"),
    axis.title.x=element_text(size=16,family = "serif"),
    axis.title.y = element_blank())
summary(datbq$bmi)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#13.80   24.10   26.70   27.44   30.00   68.10 



####PRS正态分布图_UKB####

puu<-ggplot(data=datbq, aes(x=SCORE_st)) +
  geom_histogram(aes(y=after_stat(density)), colour="white", fill=ukb[3]) +
  geom_density(alpha=0.5, colour="black",linewidth=0.8) +
  labs(x="BMI-PRS") +
  scale_x_continuous(breaks = seq(-4.5, 4.5, by = 1.5)) +
  theme_gray()+
  theme(
    text=element_text(family="serif"),
    axis.text=element_text(size=15,family  = "serif"),
    axis.title.x=element_text(size=16,family = "serif"),
    axis.title.y = element_blank())
summary(datbq$SCORE_st)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-4.6031 -0.7951 -0.1267 -0.1271  0.5378  4.3318 

####mimatch正态分布图_UKB####
puuu<-ggplot(data=datbq, aes(x=rank_diff_per)) +
  geom_histogram(aes(y=after_stat(density)), colour="white", fill=ukb[3]) +
  geom_density(alpha=0.5, colour="black",linewidth=0.7) +
  labs(x="BMI-PGM") +
  scale_x_continuous(breaks = seq(-100, 100, by = 20)) +
  scale_y_continuous(breaks = seq(0.000, 0.016, by = 0.004)) +
  theme_gray()+
  theme(
    text=element_text(family="serif"),
    axis.text=element_text(size=15,family  = "serif"),
    axis.title.x=element_text(size=16,family = "serif"),
    axis.title.y = element_blank())

summary(datbq$rank_diff_per)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-98.9182 -24.7091   0.2448   0.0000  24.6120  98.6236 M

####整合图片并保存####
library(patchwork)
p1<-(pc|pu)
p1
p2<-(pcc|puu)
p2
p3<-(pccc|puuu)
p3
p4<-p1/p2/p3
p4
cairo_pdf("C:/Users/DELL/Desktop/distribution.pdf",width = 12,height = 18)
p4
dev.off()





####BMI与BMI-PRS相关图_CKB####
#绘图
prc<-ggplot(bgsub, aes(x = bmi_calc, y = SCOREst)) +
  geom_point(colour=ckb[3],size=0.5)+
  geom_smooth(method = "lm", se = FALSE, colour="black",linewidth=0.7) +  
  labs(title="A",
       x=expression(paste("BMI (kg/",m^2,")")),
       y="BMI-PRS")+
  scale_y_continuous(limits = c(-5.0,5.0),breaks = seq(-5.0,5.0,by=2.5))+
  scale_x_continuous(limits = c(10,60),breaks = seq(10,60,by=10))+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    text=element_text(family="serif"),
    plot.title = element_text(size=18,face = "bold"),
    axis.text = element_text(size=15),
    axis.title = element_text(size=16))

#计算r2和p值

cor<-cor.test(bgsub$SCOREst, bgsub$bmi_calc)
round(cor$estimate, 2)#0.24
round(cor$p.value, 3)#0

#补充r²和p值
prc<-prc+annotate("text", x = 35, y =4.25, 
                  label="r = 0.24, p value < 0.001",
                  size=5,family="serif",fontface="bold")


####BMI与BMI-PGM相关图_CKB####
prcc<-ggplot(bgsub, aes(x = bmi_calc, y = rank_diff_per)) +
  geom_point(colour=ckb[3],size=0.5)+
  geom_smooth(method = "lm", se = FALSE, colour="black",linewidth=0.7) +
  labs(title="C",
       x=expression(paste("BMI (kg/",m^2,")")),
       y="BMI-PGM")+
  scale_y_continuous(limits = c(-100,100),breaks = seq(-100,100,by=25))+
  scale_x_continuous(limits = c(10,60),breaks = seq(10,60,by=10))+
  geom_hline(yintercept = c(-23.9,24.0),linetype=2,linewidth=0.7,color="gray")+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    text=element_text(family="serif"),
    plot.title = element_text(size=18,face = "bold"),
    axis.text = element_text(size=15),
    axis.title = element_text(size=16))

cor<-cor.test(bgsub$rank_diff_per, bgsub$bmi_calc)
round(cor$estimate, 2) #0.55
round(cor$p.value, 3) #0
prcc<-prcc+annotate("text", x = 35, y = 80, 
                    label="r = 0.55, p value < 0.001" ,
                    size=5,family="serif",fontface="bold")

####BMI箱线图_CKB####
library(dplyr)
library(tidyr)
library(ggplot2)

quantile_values <- bgsub %>%
  group_by(bmi4g) %>%
  summarise(
    Median = median(diff),
    Q1 = quantile(diff, 0.25),
    Q3 = quantile(diff, 0.75)
  )

pboxc<-ggplot(bgsub, aes(x = bmi4g, y = diff)) +
  geom_boxplot(width = 0.4,color=ckb[3]) +
  labs(
    x = "BMI groups",
    y = "BMIdiff",
    title = "A"
  ) +
  scale_y_continuous(limits = c(-10,20),breaks = seq(-10,20,by=5))+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    text=element_text(family="serif"),
    plot.title = element_text(size=18,face="bold"),
    axis.text = element_text(size=15),
    axis.title = element_text(size=16))+
  theme(legend.position = "none")


####堆积柱状图BMI_CKB####
#生成新的变量重新排列
q<-quantile(bgsub$diff)
bgsub$diff_4qgg<-NA
bgsub$diff_4qgg[(bgsub$diff>=q[2])&(bgsub$diff<q[4])]<-2
bgsub$diff_4qgg[bgsub$diff<q[2]]<-3
bgsub$diff_4qgg[bgsub$diff>=q[4]]<-1
bgsub$diff_4qgg<-factor(bgsub$diff_4qgg,labels = c("Discordantly high","Concordant","Discordantly low"))

tab_bmi4g<-table(bgsub$bmi4g,bgsub$diff_4qgg)
prop_bmi4g<-round(prop.table(tab_bmi4g,margin=1)*100,1)
prop_bmi4g<-as.data.frame(prop_bmi4g)
colnames(prop_bmi4g)<-c("bmi4g","diff_4qgg","freq")

pbarc<-ggplot(prop_bmi4g, aes(x = bmi4g, y = freq, fill = diff_4qgg)) +
  geom_bar(stat = "identity", position = "stack", width=0.4,color = "white") +
  scale_fill_manual(values = rev(ckb))+
  geom_text(aes(label = sprintf("%.1f", freq)), 
            position = position_stack(vjust = 0.5), size = 5,family="serif") +
  labs(title="C",x = "BMI groups", 
       y = "Percentage(%)",fill="BMIdiff") +
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    text=element_text(family="serif"),
    plot.title = element_text(size=18,face = "bold"),
    axis.text = element_text(size=15),
    axis.title = element_text(size=16),
    legend.position = "top",
    legend.title = element_text(size = 12,family = "serif"),
    legend.text=element_text(size=12,family = "serif"),
    legend.key.size = unit(1,"lines"))



####BMI与BMI-PRS相关图_UKB####

datbq$bmi4g<-factor(datbq$bmi4g,labels=c("Underweight","Normal weight","Overweight","Obesity"))

pru<-ggplot(datbq, aes( x = bmi,y = SCORE_st)) +
  geom_point(colour=ukb[3],size=0.5)+
  geom_smooth(method = "lm", se = FALSE, colour="black",linewidth=0.7) +  
  labs(title="B",x=expression(paste("BMI (kg/",m^2,")")))+
  scale_y_continuous(limits = c(-5.0,5.0),breaks = seq(-5.0,5.0,by=2.5))+
  scale_x_continuous(limits = c(10,60),breaks = seq(10,60,by=10))+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(size=18,face = "bold"),
    text=element_text(family="serif"),
    axis.text = element_text(size=15),
    axis.title.x = element_text(size=16),
    axis.title.y = element_blank())

#计算r2和p值
cor<-cor.test(datbq$SCORE_st, datbq$bmi)
round(cor$estimate, 2)#0.26
round(cor$p.value, 2)#0

pru<-pru+annotate("text", x = 35, y =2.5, 
                  label="r = 0.26, p value < 0.001" ,
                  size=5,family="serif",fontface="bold")


####BMI与BMI-PGM相关图_UKB####
pruu<-ggplot(datbq, aes(x = bmi, y = rank_diff_per)) +
  geom_point(colour=ukb[3],size=0.5)+
  geom_smooth(method = "lm", se = FALSE, colour="black",linewidth=0.7) +  
  labs(title="D",x=expression(paste("BMI (kg/",m^2,")")))+
  scale_y_continuous(limits = c(-100,100),breaks = seq(-100,100,by=25))+
  scale_x_continuous(limits = c(10,60),breaks = seq(10,60,by=10))+
  geom_hline(yintercept = c(-24.7,24.6),linetype=2,linewidth=0.7,color="gray")+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    text=element_text(family="serif"),
    plot.title = element_text(size=18,face = "bold"),
    axis.text = element_text(size=15),
    axis.title.x = element_text(size=16),
    axis.title.y = element_blank())

cor<-cor.test(datbq$rank_diff_per, datbq$bmi)
round(cor$estimate, 2)#0.57
round(cor$p.value, 2) #0
pruu<-pruu+annotate("text", x = 35, y =75, 
                  label="r = 0.57, p value < 0.001" ,
                  size=5,family="serif",fontface="bold")



####BMI箱线图_UKB####
library(dplyr)
library(tidyr)
library(ggplot2)

quantile_values <- datbq %>%
  group_by(bmi4g) %>%
  summarise(
    Median = median(rank_diff_per),
    Q1 = quantile(rank_diff_per, 0.25),
    Q3 = quantile(rank_diff_per, 0.75)
  )

pboxu<-ggplot(datbq, aes(x = bmi4g, y = rank_diff_per)) +
  geom_boxplot(width = 0.4,color=ukb[3]) +
  labs(title="F",x = "BMI groups") +
  scale_y_continuous(limits = c(-100,100),breaks = seq(-100,100,by=25))+
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    text=element_text(family="serif"),
    plot.title = element_text(size=18,face = "bold"),
    axis.text = element_text(size=15),
    axis.title.x = element_text(size=16),
    axis.title.y = element_blank())+
  theme(legend.position = "none")


####堆积柱状图BMI_UKB####
#生成新的变量重新排列
q<-quantile(datbq$rank_diff_per)
datbq$rankqgg<-NA
datbq$rankqgg[(datbq$rank_diff_per>=q[2])&(datbq$rank_diff_per<q[4])]<-2
datbq$rankqgg[datbq$rank_diff_per<q[2]]<-3
datbq$rankqgg[datbq$rank_diff_per>=q[4]]<-1
datbq$rankqgg<-factor(datbq$rankqgg,labels = c("Discordantly high","Concordant","Discordanlty low"))

tab_bmi4g<-table(datbq$bmi4g,datbq$rankqgg)
prop_bmi4g<-round(prop.table(tab_bmi4g,margin=1)*100,1)
prop_bmi4g<-as.data.frame(prop_bmi4g)
colnames(prop_bmi4g)<-c("bmi4g","rankqg","freq")

pbaru<-ggplot(prop_bmi4g, aes(x = bmi4g, y = freq, fill = rankqg)) +
  geom_bar(stat = "identity", position = "stack", width=0.4,color = "white") +
  scale_fill_manual(values = rev(ukb))+
  geom_text(aes(label = sprintf("%.1f", freq)), 
            position = position_stack(vjust = 0.5), size = 5,family="serif") +
  labs(title="H",x = "BMI groups", fill="BMI-PGM") +
  theme_bw()+
  theme(
    panel.grid = element_blank(),
    text=element_text(family="serif"),
    plot.title = element_text(size=18,face = "bold"),
    axis.text = element_text(size=15),
    axis.title.x = element_text(size=16),
    axis.title.y = element_blank()
    )+
  theme(legend.position = "top",
        legend.title = element_text(size = 12,family = "serif"),
        legend.text=element_text(size=12,family = "serif"),
        legend.key.size = unit(1, "lines") )


####整合图片#####
library(patchwork)
p5<-prc|pru
p6<-prcc|pruu
p7<-pboxc|pboxu
p8<-pbarc|pbaru

p9<-p5/p6/p7/p8

cairo_pdf("C:/Users/LLLLL/Desktop/F2.pdf",width = 13,height = 25)
p9
dev.off()



