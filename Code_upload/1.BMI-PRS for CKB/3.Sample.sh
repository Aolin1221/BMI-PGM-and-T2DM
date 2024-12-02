#------------------------------处理基线数据----------------------------------------
#================================================================================#
R
dat<-read.csv("/public/home/liaolin/BMIPRS/Sample/dat_lal.csv",header=TRUE,row.names=1)

# 纳入有全基因组测序结果的个体
dat1<-dat[!is.na(dat$principal_components_source) ,]   
dat1<-dat1[dat1$principal_components_source==1,]  # 有GWAS data个体（保留100640名）

# 将censoring_date分为年月日
dat1$censoring_date_y<-as.integer(substr(dat1$censoring_date,1,4)) 
dat1$censoring_date_m<-as.integer(substr(dat1$censoring_date,6,7))
dat1$censoring_date_d<-as.integer(substr(dat1$censoring_date,9,10))


#处理bmi缺失值
dat1$bmi_calc[is.na(dat1$weight_kg_x10)]<-9999 

#处理年龄
dat1$age<-dat1$age_at_study_date_x100/100

#排除审查日期与纳入研究日期相同的个体，排除bmi缺失的个体
dat1$valid<- 
  ifelse(
    (dat1$censoring_date_y<=dat1$study_date_year 
     & dat1$censoring_date_m<=dat1$study_date_month
     & dat1$censoring_date_d<=dat1$study_date_day)|
      dat1$bmi_calc==9999      
    ,1,0) 
nrow(dat1[dat1$valid==1,])  #排除掉1个个体
dat2<-dat1[dat1$valid==0,]   #保留100639个个体

#array变量的处理
head(dat2$gwas_array_type,5)
#array的类型，如果是1，记为1，如果不是1，记为0
dat2$array<-ifelse(dat2$gwas_array_type=="Axiom CKB1",1,0)

#----------------------------------亲缘关系质控-------------------------------#
#============================================================================#

#提取dat中FID和IID两列
link<-dat[,c(2,3)]
write.table(link,"/public/home/liaolin/BMIPRS/Sample/link1.txt",quote=FALSE,row.names=FALSE)
q()

#剔除有亲缘关系的个体，注：在node运行不出来，需要提交PBS

vi king.pbs
i
#-----------------------------
#PBS -N CKBkinship
#PBS -l nodes=1:ppn=20
#PBS -l walltime=20:00:00
#PBS -o /public/home/liaolin/BMIPRS/Sample/pbsout
#PBS -e /public/home/liaolin/BMIPRS/Sample/pbsouterror
cd /public/home/liaolin/BMIPRS/Sample/
/public/software/plink-2.0/plink2 \
  --bfile /public/data/GWAS/2018-11-22-Impute_QCed/bpca/bpca \
  --king-cutoff 0.125 \
  --keep link1.txt \
  --make-king square0 bin \
  --out kinship_square 
#-------------------------------
:wq
#剩下86218个sample

#保留剩下的个体，和基线数据匹配
king<-read.table("/public/home/liaolin/BMIPRS/Sample/link1cutoffin.id",header=TRUE)
m<-merge(dat2,king,by="FID",all=FALSE)
nrow(m) #86217
dat2<-m[,-ncol(m)]
colnames(dat2)[4]<-"IID"
#剩下86217个个体

#------------------------------------------性别不匹配质控-----------------------------------#
#==========================================================================================#

#读取CKB 计算过sex匹配的文件
sex<-read.table("/public/data/GWAS/2018-11-22-Impute_QCed/ckb_pca_v1.1/idmap.txt",header=TRUE)
df3<-merge(dat2_lal,sex)
df3_sub <- subset(df3, (sex - is_female==1)) 
#剩下86205个个体


#-------------------------------------------生成fam文件------------------------------------#
#==========================================================================================#

# 按照PLINK可识别的格式（.fam）生成训练集和检验集的id文件

library(plyr,lib="~/Rlib")

all<-subset(df3_sub,select=c("FID","IID"))
all$V3<-0
all$V4<-0
all$V5<--9
write.table("/public/home/liaolin/BMIPRS/Sample/allking.fam",col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")

#----------------------------------------拆分训练集、验证集----------------------------------#
#===========================================================================================#

#看哪些人基于病例对照设计,做训练集
table(df3_sub$is_in_gwas_population_subset)
#0     1 
#22177 64028 

trainset<-datsexed[datsexed$is_in_gwas_population_subset==0,]
testset<-datsexed[datsexed$is_in_gwas_population_subset==1,]

#提取出prsice需要的train.id文件、表型文件和协变量文件

train.id<-subset(trainset,c=("FID","IID"))
wrtie.table(trainking.id,"/public/home/liaolin/BMIPRS/prsice/trainking.id",col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")

pheno<-subset(trainset,c=("FID","IID","bmi_cal"))
colnames(pheno)[3]<-bmi
write.table(pheno,"/public/home/liaolin/BMIPRS/prsice/pheno.txt",col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")

cov<-subset(trainset,c=("FID","IID","age","is_female","national_pc01","national_pc02","national_pc03",
                         "national_pc04","national_pc05","national_pc06","national_pc07","national_pc08",
                         "national_pc09","national_pc10","array"))
write.table(pheno,"/public/home/liaolin/BMIPRS/prsice/cov.txt",col.names=FALSE,row.names=FALSE,quote=FALSE,sep=" ")










