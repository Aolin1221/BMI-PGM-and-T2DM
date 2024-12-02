#PRS-CS
cd /public/home/liaolin/BMIPRS/prscs
mamba create -n prscs
mamba activate prscs
mamba install prscs
git clone https://github.com/getian107/PRScs.git

#BBJ 自动搜寻phi
vi bbj.pbs
i
#PBS -N PRSCSbbj
#PBS -l nodes=1:ppn=10
#PBS -l walltime=60:00:00
#PBS -o /public/home/liaolin/BMIPRS/prscs/bbj
#PBS -e /public/home/liaolin/BMIPRS/prscs/bbj
for chr in `seq 1 22`; do
PRScs.py \
    --ref_dir=F:/科研/课题/身体测量指标与PRS与疾病/PRS/ldblk_1kg_eas \
    --bim_prefix=/public/home/liaolin/BMIPRS/prscs/ea/chr${chr} \
    --sst_file=/public/home/liaolin/BMIPRS/prscs/bbj/chr${chr}.txt \
    --n_gwas="163835" \
    --chrom=${chr} \
    --out_dir=/public/home/liaolin/BMIPRS/prscs/bbj/chr${chr} \
    --seed=1548
done



# 将PRS-CS输出的文件转换回CKB SNPID-bbj
R 
library(data.table)
Args<- commandArgs()

gwas<- Args[3]

CSpath= "/public/home/liaolin/BMIPRS/prscs/bbj/"
outpath= "/public/home/liaolin/BMIPRS/prscs/bbj/prschr/"
info<- fread("/public/home/yangsongchun/share/ckb_info_table_modified_V3.txt",header=TRUE)

m<-c(1,3,4,5,6,7,8,9,10,11,12,15,16,17,18,19,20,22)
for (chr in 1:22) {
	message(paste0("Chr",chr," is converting"))
	sub_info<- subset(info,CHR=chr,select=c("rsid","SNP"))
	 
		CS<- fread(paste0(CSpath,"chr",chr,"_pst_eff_a1_b0.5_phiauto_chr",chr,".txt"),header=FALSE)
		colnames(CS)<- c("CHR","rsid","BP","A1","A2","A1_Effect")

		CS$tag<- 1:nrow(CS)
		CS_info<- merge(CS,sub_info,by="rsid",all.x=TRUE)
		CS_info<- CS_info[order(CS_info$tag)]
			
		final<- CS_info[,c("CHR","SNP","BP","A1","A2","A1_Effect")]
		fwrite(final,paste0(outpath,"chr",chr,".txt"),col.names=FALSE,sep=" ",quote=FALSE)	
}

#分染色体计算多基因评分-bbj
for CHR in `seq 1 22` ; do
/public/software/plink-1.90/plink \
--bfile /public/home/liaolin/BMIPRS/prscs/bbj/chrnondup/chr${CHR}  \
--score /public/home/liaolin/BMIPRS/prscs/bbj/prschr/chr${CHR}.txt 2 4 6 sum \
--keep /public/home/liaolin/BMIPRS/prscs/ea/allking.fam \
--out /public/home/liaolin/BMIPRS/prscs/bbj/prschr/chr${CHR}.prs \
--threads 20
done 

#整合多基因评分-bbj
R 
prspath="/public/home/liaolin/BMIPRS/prscs/bbj/prschr/chr"
chr<-c(4,5,6,7,8,9,10,11,12,15,16,17,18,19,20,22)
chr1<-read.table("/public/home/liaolin/BMIPRS/prscs/bbj/prschr/chr1.prs.profile",header=TRUE)
chr2<-read.table("/public/home/liaolin/BMIPRS/prscs/bbj/prschr/chr2.prs.profile",header=TRUE)
chrm<-merge(chr1,chr2,by="FID")
chrm$SCORE=chrm$SCORESUM.x+chrm$SCORESUM.y
chrmm<-chrm[,c("FID","SCORE")]
for (i in 3：22){

	chri<-read.table(paste0(prspath,i,".prs.profile"),header=TRUE)
    chrm<-merge(chrmm,chri,by="FID")
	chrm$SCORE=chrm$SCORE+chrm$SCORESUM
	chrmm<-chrm[,c("FID","SCORE")]
}
head(chrmm)
write.table(chrmm,"/public/home/liaolin/BMIPRS/prscs/bbj/prs/prs.txt",col.names=TRUE,row.names=FALSE,quote=FALSE,sep=" ")

mamba create -n prscsx
mamba activate prscsx
mamba install prscsx
git clone https://github.com/getian107/PRScsx.git

#GIANT 自动搜寻phi
vi /public/home/liaolin/BMIPRS/prscs/giant/giant.pbs/giant.pbs
i
#PBS -N PRSCSgiant
#PBS -l nodes=1:ppn=10
#PBS -l walltime=60:00:00
#PBS -o /public/home/liaolin/BMIPRS/prscs/giant/phi
#PBS -e /public/home/liaolin/BMIPRS/prscs/giant/phi

for chr in `seq 1 22`; do
python /public/home/liaolin/PRScsx/PRScsx.py \
    --ref_dir=/public/home/liaolin/BMIPRS/prscs/giant/ldr/eur,/public/home/liaolin/BMIPRS/prscs/giant/ldr/amr,/public/home/liaolin/BMIPRS/prscs/giant/ldr/afr \
    --bim_prefix=/public/home/liaolin/BMIPRS/prscs/ea/chr${chr} \
    --sst_file=/public/home/liaolin/BMIPRS/prscs/giant/chr${chr}.txt \
    --n_gwas="681275","" \
    --chrom=${chr} \
    --out_dir=/public/home/liaolin/BMIPRS/prscs/giant/phi/chr${chr} \
    --seed=1548
done

/public/home/liaolin/BMIPRS/prscs/giant/giant.pbs

# 将PRS-CS输出的文件转换回CKB SNPID-giant
R 
library(data.table)
Args<- commandArgs()

gwas<- Args[3]

CSpath= "/public/home/liaolin/BMIPRS/prscs/giant/"
outpath= "/public/home/liaolin/BMIPRS/prscs/giant/prschr/"
info<- fread("/public/home/yangsongchun/share/ckb_info_table_modified_V3.txt",header=TRUE)

m<-c(1,3,4,5,6,7,8,9,10,11,12,15,16,17,18,19,20,22)
for (chr in 1:22) {
	message(paste0("Chr",chr," is converting"))
	sub_info<- subset(info,CHR=chr,select=c("rsid","SNP"))
	 
		CS<- fread(paste0(CSpath,"chr",chr,"_pst_eff_a1_b0.5_phiauto_chr",chr,".txt"),header=FALSE)
		colnames(CS)<- c("CHR","rsid","BP","A1","A2","A1_Effect")

		CS$tag<- 1:nrow(CS)
		CS_info<- merge(CS,sub_info,by="rsid",all.x=TRUE)
		CS_info<- CS_info[order(CS_info$tag)]
			
		final<- CS_info[,c("CHR","SNP","BP","A1","A2","A1_Effect")]
		fwrite(final,paste0(outpath,"chr",chr,".txt"),col.names=FALSE,sep=" ",quote=FALSE)	
}

#分染色体计算多基因评分-giant
for CHR in `seq 1 22` ; do
/public/software/plink-1.90/plink \
--bfile /public/home/liaolin/BMIPRS/prscs/ea/chrnondup/chr${CHR}  \
--score /public/home/liaolin/BMIPRS/prscs/giant/prschr/chr${CHR}.txt 2 4 6 sum \
--keep /public/home/liaolin/BMIPRS/prscs/ea/allking.fam \
--out /public/home/liaolin/BMIPRS/prscs/giant/prschr/chr${CHR}.prs \
--threads 20
done 

#整合多基因评分-giant
R 
prspath="/public/home/liaolin/BMIPRS/prscs/bbj/prschr/chr"
chr<-c(4,5,6,7,8,9,10,11,12,15,16,17,18,19,20,22)
chr1<-read.table("/public/home/liaolin/BMIPRS/prscs/giant/prschr/chr1.prs.profile",header=TRUE)
chr2<-read.table("/public/home/liaolin/BMIPRS/prscs/giant/prschr/chr2.prs.profile",header=TRUE)
chrm<-merge(chr1,chr2,by="FID")
chrm$SCORE=chrm$SCORESUM.x+chrm$SCORESUM.y
chrmm<-chrm[,c("FID","SCORE")]
for (i in 3：22){

	chri<-read.table(paste0(prspath,i,".prs.profile"),header=TRUE)
    chrm<-merge(chrmm,chri,by="FID")
	chrm$SCORE=chrm$SCORE+chrm$SCORESUM
	chrmm<-chrm[,c("FID","SCORE")]
}
head(chrmm)
write.table(chrmm,"/public/home/liaolin/BMIPRS/prscs/giant/prs/prs.txt",col.names=TRUE,row.names=FALSE,quote=FALSE,sep=" ")
