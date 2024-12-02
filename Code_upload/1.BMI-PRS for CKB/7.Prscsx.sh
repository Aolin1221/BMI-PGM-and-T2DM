#-------------------------------PRS-CSx自动搜寻phi---------------------#
#=====================================================================#

#自动搜寻phi
vi /public/home/liaolin/BMIPRS/prscsx/phichr/prscsx.pbs
i
python /public/home/liaolin/PRScsx/PRScsx.py --pop EUR,EAS --ref_dir=/public/home/liaolin/BMIPRS/prscsx/refdir --bim_prefix=/public/home/liaolin/BMIPRS/prscs/ea/chr${chr} --sst_file=/public/home/liaolin/BMIPRS/prscs/giant/chr${chr}.txt,/public/home/liaolin/BMIPRS/prscs/bbj/prephichr/chr${chr}.txt --n_gwas="681275","163835" --chrom=${chr} --out_dir=/public/home/liaolin/BMIPRS/prscsx/phichr --out_name chr${chr} --seed=1548
:wq
qub /public/home/liaolin/BMIPRS/prscsx/phichr/prscsx.pbs

for chr in `seq 1 22`;do
	qsub  /public/home/liaolin/BMIPRS/prscsx/phichr/prscsx.pbs \
	    -N CSx_${chr} \
        -v chr=$chr \
	    -l nodes=1:ppn=20 \
	    -l walltime=960:00:00 \
	    -o /public/home/liaolin/BMIPRS/prscsx/phichr \
	    -e /public/home/liaolin/BMIPRS/prscsx/phichr	   
done

#-------------------------------PRS-CSx输出的文件转换回CKB SNPID------------#
#==========================================================================#
vi /public/home/liaolin/BMIPRS/prscsx/phichrckb/convertckb.pbs
i
R --no-save < /public/home/liaolin/BMIPRS/prscsx/code/covertback.R
:wq

qsub /public/home/liaolin/BMIPRS/prscsx/phichrckb/convertckb.pbs

R
library(data.table)
Args<- commandArgs()

gwas<- Args[3]

CSxpath<- paste0("/public/home/liaolin/BMIPRS/prscsx/phichr/chr")
outpath<- paste0("/public/home/liaolin/BMIPRS/prscsx/phichrckb/")
info<- fread("/public/home/yangsongchun/share/ckb_info_table_modified_V3.txt",header=TRUE)

for (chr in 1:22) {
	message(paste0("Chr",chr," is converting"))
	sub_info<- subset(info,CHR=chr,select=c("rsid","SNP"))
	for (ances in c("EUR","EAS")) {
		CSx<- fread(paste0(CSxpath,chr,"_",ances,"_pst_eff_a1_b0.5_phiauto_chr",chr,".txt"),header=FALSE)
		colnames(CSx)<- c("CHR","rsid","BP","A1","A2","A1_Effect")

		CSx$tag<- 1:nrow(CSx)
		CSx_info<- merge(CSx,sub_info,by="rsid",all.x=TRUE)
		CSx_info<- CSx_info[order(CSx_info$tag)]
			
		final<- CSx_info[,c("CHR","SNP","BP","A1","A2","A1_Effect")]
		fwrite(final,paste0(CSxpath,"chr",chr,"_",ances,".txt"),col.names=FALSE,sep=" ",quote=FALSE)
	}
}

#-----------------------------------用PLINK计算PRS，以欧洲裔为例--------------------
#================================================================================#
vi /public/home/liaolin/BMIPRS/prscsx/prschr/prschr.pbs
i
anc=EAS
# score参数：2、4、6表示SNPid，EA，beta
/public/software/plink-1.90/plink \
	--bfile /public/home/liaolin/BMIPRS/prscs/chrnondup/chr$CHR \
	--score /public/home/liaolin/BMIPRS/prscsx/phichrckb/chr${CHR}_${anc}.txt 2 4 6 sum \
	--out /public/home/liaolin/BMIPRS/prscsx/prschr/chr${CHR}_${anc}.prs \
	--threads 20

for CHR in `seq 1 22`; do
    anc=EAS \
    qsub /public/home/liaolin/BMIPRS/prscsx/prschr/prschr.pbs \
	    -N prs_${CHR} \
        -v CHR=$CHR,anc=$anc \
	    -l nodes=1:ppn=20 \
	    -l walltime=960:00:00 \
	    -o /public/home/liaolin/BMIPRS/prscsx/prschr \
	    -e /public/home/liaolin/BMIPRS/prscsx/prschr	   
done

#--------------------------------------------------整合多基因评分-giant------------------------------------------#
#===============================================================================================================#
R 
prspath="/public/home/liaolin/BMIPRS/prscsx/prschr/chr"
chr1<-read.table("/public/home/liaolin/BMIPRS/prscsx/prschr/chr1_EAS.prs.profile",header=TRUE)
chr2<-read.table("/public/home/liaolin/BMIPRS/prscsx/prschr/chr2_EAS.prs.profile",header=TRUE)
chrm<-merge(chr1,chr2,by="FID")
chrm$SCORE=chrm$SCORESUM.x+chrm$SCORESUM.y
chrmm<-chrm[,c("FID","SCORE")]
for (i in 3:22){

	chri<-read.table(paste0(prspath,i,"_EAS.prs.profile"),header=TRUE)
    chrm<-merge(chrmm,chri,by="FID")
	chrm$SCORE=chrm$SCORE+chrm$SCORESUM
	chrmm<-chrm[,c("FID","SCORE")]
}
head(chrmm)
write.table(chrmm,"/public/home/liaolin/BMIPRS/prscsx/prs/prs.txt",col.names=TRUE,row.names=FALSE,quote=FALSE,sep=" ")
#write.table(chrmm,"/public/home/liaolin/BMIPRS/prscs/giant/prsukb/prs.txt",col.names=TRUE,row.names=FALSE,quote=FALSE,sep=" ")