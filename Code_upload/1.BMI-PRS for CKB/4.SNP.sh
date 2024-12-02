#----------------------------------PLINK_convert------------------------#
#=======================================================================#

for i in `seq 1 23`; do
/public/software/plink-1.90/plink \
    --bfile /public/data/GWAS/2018-11-22-Impute_QCed/gwasplink/chr$i \
    --keep /public/home/liaolin/BMIPRS/Sample/allking.fam \
    --make-bed \
    --out /public/home/liaolin/BMIPRS/prscs/chrnondup/chr$i \
    --threads 20
done

for i in 2 3 4 6 8 14 16 19; do
/public/software/plink-1.90/plink \
    --bfile /public/home/sundong/PhD/test/chr$i \
    --keep /public/home/liaolin/BMIPRS/Sample/allking.fam \
    --make-bed \
    --out /public/home/liaolin/BMIPRS/prscs/chrnondup/chr$i
	--threads 20
done

# 列出重复SNPID
for i in `seq 1 23`; do
/public/software/plink-1.90/plink \
    --bfile /public/home/liaolin/BMIPRS/prscs/chrnondup/chr$i \
    --list-duplicate-vars suppress-first \
    --out chr$i
done
# 剔除重复的SNPID
for i in `seq 1 23`; do
awk '{print $4}' /public/home/liaolin/BMIPRS/prscs/chrnondup/chr$i.dupvar > /public/home/liaolin/BMIPRS/prscs/chrnondup/chr$i.dup.snplist
done

for i in `seq 1 23`; do
/public/software/plink-1.90/plink \
    --bfile chr$i \
    --exclude  /public/home/liaolin/BMIPRS/prscs/chrnondup/chr$i.dup.snplist \
    --make-bed \
    --out chr${i}_nodup
done

## 删去不需要的文件
cd /public/home/liaolin/BMIPRS/prscs/chrnondup
rm chr*.log
rm chr*.dupvar

for chr in `seq 22`; do
    rm chr${chr}.bed
    rm chr${chr}.fam
    rm chr${chr}.bim
done

## 文件重命名
rename '_nodup' '' *