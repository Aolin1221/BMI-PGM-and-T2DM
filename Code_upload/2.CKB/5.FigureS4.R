rm(list = ls())
gc()

# ssh node29   
# source /public/software/apps/env_install_R.sh
# R


library(forestplot)
library(readxl)
library(RColorBrewer)


####设置配色方案####
color_palette<-brewer.pal(n = 10, name = "Set2")


####绘制森林图（补充图4）####
forest_total<-read_xlsx("/public/home/liaolin/BMI-PRS/CKB/output_paper/2.Cox/forestckb.xlsx",col_names = TRUE) 

cairo_pdf("/public/home/liaolin/BMI-PRS/CKB/output_paper/3.森林图/forest_ckb.pdf",width=15.0,height=16.0) 
forestplot(labeltext = as.matrix(forest_total[,1:4]),
          legend = c("<-25%", "≥+25"), #图例
          line.margin = 0.15,
          mean  = cbind(forest_total$HR_low,forest_total$HR_up ), #提取HR值
          lower = cbind(forest_total$LCI_low,forest_total$LCI_up),#提取HR下限
          upper = cbind(forest_total$UCI_low,forest_total$UCI_up),#提取HR上限
          graph.pos=2, #森林图列位置：第2列
          graphwidth = unit(0.3, "npc"), #森林图宽度
          colgap = unit(4,'mm'),  #列间距
          lineheight = "auto",  #行高
          zero=1, #无效线的横坐标
          lwd.zero = 1,#无效线的粗细
          is.summary=c(T,T,
                           T,F,F,
                           T,F,F,
                           T,F,F,
                           T,F,F,F,
                           T,F,F,F,
                           T,F,F,F,   
                           T,F,F,F,   
                           T,F,F,F,F, 
                           T,F,F,F,
                           T,F,F,
                           T,F,F,F),#变量名加粗，变量分组名称不加粗
           align = c("l","c","c","c"), #森林图每部分内容的对齐方式，l为左对齐，c为居中
           col=fpColors(box=c(color_palette[3],color_palette[1]),line=c("gray","gray")),#设置点值和区间估计值的颜色
           boxsize=c(rep(0.4,43),rep(0.4,43)),#方块的大小
           clip=c(-Inf,Inf),##置信区间的范围，超过变成箭头
           xlog=TRUE,#横坐标是否要取对数
           xticks=c(log(0.25),log(0.5),log(1.0),log(2.0),log(4.0)),#横坐标等距
           xticks.digits=3,#横坐标刻度小数点位数
           xlab="Hazard ratio",#横坐标标题
           lwd.xaxis = 1.5, #x轴粗细
           lwd.ci=2.5, #设置区间估计线的粗细
           new_page = TRUE,  #是否在新的页面上绘制森林图
           txt_gp = fpTxtGp(label = gpar(fontfamily="serif",col="Black", cex=1.4, font=1,hjust=-2),
                            ticks = gpar(fontfamily="serif",cex = 1.2, font=1),
                            xlab = gpar(cex = 1.4),
                            legend = gpar(cex = 1.25),
                            summary = gpar(cex = 1.4))) #字体和大小的设置
dev.off()  


