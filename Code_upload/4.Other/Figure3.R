library(forestplot)
library(readxl)
library(RColorBrewer)
color_palette<-brewer.pal(n = 8, name = "Set2")


forest_total<-read_xlsx("C:/Users/LLLLL/Desktop/forestcontinuous.xlsx",col_names = TRUE) 
forest_total[, 5:7] <- lapply(forest_total[, 5:7], as.numeric)

p1<-forestplot(labeltext = as.matrix(forest_total[,1:4]),
              line.margin = 0.15,
              mean  = forest_total$HR,
              lower = forest_total$LCI,
              upper = forest_total$UCI,
              graph.pos=4, #森林图列位置：第4列
              graphwidth = unit(0.25, "npc"), #森林图宽度
              colgap = unit(3.0,'mm'),  #列间距
              zero=1, #无效线的横坐标
              lwd.zero = 1,#无效线的粗细
              is.summary=c(T,T,T,
                           T,F,F,T,
                           T,T,
                           T,F,F,T,
                           T,F,F,T,
                           T,T,
                           T,F,F,T,
                           T,F,F,T,
                           T,F,F,T),
              align = c("l","c","c","c"), 
              col=fpColors(box="black",
                           line=c("grey","grey"),
                           summary="black"),
              boxsize=rep(0.30,31),
              clip=c(-Inf,Inf),
              xlog=TRUE,
              xticks=c(log(0.75),log(1.0),log(1.5),log(2.0)),
              xticks.digits=3,
              xlab="Hazard ratio",
              lwd.xaxis = 1.5, 
              lwd.ci=1.5, 
              lineheight = 'auto',
              txt_gp = fpTxtGp(label = gpar(fontfamily="serif",col="Black", cex=1.4, font=1,hjust=-2),
                               ticks = gpar(fontfamily="serif",cex = 1.2, font=1),
                               xlab = gpar(cex = 1.4),
                               legend = gpar(cex = 1.25),
                               summary = gpar(cex = 1.4)),
              new_page = TRUE
) 


forest_total<-read_xlsx("C:/Users/LLLLL/Desktop/forestcategory.xlsx",col_names = TRUE) 
forest_total[, 5:13] <- lapply(forest_total[, 5:13], as.numeric)
forest_total$HR_ref<-round(forest_total$HR_ref,2)


p2<-forestplot(labeltext = as.matrix(forest_total[,1:3]),
           line.margin = 0.15,
           mean  = cbind(forest_total$HR_low,forest_total$HR_ref,forest_total$HR_up ),
           lower = cbind(forest_total$LCI_low,forest_total$LCI_ref,forest_total$LCI_up),
           upper = cbind(forest_total$UCI_low,forest_total$UCI_ref,forest_total$UCI_up),
           graph.pos=1, #森林图列位置：第1列
           graphwidth = unit(0.38, "npc"), #森林图宽度
           colgap = unit(3.0,'mm'),  #列间距
           zero=1, #无效线的横坐标
           lwd.zero = 1,#无效线的粗细
           is.summary=c(T,T,T,
                        T,F,F,T,
                        T,T,
                        T,F,F,T,
                        T,F,F,T,
                        T,T,
                        T,F,F,T,
                        T,F,F,T,
                        T,F,F,T),
           align = c("c","c","c"), 
           col=fpColors(box=c(color_palette[3],"grey",color_palette[1]),
                        line=c("grey","white","grey"),
                        summary=c(color_palette[3],"white",color_palette[1])),
           boxsize=c(rep(0.30,29),rep(0.30,29),rep(0.30,29)),
           clip=c(0.5,2.0),
           xlog=TRUE,
           xticks=c(log(0.50),log(0.75),log(1.0),log(1.5),log(2.0)),
           xlab="Hazard ratio",
           lwd.xaxis = 1.5, 
           lwd.ci=1.5, 
           lineheight = 'auto',
           txt_gp = fpTxtGp(label = gpar(fontfamily="serif",col="Black", cex=1.4, font=1,hjust=-2),
                            ticks = gpar(fontfamily="serif",cex = 1.2, font=1),
                            xlab = gpar(cex = 1.4),
                            legend = gpar(cex = 1.25),
                            summary = gpar(cex = 1.4)),
           new_page = TRUE
           ) 


####整合图片####
library(gridExtra)
g1<-grid.grabExpr(print(p1))
g2<-grid.grabExpr(print(p2))

p3<-grid.arrange(g1, g2, ncol = 2)

#右下plots窗口，点击Expot→Save as PDF→修改尺寸为20*10，其余不改，点击Save


