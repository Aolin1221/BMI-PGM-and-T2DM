library(metafor)

####Model4-low-meta——re####
data<-read.csv("C:/Users/LLLLL/Desktop/meta_qg_ecluBMI1_low_4.csv")
meta_result<-rma.uni(log(hrl), sei = sel, data = data, method = "REML")
print(meta_result)

#Model 4 low
#Random-Effects Model (k = 2; tau^2 estimator: REML)
#
#tau^2 (estimated amount of total heterogeneity): 0 (SE = 0.0068)
#tau (square root of estimated tau^2 value):      0
#I^2 (total heterogeneity / total variability):   0.00%
#H^2 (total variability / sampling variability):  1.00
#
#Test for Heterogeneity:
#  Q(df = 1) = 0.3002, p-val = 0.5838
#
#Model Results:
#  
#  estimate      se     zval    pval    ci.lb    ci.ub      
#-0.2639  0.0418  -6.3071  <.0001  -0.3458  -0.1819  ***  
round(exp(-0.0836),2) #0.77 (0.71-0.83)


####Model4--low--meta--fe####
data<-read.csv("C:/Users/DELL/Desktop/小论文/code/Meta/meta_qg_low_4.csv")
meta_result<-rma.uni(log(hrl), sei = sel, data = data, method = "FE")
print(meta_result)
#Model 4 low:
#I^2 (total heterogeneity / total variability):   0.00%
#H^2 (total variability / sampling variability):  0.30
#
#Test for Heterogeneity:
#  Q(df = 1) = 0.3002, p-val = 0.5838
#
#Model Results:
#  
#  estimate      se     zval    pval    ci.lb    ci.ub      
#-0.2639  0.0418  -6.3071  <.0001  -0.3458  -0.1819  ***
#0.77 (0.71-0.83)


####Model4--up--meta--re####
data<-read.csv("C:/Users/DELL/Desktop/小论文/code/Meta/meta_qg_up_4.csv")
meta_result<-rma.uni(log(hru), sei = seu, data = data, method = "REML")
print(meta_result)
#Model 4 up: 
#tau^2 (estimated amount of total heterogeneity): 0.0081 (SE = 0.0141)
#tau (square root of estimated tau^2 value):      0.0902
#I^2 (total heterogeneity / total variability):   81.37%
#H^2 (total variability / sampling variability):  5.37
#
#Test for Heterogeneity:
#  Q(df = 1) = 5.3663, p-val = 0.0205
#
#Model Results:
#  
#  estimate      se    zval    pval   ci.lb   ci.ub     
#0.1961  0.0704  2.7839  0.0054  0.0580  0.3341  **  
round(exp(0.3341),2)
#1.22 (1.06-1.40)


####Model4--up--meta--fe####
data<-read.csv("C:/Users/DELL/Desktop/小论文/code/Meta/meta_qg_up_4.csv")
meta_result<-rma.uni(log(hru), sei = seu, data = data, method = "FE")
print(meta_result)
#Model 4 up:
#I^2 (total heterogeneity / total variability):   81.37%
#H^2 (total variability / sampling variability):  5.37
#
#Test for Heterogeneity:
#  Q(df = 1) = 5.3663, p-val = 0.0205
#
#Model Results:
#  
#  estimate      se    zval    pval   ci.lb   ci.ub      
#0.1700  0.0272  6.2514  <.0001  0.1167  0.2232  *** 
round(exp(0.2232),2) #1.19 (1.12-1.25)


####敏感性分析1####
library(metafor)
data<-read.csv("C:/Users/DELL/Desktop/小论文/code/Meta/sen1_low.csv")
meta_result<-rma.uni(log(hrl), sei = sel, data = data, method = "REML")
print(meta_result)
#tau^2 (estimated amount of total heterogeneity): 0 (SE = 0.0071)
#tau (square root of estimated tau^2 value):      0
#I^2 (total heterogeneity / total variability):   0.00%
#H^2 (total variability / sampling variability):  1.00
#
#Test for Heterogeneity:
#  Q(df = 1) = 0.1561, p-val = 0.6927
#
#Model Results:
#  
#  estimate      se     zval    pval    ci.lb    ci.ub      
#-0.2862  0.0431  -6.6420  <.0001  -0.3707  -0.2018  *** 
round(exp(-0.2018),2) #0.75 (0.69-0.82)


library(metafor)
data<-read.csv("C:/Users/DELL/Desktop/小论文/code/Meta/sen1_up.csv")
meta_result<-rma.uni(log(hru), sei = seu, data = data, method = "REML")
print(meta_result)
#tau^2 (estimated amount of total heterogeneity): 0.0044 (SE = 0.0091)
#tau (square root of estimated tau^2 value):      0.0667
#I^2 (total heterogeneity / total variability):   69.29%
#H^2 (total variability / sampling variability):  3.26
#
#Test for Heterogeneity:
#  Q(df = 1) = 3.2561, p-val = 0.0712
#
#Model Results:
#  
#  estimate      se    zval    pval   ci.lb   ci.ub      
#0.1879  0.0561  3.3507  0.0008  0.0780  0.2978  ***  
round(exp(0.2978),2) #1.21 (1.08-1.35)


####敏感性分析2####
library(metafor)
data<-read.csv("C:/Users/DELL/Desktop/小论文/code/Meta/sen2_low.csv")
meta_result<-rma.uni(log(hrl), sei = sel, data = data, method = "REML")
print(meta_result)
#tau^2 (estimated amount of total heterogeneity): 0 (SE = 0.0069)
#tau (square root of estimated tau^2 value):      0
#I^2 (total heterogeneity / total variability):   0.00%
#H^2 (total variability / sampling variability):  1.00
#
#Test for Heterogeneity:
#  Q(df = 1) = 0.2669, p-val = 0.6054
#
#Model Results:
#  
#  estimate      se     zval    pval    ci.lb    ci.ub      
#-0.2621  0.0431  -6.0846  <.0001  -0.3465  -0.1777  ***  
round(exp(-0.1777),2) #0.77 (0.71-0.84)


library(metafor)
data<-read.csv("C:/Users/DELL/Desktop/小论文/code/Meta/sen2_up.csv")
meta_result<-rma.uni(log(hru), sei = seu, data = data, method = "REML")
print(meta_result)
#tau^2 (estimated amount of total heterogeneity): 0.0079 (SE = 0.0139)
#tau (square root of estimated tau^2 value):      0.0889
#I^2 (total heterogeneity / total variability):   80.58%
#H^2 (total variability / sampling variability):  5.15
#
#Test for Heterogeneity:
#  Q(df = 1) = 5.1496, p-val = 0.0233
#
#Model Results:
#  
#  estimate      se    zval    pval   ci.lb   ci.ub     
#0.2004  0.0698  2.8732  0.0041  0.0637  0.3372  ** 
round(exp(0.3372),2) #1.22 (1.07-1.40)


####敏感性分析3####
library(metafor)
data<-read.csv("C:/Users/DELL/Desktop/小论文/code/Meta/sen3_low.csv")
meta_result<-rma.uni(log(hrl), sei = sel, data = data, method = "REML")
print(meta_result)
#tau^2 (estimated amount of total heterogeneity): 0 (SE = 0.0094)
#tau (square root of estimated tau^2 value):      0
#I^2 (total heterogeneity / total variability):   0.00%
#H^2 (total variability / sampling variability):  1.00
#
#Test for Heterogeneity:
#  Q(df = 1) = 0.0002, p-val = 0.9881
#
#Model Results:
#  
#  estimate      se     zval    pval    ci.lb    ci.ub      
#-0.3223  0.0558  -5.7735  <.0001  -0.4317  -0.2129  ***  
round(exp(-0.2129),2) #0.72 (0.65-0.81)


library(metafor)
data<-read.csv("C:/Users/DELL/Desktop/小论文/code/Meta/sen3_up.csv")
meta_result<-rma.uni(log(hru), sei = seu, data = data, method = "REML")
print(meta_result)
#tau^2 (estimated amount of total heterogeneity): 0.0072 (SE = 0.0138)
#tau (square root of estimated tau^2 value):      0.0846
#I^2 (total heterogeneity / total variability):   73.22%
#H^2 (total variability / sampling variability):  3.73
#
#Test for Heterogeneity:
#  Q(df = 1) = 3.7345, p-val = 0.0533
#
#Model Results:
#  
#  estimate      se    zval    pval   ci.lb   ci.ub     
#0.2199  0.0699  3.1473  0.0016  0.0829  0.3568  **   
round(exp(0.3568),2) #1.25 (1.09-1.43)


####敏感性分析4_三分位数####
library(metafor)
data<-read.csv("C:/Users/DELL/Desktop/小论文/code/Meta/sen4_low.csv")
meta_result<-rma.uni(log(hrl), sei = sel, data = data, method = "REML")
print(meta_result)
#tau^2 (estimated amount of total heterogeneity): 0 (SE = 0.0057)
#tau (square root of estimated tau^2 value):      0
#I^2 (total heterogeneity / total variability):   0.00%
#H^2 (total variability / sampling variability):  1.00
#
#Test for Heterogeneity:
#  Q(df = 1) = 0.8146, p-val = 0.3668
#
#Model Results:
#  
#  estimate      se     zval    pval    ci.lb    ci.ub      
#-0.2259  0.0387  -5.8330  <.0001  -0.3019  -0.1500  ***
round(exp(-0.1500),2) #0.80 (0.74-0.86)


library(metafor)
data<-read.csv("C:/Users/DELL/Desktop/小论文/code/Meta/sen4——_up.csv")
meta_result<-rma.uni(log(hru), sei = seu, data = data, method = "REML")
print(meta_result)

#tau^2 (estimated amount of total heterogeneity): 0.0037 (SE = 0.0082)
#tau (square root of estimated tau^2 value):      0.0612
#I^2 (total heterogeneity / total variability):   64.82%
#H^2 (total variability / sampling variability):  2.84
#
#Test for Heterogeneity:
#  Q(df = 1) = 2.8422, p-val = 0.0918
#
#Model Results:
#  
#  estimate      se    zval    pval   ci.lb   ci.ub     
#0.1628  0.0530  3.0715  0.0021  0.0589  0.2666  **
round(exp(0.2666),2) #1.18 (1.06-1.31)


####敏感性分析5_五分位数####
library(metafor)
data<-read.csv("C:/Users/DELL/Desktop/小论文/code/Meta/sen5_low.csv")
meta_result<-rma.uni(log(hrl), sei = sel, data = data, method = "REML")
print(meta_result)

#tau^2 (estimated amount of total heterogeneity): 0 (SE = 0.0088)
#tau (square root of estimated tau^2 value):      0
#I^2 (total heterogeneity / total variability):   0.00%
#H^2 (total variability / sampling variability):  1.00
#
#Test for Heterogeneity:
#  Q(df = 1) = 0.9409, p-val = 0.3320
#
#Model Results:
#  
#  estimate      se     zval    pval    ci.lb    ci.ub      
#-0.3188  0.0476  -6.7033  <.0001  -0.4120  -0.2256  *** 
round(exp(-0.2256),2) #0.73 (0.66-0.80)


library(metafor)
data<-read.csv("C:/Users/DELL/Desktop/小论文/code/Meta/sen5_up.csv")
meta_result<-rma.uni(log(hru), sei = seu, data = data, method = "REML")
print(meta_result)
#tau^2 (estimated amount of total heterogeneity): 0.0097 (SE = 0.0165)
#tau (square root of estimated tau^2 value):      0.0986
#I^2 (total heterogeneity / total variability):   83.57%
#H^2 (total variability / sampling variability):  6.09
#
#Test for Heterogeneity:
#  Q(df = 1) = 6.0852, p-val = 0.0136
#
#Model Results:
#  
#  estimate      se    zval    pval   ci.lb   ci.ub    
#0.1885  0.0761  2.4774  0.0132  0.0394  0.3377  *
round(exp(0.3377),2) #1.21 (1.04-1.40)



####敏感性分析1_连续变量####
library(metafor)
data<-read.csv("C:/Users/LLLLL/Desktop/meta_continuous_sen1.csv")
meta_result<-rma.uni(log(hr), sei = se, data = data, method = "REML")
print(meta_result)

#tau^2 (estimated amount of total heterogeneity): 0.0034 (SE = 0.0057)
#tau (square root of estimated tau^2 value):      0.0586
#I^2 (total heterogeneity / total variability):   85.51%
#H^2 (total variability / sampling variability):  6.90
#
#Test for Heterogeneity:
#  Q(df = 1) = 6.9016, p-val = 0.0086
#
#Model Results:
#  
#  estimate      se    zval    pval   ci.lb   ci.ub      
#0.1695  0.0447  3.7901  0.0002  0.0819  0.2572  *** 
round(exp(0.2572),2) #1.18 (1.09-1.29)

####敏感性分析2_连续变量####
library(metafor)
data<-read.csv("C:/Users/LLLLL/Desktop/meta_continuous_sen2.csv")
meta_result<-rma.uni(log(hr), sei = se, data = data, method = "REML")
print(meta_result)

#tau^2 (estimated amount of total heterogeneity): 0.0043 (SE = 0.0069)
#tau (square root of estimated tau^2 value):      0.0655
#I^2 (total heterogeneity / total variability):   88.40%
#H^2 (total variability / sampling variability):  8.62
#
#Test for Heterogeneity:
#  Q(df = 1) = 8.6186, p-val = 0.0033
#
#Model Results:
#  
#  estimate      se    zval    pval   ci.lb   ci.ub      
#0.1709  0.0492  3.4737  0.0005  0.0745  0.2674  *** 
round(exp(0.2674 ),2) #1.19 (1.08-1.31)


####敏感性分析3_连续变量####
library(metafor)
data<-read.csv("C:/Users/LLLLL/Desktop/meta_continuous_sen3.csv")
meta_result<-rma.uni(log(hr), sei = se, data = data, method = "REML")
print(meta_result)

#tau^2 (estimated amount of total heterogeneity): 0.0021 (SE = 0.0041)
#tau (square root of estimated tau^2 value):      0.0461
#I^2 (total heterogeneity / total variability):   73.29%
#H^2 (total variability / sampling variability):  3.74
#
#Test for Heterogeneity:
#  Q(df = 1) = 3.7435, p-val = 0.0530
#
#Model Results:
#  
#  estimate      se    zval    pval   ci.lb   ci.ub      
#0.1953  0.0380  5.1338  <.0001  0.1207  0.2699  *** 

round(exp(0.2699 ),2) #1.22 (1.13-1.31)



####不分层连续变量meta####
data<-read.csv("C:/Users/LLLLL/Desktop/meta_continuous.csv")
meta_result<-rma.uni(log(hr), sei = se, data = data, method = "REML")
print(meta_result)
#tau^2 (estimated amount of total heterogeneity): 0.0046 (SE = 0.0073)
#tau (square root of estimated tau^2 value):      0.0681
#I^2 (total heterogeneity / total variability):   89.39%
#H^2 (total variability / sampling variability):  9.42
#
#Test for Heterogeneity:
#  Q(df = 1) = 9.4236, p-val = 0.0021
#
#Model Results:
#  
#  estimate      se    zval    pval   ci.lb   ci.ub      
#0.1687  0.0509  3.3155  0.0009  0.0690  0.2684  ***
round(exp(0.2684),2)  #1.18 (1.07-1.31)
 

####PRS分层连续变量meta_low####
data<-read.csv("C:/Users/LLLLL/Desktop/meta_continuous_prslow.csv")
meta_result<-rma.uni(log(hr), se = se, data = data, method = "REML")
print(meta_result)
#tau^2 (estimated amount of total heterogeneity): 0 (SE = 0.0108)
#tau (square root of estimated tau^2 value):      0
#I^2 (total heterogeneity / total variability):   0.00%
#H^2 (total variability / sampling variability):  1.00
#
#Test for Heterogeneity:
#  Q(df = 1) = 0.9881, p-val = 0.3202
#
#Model Results:
#  
#  estimate      se    zval    pval   ci.lb   ci.ub    
#0.1188  0.0478  2.4853  0.0129  0.0251  0.2124  *  
round(exp(0.2124),2) #1.13 (1.03-1.24) 0.0129

####PRS分层连续变量meta_middle####
data<-read.csv("C:/Users/LLLLL/Desktop/meta_continuous_prsmiddle.csv")
meta_result<-rma.uni(log(hr), se = se, data = data, method = "REML")
print(meta_result)

#tau^2 (estimated amount of total heterogeneity): 0 (SE = 0.0091)
#tau (square root of estimated tau^2 value):      0
#I^2 (total heterogeneity / total variability):   0.00%
#H^2 (total variability / sampling variability):  1.00
#
#Test for Heterogeneity:
#  Q(df = 1) = 0.0014, p-val = 0.9700
#
#Model Results:
#  
#  estimate      se    zval    pval   ci.lb   ci.ub      
#0.2378  0.0393  6.0445  <.0001  0.1607  0.3150  *** 
round(exp(0.3150),2) #1.27 (1.17-1.37)




####PRS分层连续变量meta_high####
data<-read.csv("C:/Users/LLLLL/Desktop/meta_continuous_prshigh.csv")
meta_result<-rma.uni(log(hr), se = se, data = data, method = "REML")
print(meta_result)

#tau^2 (estimated amount of total heterogeneity): 0 (SE = 0.0089)
#tau (square root of estimated tau^2 value):      0
#I^2 (total heterogeneity / total variability):   0.00%
#H^2 (total variability / sampling variability):  1.00
#
#Test for Heterogeneity:
#  Q(df = 1) = 0.0335, p-val = 0.8548
#
#Model Results:
#  
#  estimate      se    zval    pval   ci.lb   ci.ub      
#0.3462  0.0359  9.6463  <.0001  0.2759  0.4166  *** 
round(exp(0.4166),2) #1.41 (1.32-1.52)


####PRS分层的连续变量的HR值得线性趋势检验####

data<-data.frame(
  Score_Level=c(1,2,3),  # 多基因评分层级
  HR_Value=c(1.13,1.27,1.41)  # 对应的meta分析后的HR值
)

# 拟合线性模型
model<-lm(HR_Value~Score_Level,data = data)

# 查看模型摘要
summary(model)

# 绘制散点图并添加线性回归线
ggplot(data, aes(x = Score_Level, y = HR_Value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(x = "多基因评分层级", y = "Meta分析后的HR值") +
  ggtitle("多基因评分层级与HR值的线性关系")


####bmi分层_连续变量_meta_normal####
data<-read.csv("C:/Users/LLLLL/Desktop/小论文/code/Meta/meta_continuous_bmi_nm.csv")
meta_result<-rma.uni(log(hr), se = se, data = data, method = "REML")
print(meta_result)

#tau^2 (estimated amount of total heterogeneity): 0.0313 (SE = 0.0485)
#tau (square root of estimated tau^2 value):      0.1770
#I^2 (total heterogeneity / total variability):   91.45%
#H^2 (total variability / sampling variability):  11.69
#
#Test for Heterogeneity:
#  Q(df = 1) = 11.6947, p-val = 0.0006
#
#Model Results:
#  
#  estimate      se    zval    pval    ci.lb   ci.ub    
#0.2306  0.1309  1.7620  0.0781  -0.0259  0.4872  . 

round(exp(0.4872),2) #1.26 (0.97-1.63)




####bmi分层_连续变量_meta_overweight####
data<-read.csv("C:/Users/LLLLL/Desktop/小论文/code/Meta/meta_continuous_bmi_ow.csv")
meta_result<-rma.uni(log(hr), se = se, data = data, method = "REML")
print(meta_result)

#tau^2 (estimated amount of total heterogeneity): 0.0037 (SE = 0.0071)
#tau (square root of estimated tau^2 value):      0.0609
#I^2 (total heterogeneity / total variability):   73.60%
#H^2 (total variability / sampling variability):  3.79
#
#Test for Heterogeneity:
#  Q(df = 1) = 3.7881, p-val = 0.0516
#
#Model Results:
#  
#estimate      se    zval    pval   ci.lb   ci.ub      
#0.1833  0.0499  3.6733  0.0002  0.0855  0.2811  *** 
round(exp(0.2811 ),2) #1.20 (1.09-1.32)


####bmi分层_正常体重_分类变量-low-meta——re####
data<-read.csv("C:/Users/LLLLL/Desktop/小论文/code/Meta/meta_qg_low_by_bmi_nm.csv")
meta_result<-rma.uni(log(hrl), sei = sel, data = data, method = "REML")
print(meta_result)

#Random-Effects Model (k = 2; tau^2 estimator: REML)
#
#tau^2 (estimated amount of total heterogeneity): 0.0393 (SE = 0.0691)
#tau (square root of estimated tau^2 value):      0.1983
#I^2 (total heterogeneity / total variability):   80.48%
#H^2 (total variability / sampling variability):  5.12
#
#Test for Heterogeneity:
#  Q(df = 1) = 5.1226, p-val = 0.0236
#
#Model Results:
#  
#estimate      se     zval    pval    ci.lb   ci.ub    
#-0.2480  0.1562  -1.5874  0.1124  -0.5543  0.0582 

round(exp(0.0582 ),2) #0.78 (0.57-1.06)


####bmi分层_正常体重_分类变量-up-meta——re####
data<-read.csv("C:/Users/LLLLL/Desktop/小论文/code/Meta/meta_qg_up_by_bmi_nm.csv")
meta_result<-rma.uni(log(hru), sei = seu, data = data, method = "REML")
print(meta_result)

#Random-Effects Model (k = 2; tau^2 estimator: REML)
#
#tau^2 (estimated amount of total heterogeneity): 0 (SE = 0.1036)
#tau (square root of estimated tau^2 value):      0
#I^2 (total heterogeneity / total variability):   0.00%
#H^2 (total variability / sampling variability):  1.00
#
#Test for Heterogeneity:
#  Q(df = 1) = 0.7248, p-val = 0.3946
#
#Model Results:
#  
#  estimate      se    zval    pval   ci.lb   ci.ub     
#0.3066  0.1159  2.6459  0.0081  0.0795  0.5337  ** 
  
round(exp(0.5337 ),2) #1.36 (1.08-1.71)



####bmi分层_超重_分类变量-low-meta——re####
data<-read.csv("C:/Users/LLLLL/Desktop/小论文/code/Meta/meta_qg_low_by_bmi_ow.csv")
meta_result<-rma.uni(log(hrl), sei = sel, data = data, method = "REML")
print(meta_result)

#Random-Effects Model (k = 2; tau^2 estimator: REML)
#
#tau^2 (estimated amount of total heterogeneity): 0.0144 (SE = 0.0481)
#tau (square root of estimated tau^2 value):      0.1202
#I^2 (total heterogeneity / total variability):   42.49%
#H^2 (total variability / sampling variability):  1.74
#
#Test for Heterogeneity:
#  Q(df = 1) = 1.7389, p-val = 0.1873
#
#Model Results:
#  
#estimate      se     zval    pval    ci.lb    ci.ub    
#-0.2793  0.1162  -2.4040  0.0162  -0.5070  -0.0516  * 
  
round(exp(-0.0516 ),2) #0.76 (0.60-0.95)


####bmi分层_超重_分类变量-up-meta——re####
data<-read.csv("C:/Users/LLLLL/Desktop/小论文/code/Meta/meta_qg_up_by_bmi_ow.csv")
meta_result<-rma.uni(log(hru), sei = seu, data = data, method = "REML")
print(meta_result)

#Random-Effects Model (k = 2; tau^2 estimator: REML)
#
#tau^2 (estimated amount of total heterogeneity): 0.0128 (SE = 0.0239)
#tau (square root of estimated tau^2 value):      0.1129
#I^2 (total heterogeneity / total variability):   75.37%
#H^2 (total variability / sampling variability):  4.06
#
#Test for Heterogeneity:
#  Q(df = 1) = 4.0608, p-val = 0.0439
#
#Model Results:
#  
#estimate      se    zval    pval   ci.lb   ci.ub    
#0.1875  0.0917  2.0451  0.0408  0.0078  0.3672  * 
  
round(exp(0.3672 ),2) #1.21 (1.01-1.44)



####bmi分层_超重_连续变量的HR值得线性趋势检验####

data<-data.frame(
  Score_Level=c(1,2,3),  # 多基因评分层级
  HR_Value=c(1.13,1.27,1.41)  # 对应的meta分析后的HR值
)

# 拟合线性模型
model<-lm(HR_Value~Score_Level,data = data)

# 查看模型摘要
summary(model)

# 绘制散点图并添加线性回归线
ggplot(data, aes(x = Score_Level, y = HR_Value)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, col = "blue") +
  labs(x = "多基因评分层级", y = "Meta分析后的HR值") +
  ggtitle("多基因评分层级与HR值的线性关系")

