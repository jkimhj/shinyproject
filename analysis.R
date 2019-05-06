library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(shinydashboard)
library(plotly)
library(DT)
library(googleVis)
library(reshape)
library(reshape2)
library(leaflet)
library(zipcode)
library(data.table)
library(vioplot)

data = read.csv("DOHMH_Childcare_Center_Inspections.csv", stringsAsFactors = FALSE)
data = as.data.frame(data)
names(data) <- tolower(names(data))
data = data %>% group_by(day.care.id) %>% arrange(day.care.id)

center_table = select(data, day.care.id, center.name, borough, zipcode, maximum.capacity, 
                      program.type, facility.type, violation.rate.percent, 
                      total.educational.workers, public.health.hazard.violation.rate, 
                      critical.violation.rate)
center_table = center_table %>% filter(!is.na(violation.rate.percent), !is.na(public.health.hazard.violation.rate), 
                             !is.na(critical.violation.rate), !is.na(zipcode), !is.na(maximum.capacity),
                             !is.na(facility.type),!is.na(program.type), !is.na(total.educational.workers))
center_table$facility.type = toupper(center_table$facility.type)
center_table = unique(center_table)

viol_rate = center_table$violation.rate.percent
phh_rate = center_table$public.health.hazard.violation.rate
crit_rate = center_table$critical.violation.rate

###correlation between viol_rate, phh_rate, crit_rate
cor(viol_rate, phh_rate)  #0.6562685, p-value < 0.05
cor(viol_rate, crit_rate) #0.8868788, p-value < 0.05
cor(phh_rate, crit_rate) #0.4227864, p-value < 0.05


###One-Way ANOVA TEST ############################################
#1. Violation Rate Percent 
viol1 = center_table$violation.rate.percent[center_table$borough=="BRONX"]
viol2 = center_table$violation.rate.percent[center_table$borough=="BROOKLYN"]
viol3 = center_table$violation.rate.percent[center_table$borough=="MANHATTAN"]
viol4 = center_table$violation.rate.percent[center_table$borough=="QUEENS"]
viol5 = center_table$violation.rate.percent[center_table$borough=="STATEN ISLAND"]
boxplot(viol1, viol2, viol3, viol4, viol5, 
        names=c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND"), 
        col="green") 
title("Box plots of Violation Rate Percent by Borough", 
      xlab = "Borough", ylab="Rate (%)")
model1<- aov(center_table$violation.rate.percent~center_table$borough)

#Df  Sum Sq Mean Sq F value Pr(>F)    
#center_table$borough    4  126579   31645   39.92 <2e-16 ***
#  Residuals            2860 2266925     793                   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#####The F-statistic value is 39.92 and it is highly signficiant as the p-value is 
####much less than the level of significance (1% or 0.01). Therefore, reject the 
####null hypothesis of equal mean value of violation rate percent across all the
####boroughs. The mean value of violation rate percent of the five boroughs are not equal.

#2. PHH Rate  
phh1 = center_table$public.health.hazard.violation.rate[center_table$borough=="BRONX"]
phh2 = center_table$public.health.hazard.violation.rate[center_table$borough=="BROOKLYN"]
phh3 = center_table$public.health.hazard.violation.rate[center_table$borough=="MANHATTAN"]
phh4 = center_table$public.health.hazard.violation.rate[center_table$borough=="QUEENS"]
phh5 = center_table$public.health.hazard.violation.rate[center_table$borough=="STATEN ISLAND"]
boxplot(phh1, phh2, phh3, phh4, phh5, 
        names=c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND"), 
        col="red") 
title("Box plots of Public Health Hazard Violation Rate by Borough", 
      xlab = "Borough", ylab="Rate (%)")
model2<- aov(center_table$public.health.hazard.violation.rate~center_table$borough)
summary(model2)
#Df  Sum Sq Mean Sq F value   Pr(>F)    
#center_table$borough    4   40774   10194   19.79 4.36e-16 ***
#  Residuals            2860 1472983     515                     
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#3. Critical Violation Rate
crit1 = center_table$critical.violation.rate[center_table$borough=="BRONX"]
crit2 = center_table$critical.violation.rate[center_table$borough=="BROOKLYN"]
crit3 = center_table$critical.violation.rate[center_table$borough=="MANHATTAN"]
crit4 = center_table$critical.violation.rate[center_table$borough=="QUEENS"]
crit5 = center_table$critical.violation.rate[center_table$borough=="STATEN ISLAND"]
boxplot(crit1, crit2, crit3, crit4, crit5, 
        names=c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND"), 
        col="blue") 
title("Box plots of Critical Violation Rate by Borough", 
      xlab = "Borough", ylab="Rate (%)")
model3<- aov(center_table$critical.violation.rate~center_table$borough)
summary(model3)

#Df  Sum Sq Mean Sq F value Pr(>F)    
#center_table$borough    4  103806   25951   35.66 <2e-16 ***
#  Residuals            2860 2081105     728                   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



#####Tukey's Honestly Significant Differences #######
TukeyHSD(model1, conf.level = 0.99)
plot(TukeyHSD(model1, conf.level = 0.99),las=1, col = "green")
# Tukey multiple comparisons of means
# 99% family-wise confidence level
# 
# Fit: aov(formula = center_table$violation.rate.percent ~ center_table$borough)
# 
# $`center_table$borough`
# diff         lwr        upr     p adj
# BROOKLYN-BRONX          -19.8426324 -25.2290866 -14.456178 0.0000000
# MANHATTAN-BRONX         -18.6375109 -24.3473750 -12.927647 0.0000000
# QUEENS-BRONX            -14.8061798 -20.6376423  -8.974717 0.0000000
# STATEN ISLAND-BRONX     -19.7032752 -28.8071661 -10.599384 0.0000000
# MANHATTAN-BROOKLYN        1.2051215  -3.3204427   5.730686 0.9087772
# QUEENS-BROOKLYN           5.0364526   0.3584041   9.714501 0.0041930
# STATEN ISLAND-BROOKLYN    0.1393572  -8.2724805   8.551195 0.9999981
# QUEENS-MANHATTAN          3.8313311  -1.2157271   8.878389 0.0970627
# STATEN ISLAND-MANHATTAN  -1.0657643  -9.6882739   7.556745 0.9944720
# STATEN ISLAND-QUEENS     -4.8970954 -13.6006050   3.806414 0.3546301
############## The results show that all the pairs of violation rate are statistically
######## significantly different except for the pair STATEN ISLAND-BROOKLYN and 
#######STATEN ISLAND-MANHATTAN. The pair-wise difference between BROOKLYN-BRONX 
######is -19.8426324, which means that BRONX has higher violation rate than 
#####BROOKLYN and this is statistically significant. 
#####Similar significances appear in all the pairs that include BRONX.

TukeyHSD(model2, conf.level = 0.99)
plot(TukeyHSD(model2, conf.level = 0.99),las=1, col = "red")

# Tukey multiple comparisons of means
# 99% family-wise confidence level
# 
# Fit: aov(formula = center_table$public.health.hazard.violation.rate ~ center_table$borough)
# 
# $`center_table$borough`
# diff        lwr         upr     p adj
# BROOKLYN-BRONX          -11.1786965 -15.520631 -6.83676230 0.0000000
# MANHATTAN-BRONX          -5.8410530 -10.443683 -1.23842330 0.0003529
# QUEENS-BRONX             -4.9648821  -9.665530 -0.26423386 0.0053200
# STATEN ISLAND-BRONX      -7.2412279 -14.579728  0.09727231 0.0115424
# MANHATTAN-BROOKLYN        5.3376435   1.689659  8.98562807 0.0000194
# QUEENS-BROOKLYN           6.2138144   2.442915  9.98471418 0.0000009
# STATEN ISLAND-BROOKLYN    3.9374686  -2.843179 10.71811582 0.3219232
# QUEENS-MANHATTAN          0.8761709  -3.192182  4.94452352 0.9561708
# STATEN ISLAND-MANHATTAN  -1.4001749  -8.350641  5.55029161 0.9654999
# STATEN ISLAND-QUEENS     -2.2763458  -9.292105  4.73941346 0.8284439

TukeyHSD(model3, conf.level = 0.99)
plot(TukeyHSD(model3, conf.level = 0.99),las=1, col = "blue")

# Tukey multiple comparisons of means
# 99% family-wise confidence level
# 
# Fit: aov(formula = center_table$critical.violation.rate ~ center_table$borough)
# 
# $`center_table$borough`
# diff        lwr        upr     p adj
# BROOKLYN-BRONX          -16.795517 -21.956488 -11.634546 0.0000000
# MANHATTAN-BRONX         -18.091978 -23.562821 -12.621135 0.0000000
# QUEENS-BRONX            -14.495792 -20.083143  -8.908441 0.0000000
# STATEN ISLAND-BRONX     -19.673418 -28.396210 -10.950627 0.0000000
# MANHATTAN-BROOKLYN       -1.296461  -5.632580   3.039658 0.8669586
# QUEENS-BROOKLYN           2.299725  -2.182495   6.781945 0.4518701
# STATEN ISLAND-BROOKLYN   -2.877902 -10.937610   5.181807 0.7723550
# QUEENS-MANHATTAN          3.596186  -1.239596   8.431969 0.1095294
# STATEN ISLAND-MANHATTAN  -1.581440  -9.843002   6.680121 0.9713358
# STATEN ISLAND-QUEENS     -5.177627 -13.516798   3.161544 0.2552626


#########Chi square testing
tbl1 = table(center_table$violation.rate.percent, center_table$borough)
chisq.test(tbl1)
# Pearson's Chi-squared test
# 
# data:  tbl1
# X-squared = 400.72, df = 128, p-value < 2.2e-16
#######As the p-value is less than the .05 significance level, 
# we reject the null hypothesis that the violation rate percent is independent 
# of the borough. In other words, we favor the null hypothesis that 
#the violation rate percent is affected by borough. 

tbl2 = table(center_table$public.health.hazard.violation.rate, 
             center_table$borough)
chisq.test(tbl2)
# Pearson's Chi-squared test
# 
# data:  tbl2
# X-squared = 267.08, df = 104, p-value = 2.665e-16

tbl3 = table(center_table$critical.violation.rate, 
             center_table$borough)
chisq.test(tbl3)

# Pearson's Chi-squared test
# 
# data:  tbl3
# X-squared = 403.94, df = 124, p-value < 2.2e-16



###correlation between maximum capacity and viol_rate###################################
cor.test(viol_rate, center_table$maximum.capacity)  

# Pearson's product-moment correlation
# 
# data:  viol_rate and center_table$maximum.capacity
# t = -1.9618, df = 2863, p-value = 0.04988
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -7.316130e-02 -1.937356e-05
# sample estimates:
#         cor 
# -0.03663941 

###One-Way ANOVA TEST ############################################
#Maximum capacity across Borough
maxcap1 = center_table$maximum.capacity[center_table$borough=="BRONX"]
maxcap2 = center_table$maximum.capacity[center_table$borough=="BROOKLYN"]
maxcap3 = center_table$maximum.capacity[center_table$borough=="MANHATTAN"]
maxcap4 = center_table$maximum.capacity[center_table$borough=="QUEENS"]
maxcap5 = center_table$maximum.capacity[center_table$borough=="STATEN ISLAND"]
boxplot(maxcap1, maxcap2, maxcap3, maxcap4, maxcap5, 
        names=c("BRONX", "BROOKLYN", "MANHATTAN", "QUEENS", "STATEN ISLAND"), 
        col="green", ylim=c(0,1000))
title("Box plots of Max Capacity by Borough", 
      xlab = "Borough", ylab="Max Capacity (people)")

maxcap_model<- aov(center_table$maximum.capacity~center_table$borough)
summary(maxcap_model)
#                         Df   Sum Sq Mean Sq F value Pr(>F)
# center_table$borough    4    34401    8600   0.514  0.725
# Residuals            2860 47855377   16733 

#####Tukey's Honestly Significant Differences ########################
TukeyHSD(maxcap_model, conf.level = 0.99)
plot(TukeyHSD(maxcap_model, conf.level = 0.99),las=1, col = "green")
# 
# Tukey multiple comparisons of means
# 99% family-wise confidence level
# 
# Fit: aov(formula = center_table$maximum.capacity ~ center_table$borough)
# 
# $`center_table$borough`
# diff       lwr      upr     p adj
# BROOKLYN-BRONX           4.4131907 -20.33536 29.16174 0.9779025
# MANHATTAN-BRONX         -4.3963609 -30.63085 21.83813 0.9824519
# QUEENS-BRONX             0.4571268 -26.33605 27.25031 0.9999978
# STATEN ISLAND-BRONX      4.8262429 -37.00241 46.65489 0.9957617
# MANHATTAN-BROOKLYN      -8.8095515 -29.60266 11.98356 0.6404500
# QUEENS-BROOKLYN         -3.9560639 -25.44978 17.53765 0.9751700
# STATEN ISLAND-BROOKLYN   0.4130522 -38.23590 39.06200 0.9999997
# QUEENS-MANHATTAN         4.8534876 -18.33568 28.04265 0.9604171
# STATEN ISLAND-MANHATTAN  9.2226037 -30.39430 48.83950 0.9423486
# STATEN ISLAND-QUEENS     4.3691161 -35.61994 44.35818 0.9965693

#########Chi square testing
maxcap_tbl = table(center_table$maximum.capacity, viol_rate)
chisq.test(maxcap_tbl)
# Pearson's Chi-squared test
# 
# data:  maxcap_tbl
# X-squared = 8468.8, df = 8960, p-value = 0.9999




###One-Way ANOVA TEST ############################################
#Facility Type
facil1 = center_table$violation.rate.percent[center_table$facility.type=="GDC"]
facil2 = center_table$violation.rate.percent[center_table$facility.type=="SBCC"]
facil3 = center_table$violation.rate.percent[center_table$facility.type=="CAMP"]

boxplot(facil1, facil2, facil3, names=c("GDC", "SBCC", "CAMP"), col="red") 
title("Box plots of Violation Rate Percent by Facility Type", 
      xlab = "Facility Type", ylab="Rate (%)")
facil_model<- aov(center_table$violation.rate.percent~center_table$facility.type)
summary(facil_model)
#                               Df  Sum Sq Mean Sq F value  Pr(>F)   
# center_table$facility.type    2   11487    5744   6.901 0.00102 **
#   Residuals                  2862 2382016     832                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#####Tukey's Honestly Significant Differences ########################
TukeyHSD(facil_model, conf.level = 0.99)
plot(TukeyHSD(facil_model, conf.level = 0.99),las=1, col = "red")
# 99% family-wise confidence level
# 
# Fit: aov(formula = center_table$violation.rate.percent ~ center_table$facility.type)
# 
# $`center_table$facility.type`
# diff       lwr       upr     p adj
# GDC-CAMP  1.309662 -2.836137  5.455461 0.6269646
# SBCC-CAMP 6.513450  1.004182 12.022717 0.0016638
# SBCC-GDC  5.203788  0.672616  9.734959 0.0023707


#########Chi square testing
facil_tbl = table(center_table$violation.rate.percent, center_table$facility.type)
chisq.test(facil_tbl)
# Pearson's Chi-squared test
# 
# data:  facil_tbl
# X-squared = 938.15, df = 64, p-value < 2.2e-16
# 
# Warning message:
# In chisq.test(facil_tbl) : Chi-squared approximation may be incorrect




###One-Way ANOVA TEST ############################################
#Program Type
prog1 = center_table$violation.rate.percent[center_table$program.type=="PRESCHOOL"]
prog2 = center_table$violation.rate.percent[center_table$program.type=="INFANT TODDLER"]
prog3 = center_table$violation.rate.percent[center_table$program.type=="ALL AGE CAMP"]
prog4 = center_table$violation.rate.percent[center_table$program.type=="SCHOOL AGE CAMP"]

boxplot(prog1, prog2, prog3, prog4, names=c("PRESCHOOL", "INFANT TODDLER", "ALL AGE CAMP", "SCHOOL AGE CAMP"), col="blue") 
title("Box plots of Violation Rate Percent by Program Type", 
      xlab = "Program Type", ylab="Rate (%)")
prog_model<- aov(center_table$violation.rate.percent~center_table$program.type)
summary(prog_model)

#                             Df  Sum Sq Mean Sq F value Pr(>F)
# center_table$program.type    3    3855  1285.1   1.539  0.202
# Residuals                 2861 2389649   835.2  

#####Tukey's Honestly Significant Differences ########################
TukeyHSD(prog_model, conf.level = 0.99)
plot(TukeyHSD(prog_model, conf.level = 0.99),las=1, col = "blue")
# Tukey multiple comparisons of means
# 99% family-wise confidence level
# 
# Fit: aov(formula = center_table$violation.rate.percent ~ center_table$program.type)
# 
# $`center_table$program.type`
# diff        lwr       upr     p adj
# INFANT TODDLER-ALL AGE CAMP     0.3778064  -5.780774  6.536386 0.9975247
# PRESCHOOL-ALL AGE CAMP          2.3081965  -2.224560  6.840953 0.3863298
# SCHOOL AGE CAMP-ALL AGE CAMP   -4.2959971 -21.229516 12.637521 0.8587930
# PRESCHOOL-INFANT TODDLER        1.9303901  -3.133275  6.994055 0.6345652
# SCHOOL AGE CAMP-INFANT TODDLER -4.6738034 -21.757096 12.409489 0.8291922
# SCHOOL AGE CAMP-PRESCHOOL      -6.6041936 -23.170927  9.962539 0.5999884

#########Chi square testing
prog_tbl = table(center_table$violation.rate.percent, center_table$program.type)
chisq.test(prog_tbl)
# 
# Pearson's Chi-squared test
# 
# data:  prog_tbl
# X-squared = 575.18, df = 96, p-value < 2.2e-16



#########################Total Educational Workers#######################
# > summary(center_table$total.educational.workers)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.000   6.000   8.353  12.000 115.000 

cor.test(viol_rate, center_table$total.educational.workers)
# Pearson's product-moment correlation
# 
# data:  viol_rate and center_table$total.educational.workers
# t = 1.8497, df = 2863, p-value = 0.06446
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.002074591  0.071078225
# sample estimates:
#        cor 
# 0.03454809 

staff_tbl = table(center_table$violation.rate.percent, center_table$total.educational.workers)
chisq.test(staff_tbl)
# Pearson's Chi-squared test
# 
# data:  staff_tbl
# X-squared = 2925.1, df = 1952, p-value < 2.2e-16
# 
# Warning message:
# In chisq.test(staff_tbl) : Chi-squared approximation may be incorrect