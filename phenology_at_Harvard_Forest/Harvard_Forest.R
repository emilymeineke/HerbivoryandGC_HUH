setwd("C:/Users/davies/Documents/GitHub/HerbivoryandGC_HUH")

data<-read.csv("phenology at Harvard Forest/HF_phenology.csv")
data<-read.csv("HF_phenology.csv")

#short function to extract year as a numeric variable
substrRight <- function(x, n){
  sapply(x, function(xx)
         substr(xx, (nchar(xx)-n+1), nchar(xx))
         )
}
yr<- as.numeric(substrRight(as.vector(data$date),2))
data<-cbind(data, yr)

#show list of species codes
unique(data$species)
# [1] ACPE ACRU ACSA AMSP ARSP BEAL BELE BEPA BEPO CADE COAL CRSP FAGR FRAM HAVI ILVE KAAN KALA LYLI NEMU NYSY PIST POTR PRSE
#[25] QUAL QURU QUVE RHSP SAPU TSCA VACO VIAL VICA


#subset data to just blueberry
blueberry<-subset(data, species=="VACO")

#subset leaf bud break
bb.leaf<-subset(blueberry, bcon == "BB")#leaf bud break
#subset flower bud break
bb.flower<-subset(blueberry, fbud == "BB")#flower bud breaks


#calculate date of first flower/leaf bud for each individual each year
flower<-aggregate(bb.flower["julian"],bb.flower[c("yr","tree.id")], FUN= min)
leaf<-aggregate(bb.leaf["julian"],bb.leaf[c("yr","tree.id")], FUN= min)

#merge back leaf and flower dates- n.b. x = lower doy, y = leaf doy
bb<-merge(flower, leaf, by = c("yr", "tree.id"))

#plot the data coloured by tree ID
plot(bb$julian.x, bb$julian.y, col = bb$tree.id, pch=19, xlab = "FFD", ylab = "FLD")

#run the LME
library(nlme)
library(MuMIn)
rand.int.model<-lme(julian.y~ julian.x, random = ~1|tree.id, data = bb)
summary(rand.int.model)
#r.squaredGLMM(rand.int.model)

#Linear mixed-effects model fit by REML
# Data: bb 
#       AIC      BIC    logLik
#  319.6337 326.9482 -155.8168
#
#Random effects:
# Formula: ~1 | tree.id
#         (Intercept) Residual
#StdDev: 0.0003078283 6.291669
#
#Fixed effects: julian.y ~ julian.x 
#               Value Std.Error DF  t-value p-value
#(Intercept) 57.04264 15.274266 43 3.734559   5e-04
#julian.x     0.46891  0.114587 43 4.092205   2e-04
# Correlation: 
#         (Intr)
#julian.x -0.998
#
#Standardized Within-Group Residuals:
#        Min          Q1         Med          Q3         Max 
#-2.16610523 -0.30709863  0.01953268  0.69863050  1.84776329 
#
#Number of Observations: 48
#Number of Groups: 4 


#LM - Just to take a look at the r-sq
lm.model<-lm(julian.y~ julian.x + tree.id, data = bb)
summary(lm.model)

#Call:
#lm(formula = julian.y ~ julian.x + tree.id, data = bb)
#
#Residuals:
#     Min       1Q   Median       3Q      Max 
#-13.7050  -2.4218   0.3088   3.7303  13.8397 
#
#Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     53.2810    15.9113   3.349 0.001697 ** 
#julian.x         0.5004     0.1194   4.190 0.000136 ***
#tree.idVACO-02   1.3758     2.6096   0.527 0.600772    
#tree.idVACO-03  -2.1673     2.6072  -0.831 0.410398    
#tree.idVACO-04  -0.9179     2.6269  -0.349 0.728473    
#---
#Signif. codes:  0 ?***? 0.001 ?**? 0.01 ?*? 0.05 ?.? 0.1 ? ? 1
#
#Residual standard error: 6.368 on 43 degrees of freedom
#Multiple R-squared:  0.2981,    Adjusted R-squared:  0.2328 
#F-statistic: 4.565 on 4 and 43 DF,  p-value: 0.003674
