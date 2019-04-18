#********************************************************************************
#                          INSTALL PACKAGES SECTION
#********************************************************************************

install.packages("RcmdrMisc")
install.packages("tidyverse")
install.packages("ForImp")
install.packages("psych")
install.packages("pastecs")


#********************************************************************************
#                          SETTING WORKING DIRECTORY
#********************************************************************************

getwd()
setwd("C:/Users/DECA/Desktop/Learning/")

#********************************************************************************
#                          IMPORT LIBRARIES
#********************************************************************************

library(readxl)
library("ggpubr")
library(RcmdrMisc)
library(haven)
library(ForImp)
library(car)
library(psych)
library(pastecs)

# OBTAINING DATA
BD1 <- read_excel("BD1 Test.xlsx", sheet = "Sheet1")
summary(BD1)

#T TEST OF V2 AND V4A_D & V4B_D
t.test(V2~ Va_d, 
       alternative="two.sided", 
       conf.level=.95, 
       var.equal = TRUE, 
       data = BD1)

t.test(V2~ Vb_d, 
       alternative="two.sided", 
       conf.level=.95, 
       var.equal = TRUE, 
       data = BD1)

#CORRELATION MATRIX
rcorr.adjust(
  BD1[c("V2d","Va_d","Vb_d")], 
  type="pearson", 
  use="complete")

#IMPLEMENTING DATA TREATMENT
BD1T<-ld(within(BD1,{V4b <- NULL}))
show(BD1T)
BD1T<-ld(within(BD1,{V4b <- NULL}))
numSummary(BD1T[c("C1","V1","V2","V3","V4a","V5")],statistics = c("mean","sd"))

#REMOVE DATA BASED ON A SINGLE COLUMN VALUE
BD2T<-subset(BD1, !is.na(V2), data=BD1)
numSummary(BD2T[c("V4a","V1","V2","V3","V5","C1")],statistics = c("mean","sd"))
BD2T<-subset(BD1, !is.na(V4a), data=BD1)
numSummary(BD2T[c("V4a","V1","V2","V3","V5","C1")],statistics = c("mean","sd"))
numSummary(BD1[c("V4a","V1","V2","V3","V5","C1")],statistics = c("mean","sd"))

#REGRESSION IMPUTATION   
BD1R<-lm(V4a~V1+V3+V5+C1, data=BD1)
summary(BD1R)
colnames(summary(BD1R)$coef)
V1<-summary(BD1R)$coef[,"Estimate"]["V1"]
V3<-summary(BD1R)$coef[,"Estimate"]["V3"]
V5<-summary(BD1R)$coef[,"Estimate"]["V5"]
C1<-summary(BD1R)$coef[,"Estimate"]["C1"]
Intercept<-summary(BD1R)$coef[,"Estimate"]["(Intercept)"]
BD1I<-BD1
for (x in 1:dim(BD1)[1]) {
  if (is.na(BD1$V4a[x])) {
    BD1I$V4a[x]<-Intercept+V1*BD1$V1[x]+V3*BD1$V3[x]+V5*BD1$V5[x]+C1*BD1$C1[x]
    print(BD1I$V4a[x]) 
  }
}
#CRITICAL VALUE OF A T DISTRIBUTION
qt((0.05/200),98,lower.tail = F)

#MULTI VARIABLE ATIPICAL DATA DETECTION
X<-matrix(c(13,5031,130,1168,12,
            16,5735,165,1231,12,
            13,5211,150,1145,11,
            15,4982,150,1144,12,
            14,4949,140,1149,11,
            16,7030,198,1447,10,
            17,7440,220,1451,9,
            17,7210,215,1437,9,
            17,7456,225,1475,10,
            16,6391,190,1283,9),nrow = 10,ncol = 5, byrow = T)

mean<-colMeans(X)                             
Sx<-cov(X)                                    
D2<-mahalanobis(X,mean,Sx,inverted = FALSE)   
#FIND CRITICAL MAHALANOBIS VALUE 
#1. DF = # of Variables (Significativity)
pchisq(D2, df=5, lower.tail=FALSE)       
#2. Find Critical Value with
qchisq(.99, df=5)     

#NORMAL
round(stat.desc(X, basic = F, norm = T),
      digits=3)
