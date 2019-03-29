#********************************************************************************
#                          INSTALL PACKAGES SECTION
#********************************************************************************

install.packages("RcmdrMisc")
install.packages("tidyverse")
install.packages("ForImp")

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
