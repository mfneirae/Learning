#==================================
# librerias
#==================================

library(haven)
library(MASS)
library(stargazer)
library(pastecs)

#==================================
# Datos
#==================================

Datos_8_3_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_8_3_Caso.sav")
datos<-Datos_8_3_Caso

#==================================
# Estimación modelo
#==================================

fit<-lm(data=datos,absen~edad+antigue+salario)
stargazer(fit,type="text")

stat.desc(datos$absen,basic=FALSE,desc=TRUE)
anova(fit)
