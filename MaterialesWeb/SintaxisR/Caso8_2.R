#==================================
# librerias
#==================================

library(haven)
library(MASS)

#==================================
# Datos
#==================================

Datos_8_2_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_8_2_Caso.sav")
datos<-Datos_8_2_Caso

#==================================
# Estimación modelo
#==================================

fit<-lm(data=datos,cafeqt~cafepr)
