library(haven)
Ilustracion_13_1 <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Ilustracion_13_1.sav")
datos<-Ilustracion_13_1

summary(lm(Y~X,datos))
