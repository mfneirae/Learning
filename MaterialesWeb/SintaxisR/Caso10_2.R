# ================================
# Librerias
# ================================

library(foreign)
library(nnet)
library(stargazer)
library(ggplot2)
library(gmodels)

# ================================
# Datos
# ================================


# Se obtiene el fichero de datos directamente de UCLA
datos = read.dta("http://www.ats.ucla.edu/stat/data/hsb2.dta")

# Nos aseguramos a efectos de interpretación que el nivel de referencia de la dependiente es la clase baja
datos$ses2 = relevel(datos$ses, ref = "low")

# ================================
# Datos
# ================================

CrossTable(datos$female,datos$ses,chisq=TRUE,
           prop.chisq = FALSE, prop.c = FALSE, prop.t=FALSE)
aggregate(cbind(science,socst)~ses, data=datos, mean, na.rm=TRUE)

# ================================
# Estimación
# ================================
multi1 <- multinom(ses2 ~ science + socst + female,data=datos)
summary(multi1)

# multinom() no da los valores p, usamos el paquete stargazer() 

library(stargazer)
stargazer(multi1, type="text")


# Aunque podríamos hacerlo manualmente
z <- summary(multi1)$coefficients/summary(multi1)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Relative risk ratios allow an easier interpretation of the logit coefficients. They are the exponentiated value of the logit coefficients.

multi1.rrr = exp(coef(multi1))
multi1.rrr

stargazer(multi1, type="text", coef=list(multi1.rrr), p.auto=FALSE)

# ================================
# Ajuste
# ================================

# Estimamos el modelo solo con la constante
multi0 <- multinom(ses2 ~ 1,data=datos)

# Calculamos el estadístico chi cuadrado como 
# diferencia de sus -2LL (deviance)

chi2<-multi0$deviance-multi1$deviance
df.chi2<-multi1$edf-multi0$edf
Sig.chi2<-1-pchisq(chi2,df.chi2)
print(cbind(chi2,df.chi2,Sig.chi2))

# R2 de McFadden
R2MF<-(multi0$deviance-multi1$deviance)/multi0$deviance
