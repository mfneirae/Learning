#==================================
# librerias
#==================================

library(haven)
library(MASS)
library(stargazer)
library(psych)
library(ggfortify)
library(car)
library(gvlma)
library(perturb)
library(moments)
library(fBasics)
library(normtest)
library(nortest)
library(lmtest)


#==================================
# Datos
#==================================

Datos_8_5_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_8_5_Caso.sav")
datos<-Datos_8_5_Caso

#==================================
# Descriptivos
#==================================

pairs.panels(datos[2:5],ellipses = FALSE,hist.col = "darkgrey",lm=TRUE,cex=1)

#==================================
# Estimación modelo
#==================================

fit<-lm(data=datos,perf~iq+mot+soc)
stargazer(fit,type="text")

#==================================
# Diagnosticos
#==================================

# gráficos de residuos
#==================================
autoplot(fit, label.size = 3)

#Normalidad
#==================================
jarqueberaTest(fit$resid)
ajb.norm.test(fit$resid, nrepl=2000)
shapiro.test(fit$resid)
lillie.test(fit$resid)


#Independencia de los errores
#==================================
durbinWatsonTest(fit)

#linearity
#==================================
crPlots(fit,smooth=FALSE,col.lines=c("red", "red"))

#Homoscedasticidad
#==================================
#Breusch Pagan Test
bptest(fit) 

#Test de White, se aplica con el anterior pero hay que añadir todas las
#interacciones entre los regresores y sus cuadrados
bptest(fit,~iq*mot+iq*soc+soc*mot+I(iq^2)+I(mot^2)+I(soc^2),data=datos)

#Test de Goldfeld-Quandt
gqtest(fit)

spreadLevelPlot(fit)

#Validación global
#==================================
gvlma(fit)

#multicolinealidad
#==================================
vif(fit)
colldiag(fit)

#outliers
#==================================
outlierTest(fit)

#Casos influyentes
#==================================

#Distancia de Cook
cutoff <- 4/(nrow(datos)-length(fit$coefficients)-2)
plot(fit, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")

#hat values-leverage
hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit)

#Added variable plots
avPlots(fit, ask=FALSE, id.method="identify")

#Gráficos de influencia
influencePlot(fit, id.method="identify", main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")

