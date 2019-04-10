#Estimación del caso 16.1 mediante el paquete matrixpls version LAVAAN

library(haven)
Datos_16_1_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_16_1_Caso.sav")
View(Datos_16_1_Caso)

#===================================
# Renombramos al fichero de datos
#===================================
#para llamar siempre al fichero de datos "datos"

datos<-Datos_16_1_Caso

library(lavaan)
library(matrixpls)
library(plspm)
library(semPLS)



#===================================
# Modelo Lavaan
#===================================

modelo.lavaan<-'

facuso=~facuso1+facuso3+facuso4+facuso6
utiper=~utiper1+utiper2+utiper3+utiper4+utiper5+utiper6
actitud=~actitud1+actitud2+actitud4+actitud5
        +actitud6+actitud7+actitud8+actitud10
intcomp=~intcomp1
dep<~dep1+dep2+dep3+dep4+dep5+dep6

intcomp~actitud+utiper+dep
actitud~dep+utiper+facuso
dep~facuso+utiper
utiper~facuso

'

#Estimacion e indicadores de calidad OBLIGATORIO que use matriz de covarianzas no raw data
modelo.lavaan.out <- matrixpls(cov(datos),modelo.lavaan)
summary(modelo.lavaan.out)

#Blindfolding

predictions.blindfold <- matrixpls.crossvalidate(cov(datos), model = modelo.lavaan, blindfold = TRUE, predictionType = "redundancy", groups = 7)

q2(cov(datos), predictions.blindfold, model=modelo.lavaan.pls)


#Estimaciones e Indicadores de calidad

set.seed(1)
boot.out <- matrixpls.boot(cov(datos), model = modelo.lavaan, R = 500,
                           parallel = "multicore", ncpus = parallel::detectCores())
summary(boot.out)  

#======================================
# Diagnóstico colinealidad formativos
#======================================

library(car)

colinealidad <- lm(facuso1~dep1+dep2+dep3+dep4+dep5+dep6, data=datos)
vif(colinealidad)

#======================================
# Potencia de las regresiones
#======================================

# N= (tamaño muestral) 
# u = número de variables independientes de la mayor regresión
# V = N -u -1
# f2 = 0.15 (efecto medio)
# sig.level es elnivel de significación, normalmente 0.05
 
#En nuestro ejemplo tenemos 464 y la regresión más complicada es la del constructo formativo
#con 6 regresores por lo que v=464-6-1=457. Se deja como NULL lo que se quiere calcular

library(pwr)
pwr.f2.test(u =6, v = 457 , f2 =0.15 , sig.level = 0.05, power = NULL)

#Si quisiéramos saber la muestra necesaria para alcanzar una potencia del 80%

library(pwr)
pwr.f2.test(u =6, v = NULL , f2 =0.15 , sig.level = 0.05, power = 0.80)

