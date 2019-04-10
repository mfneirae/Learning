library(haven)
Datos_14_1_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_14_1_Caso.sav")

datos<-Datos_14_1_Caso
datos<-data.frame(datos)

#========================================
# Cargamos librerías  
#========================================

library(psych)
library(lavaan)
library(ggplot2)
library(semTools)
library(semPlot)

#========================================
# Alpha de Cronbach con item equivocado
#========================================

myvars <- c("A1", "A2", "A3", "B1")
subescala1 <- datos[myvars]

alpha(subescala1,check.keys = FALSE)

#========================================
# Alpha de Cronbach correcto
#========================================

myvars1 <- c("A1", "A2", "A3")
subescala.conocidos <- datos[myvars1]

myvars2 <- c("B1", "B2", "B3")
subescala.donaciones <- datos[myvars2]

alpha(subescala.conocidos,check.keys = FALSE)
alpha(subescala.donaciones,check.keys = FALSE)


#========================================
# CFA para la validación del instrumento de medida
#========================================


modelo.cfa <- '

# Modelo de medida

conocidos   =~ A1+A2+A3
donaciones  =~ B1+B2+B3


#Varianzas de los factores

conocidos~~conocidos
donaciones~~donaciones

#Covarianzas

conocidos~~donaciones

#Varianzas de los términos de error

A1~~A1
A2~~A2
A3~~A3
B1~~B1
B2~~B2
B3~~B3

'

#Estimación del modelo

fit <- lavaan(modelo.cfa, data=datos, std.lv=TRUE, mimic="eqs", estimator="ML", verbose=TRUE, warn=TRUE) 

#Petición de elementos en la salida

summary (fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
resid(fit, type="cor")

#Indices de modificación

modindices(fit, sort.=TRUE, minimum.value = 3.84)

# =======================================
# GRÁFICO DE RESIDUOS TÍPICO DE EQS
# =======================================


# Para sacar el gráfico de residuos estandarizados de EQS los extraemos de la matriz
# a es una matriz simétrica pero para hacer el histograma los pasamos a un vector que
# llamamos b

a<-resid(fit,type="cor")$cor
b<-c(a[lower.tri(a)])

qplot(b,
      geom="histogram",
      binwidth = 0.01,  
      main = "Gráfico de residuos", 
      xlab = "Tamaño del residuo",
      ylab = "frecuencia",
      breaks=seq(-0.5,0.5,by=0.05),
      fill=I("black"), 
      col=I("white"), 
      alpha=I(.7),
      xlim=c(-0.5,0.5))

# =======================================
# FIABILIDAD Y VALIDEZ CONVERGENTE
# =======================================


# Nos calculamos a mano CA, CR y AVE
# El CA mediante la fórmula de las correlaciones

# Sacamos la matriz de correlaciones entre los indicadores

matriz_cov_muestral<-lavInspect(fit,"sampstat")$cov

# Y la convertimos en la matriz de correlaciones que necesita el CA

matriz_cor_muestral<-cov2cor(matriz_cov_muestral)

# Hay que saber cuantos indicadores tiene cada factor porque los vamos extrayendo
# de la matriz de correlaciones [1:5,1:5] implica que el factor f1 tiene 5 indicadores
# que están en las filas 1 a la 5 y las columnas 1 a la 5 (logicamente) de la matriz
# de correlaciones

# Hay dos formas de poner la información, si están correlativos como se ve en cor_f2
# si estuvieran salteados como en cor_f1, aunque en cor_f1 están ordenados, sería algo
# de la forma: [c(1,2,3,7),c(1,2,3,7)]. cor_f1 lo podríamos haber puesto también al estar
# correlativos como [1:5, 1:5]

cor_f1<-matriz_cor_muestral[1:3,1:3]         
cor_f2<-matriz_cor_muestral[4:6,4:6]



# Calculamos la media de las correlaciones entre los ítems para el CA

mean_cor_f1<-mean(cor_f1[lower.tri(cor_f1)])
mean_cor_f2<-mean(cor_f2[lower.tri(cor_f2)])



# Para el CR y AVE extraemos las cargas factoriales estandarizadas l_fx de la matriz lambda

l_f1<-lavInspect(fit,"std")$lambda[1:3,1]
l_f2<-lavInspect(fit,"std")$lambda[4:6,2]




# También extraemos las variazas de los errores v_fx de la matriz theta

v_f1<-diag(lavInspect(fit,"std")$theta)[1:3]
v_f2<-diag(lavInspect(fit,"std")$theta)[4:6]



# Calculamos el CA con la fórmula CA = k*rho/((1+(k-1)*rho)) donde rho es
# la media de las correlaciones entre los ítems
# OJO que hay que saber cuantos indicadores k tiene cada factor

ca_f1<-3*mean_cor_f1/(1+(3-1)*mean_cor_f1)
ca_f2<-3*mean_cor_f2/(1+(3-1)*mean_cor_f2)



# Calculamos CR con la fórmula clásica

cr_f1<-sum(l_f1)^2/(sum(l_f1)^2+sum(v_f1)) 
cr_f2<-sum(l_f2)^2/(sum(l_f2)^2+sum(v_f2)) 



# Calculamos AVE con la fórmula clásica

ave_f1<-(sum(l_f1^2))/((sum(l_f1^2))+sum(v_f1))
ave_f2<-(sum(l_f2^2))/((sum(l_f2^2))+sum(v_f2))



# Esta parte es solo para la presentación de los resultados
# y que aparezcan etiquetados en la salida

ca<-c(ca_f1,ca_f2) 
names(ca)=c("ca_conocidos", "ca_donaciones")

cr<-c(cr_f1,cr_f2) 
names(cr)=c("cr_conocidos", "cr_donaciones")

ave<-c(ave_f1,ave_f2)
names(ave)=c("ave_conocidos", "ave_donaciones")

print(ca)
print(cr)
print(ave)

# =======================================
# VALIDEZ DISCRIMINANTE
# =======================================

# El paquete semTools nos permite calcular el ratio htmt

htmt(datos,modelo.cfa)

# =======================================
# PRUEBAS DE NORMALIDAD
# =======================================


mardiaKurtosis(datos, use="pairwise.complete.obs")
mardiaSkew(datos,use="pairwise.complete.obs")

# =======================================
# GRÁFICO CON LAS ESTIMACIONES DEL MODELO
# =======================================

#Generación del gráfico
semPaths(fit,what="std",whatLabels="std", style="lisrel", layout="tree")


