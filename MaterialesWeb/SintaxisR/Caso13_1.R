
# =======================================
# CARGA DE LOS DATOS
# =======================================

library (lavaan)
library(semTools)
library(semPlot)
library(ggplot2)

#Conversión del vector de correlaciones en una matriz que llamamos datos.cor
#Definimos el vector que llamamos x

x <- c(1.000,
       .493,1.000,
       .401,.314,1.000,
       .278,.347,.147,1.000,
       .317,.318,.183,.587,1.000,
       .284,.327,.179,.463,.453,1.000)

#Convertimos el vector x en la matriz datos.cor
datos.cor<-lav_matrix_lower2full(x)

#Etiquetamos a las variables de la matriz

colnames(datos.cor) <- rownames(datos.cor) <- 
  c("L","FSF", "H","M","FSC","Q")

#Pedimos la visualización de la matriz datos.cor
View(datos.cor)

#Introducimos las desviaciones típicas SD
datos.sd <- c(1.090, 0.590, 0.980, 1.100, 0.410, 1.110)

names(datos.sd) <- 
  c("L","FSF", "H","M","FSC","Q")

#Pedimos la visualización de la matriz datos.cor
View(datos.sd)

#Convertimos las correlaciones y desviaciones típicas en varianzas y covarianzas
datos.cov<-cor2cov(datos.cor,datos.sd)

#Pedimos la visualización de la matriz datos.cor
View(datos.cov)


# ============================================
# PLANTEAMIENTO DEL MODELO CFA PARA VALIDACIÓN
# ============================================

modelo.cfa <- '

# Modelo de medida

IV  =~ L+FSF+H
IQ  =~ M+FSC+Q


#Varianzas de los factores

IV~~1*IV
IQ~~1*IQ

#Covarianzas

IV~~IQ

#Varianzas de los términos de error

L~~L
FSF~~FSF
H~~H
M~~M
FSC~~FSC
Q~~Q
'

#Estimación del modelo

fit <- lavaan(modelo.cfa, sample.cov=datos.cov, sample.nobs=275, std.lv=TRUE, mimic="eqs", estimator="ML", verbose=TRUE, warn=TRUE) 

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
# GRÁFICO CON LAS ESTIMACIONES DEL MODELO
# =======================================

#Generación del gráfico
semPaths(fit,what="std",whatLabels="std", style="lisrel", layout="tree")

