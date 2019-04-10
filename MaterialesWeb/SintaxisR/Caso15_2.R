
# =======================================
# CARGA DE LOS DATOS
# =======================================

library(haven)
Datos_13_2_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_13_2_Caso.sav")
View(Datos_13_2_Caso)

datos<-Datos_13_2_Caso

library (lavaan)
library(semTools)
library(semPlot)
library(ggplot2)

# ============================================
# MODELO ESTRUCTURAL
# ============================================

modelo.cfa <- '

# Modelo de medida

actitud          =~ 1*att1+att2+att3+att4
entretenimiento  =~ 1*ent1+ent2
utilidad         =~ 1*usf1+usf2+usf3
irritacion       =~ 1*irr1+irr2+irr3+irr4
aceptacion       =~ 1*acc1+acc2+acc3

# Relaciones estructurales

irritacion   ~entretenimiento+utilidad
actitud      ~entretenimiento+irritacion+utilidad
aceptacion   ~actitud

#Varianzas de los factores

entretenimiento~~entretenimiento 
utilidad~~utilidad

# actitud~~actitud
# irritacion~~irritacion
# aceptacion~~aceptacion


#Covarianzas

#actitud~~entretenimiento
#actitud~~utilidad
#actitud~~irritacion
#actitud~~aceptacion

entretenimiento~~utilidad

#entretenimiento~~irritacion
#entretenimiento~~aceptacion
#utilidad~~irritacion
#utilidad~~aceptacion
#irritacion~~aceptacion


#Varianzas de los términos de error

att1~~att1
att2~~att2
att3~~att3
att4~~att4
ent1~~ent1
ent2~~ent2
usf1~~usf1
usf2~~usf2
usf3~~usf3
irr1~~irr1
irr2~~irr2
irr3~~irr3
irr4~~irr4
acc1~~acc1
acc2~~acc2
acc3~~acc3

#errores factores dependientes

actitud~~actitud
irritacion~~irritacion
aceptacion~~aceptacion

'

#Estimación del modelo

fit <- lavaan(modelo.cfa, data=datos, std.lv=FALSE, mimic="eqs", estimator="ML", verbose=TRUE, warn=TRUE) 

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
# PRUEBAS DE NORMALIDAD
# =======================================


mardiaKurtosis(datos, use="pairwise.complete.obs")
mardiaSkew(datos,use="pairwise.complete.obs")

# =======================================
# GRÁFICO CON LAS ESTIMACIONES DEL MODELO
# =======================================

#Generación del gráfico
semPaths(fit,what="std",whatLabels="std", style="lisrel", layout="tree")

