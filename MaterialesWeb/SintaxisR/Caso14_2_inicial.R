
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
# PLANTEAMIENTO DEL MODELO CFA PARA VALIDACIÓN
# ============================================

modelo.cfa <- '

# Modelo de medida

actitud          =~ att1+att2+att3+att4
entretenimiento  =~ ent1+ent2
utilidad         =~ usf1+usf2+usf3
irritacion       =~ irr1+irr2+irr3+irr4
aceptacion       =~ acc1+acc2+acc3


#Varianzas de los factores

actitud~~actitud
entretenimiento~~entretenimiento
utilidad~~utilidad
irritacion~~irritacion
aceptacion~~aceptacion

#Covarianzas

actitud~~entretenimiento
actitud~~utilidad
actitud~~irritacion
actitud~~aceptacion
entretenimiento~~utilidad
entretenimiento~~irritacion
entretenimiento~~aceptacion
utilidad~~irritacion
utilidad~~aceptacion
irritacion~~aceptacion

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

# Pedimos el reliability de semTools pero está calculado de otra forma
# y no nos dará los resultados que publicamos habitualmente

semTools::reliability(fit)

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

cor_f1<-matriz_cor_muestral[1:4,1:4]         
cor_f2<-matriz_cor_muestral[5:6,5:6]
cor_f3<-matriz_cor_muestral[7:9,7:9]
cor_f4<-matriz_cor_muestral[10:13,10:13]
cor_f5<-matriz_cor_muestral[14:16,14:16]


# Calculamos la media de las correlaciones entre los ítems para el CA

mean_cor_f1<-mean(cor_f1[lower.tri(cor_f1)])
mean_cor_f2<-mean(cor_f2[lower.tri(cor_f2)])
mean_cor_f3<-mean(cor_f3[lower.tri(cor_f3)])
mean_cor_f4<-mean(cor_f4[lower.tri(cor_f4)])
mean_cor_f5<-mean(cor_f5[lower.tri(cor_f5)])


# Para el CR y AVE extraemos las cargas factoriales estandarizadas l_fx de la matriz lambda

l_f1<-lavInspect(fit,"std")$lambda[1:4,1]
l_f2<-lavInspect(fit,"std")$lambda[5:6,2]
l_f3<-lavInspect(fit,"std")$lambda[7:9,3]
l_f4<-lavInspect(fit,"std")$lambda[10:13,4]
l_f5<-lavInspect(fit,"std")$lambda[14:16,5]



# También extraemos las variazas de los errores v_fx de la matriz theta

v_f1<-diag(lavInspect(fit,"std")$theta)[1:4]
v_f2<-diag(lavInspect(fit,"std")$theta)[5:6]
v_f3<-diag(lavInspect(fit,"std")$theta)[7:9]
v_f4<-diag(lavInspect(fit,"std")$theta)[10:13]
v_f5<-diag(lavInspect(fit,"std")$theta)[14:16]



# Calculamos el CA con la fórmula CA = k*rho/((1+(k-1)*rho)) donde rho es
# la media de las correlaciones entre los ítems
# OJO que hay que saber cuantos indicadores k tiene cada factor

ca_f1<-4*mean_cor_f1/(1+(4-1)*mean_cor_f1)
ca_f2<-2*mean_cor_f2/(1+(2-1)*mean_cor_f2)
ca_f3<-3*mean_cor_f3/(1+(3-1)*mean_cor_f3)
ca_f4<-4*mean_cor_f4/(1+(4-1)*mean_cor_f4)
ca_f5<-3*mean_cor_f5/(1+(3-1)*mean_cor_f5)


# Calculamos CR con la fórmula clásica

cr_f1<-sum(l_f1)^2/(sum(l_f1)^2+sum(v_f1)) 
cr_f2<-sum(l_f2)^2/(sum(l_f2)^2+sum(v_f2)) 
cr_f3<-sum(l_f3)^2/(sum(l_f3)^2+sum(v_f3))
cr_f4<-sum(l_f4)^2/(sum(l_f4)^2+sum(v_f4))
cr_f5<-sum(l_f5)^2/(sum(l_f5)^2+sum(v_f5))


# Calculamos AVE con la fórmula clásica

ave_f1<-(sum(l_f1^2))/((sum(l_f1^2))+sum(v_f1))
ave_f2<-(sum(l_f2^2))/((sum(l_f2^2))+sum(v_f2))
ave_f3<-(sum(l_f3^2))/((sum(l_f3^2))+sum(v_f3))
ave_f4<-(sum(l_f4^2))/((sum(l_f4^2))+sum(v_f4))
ave_f5<-(sum(l_f5^2))/((sum(l_f5^2))+sum(v_f5))
                                 

# Esta parte es solo para la presentación de los resultados
# y que aparezcan etiquetados en la salida

ca<-c(ca_f1,ca_f2,ca_f3,ca_f4,ca_f5) 
names(ca)=c("ca_att", "ca_ent", "ca_usf", "ca_irr", "ca_acc")

cr<-c(cr_f1,cr_f2,cr_f3,cr_f4,cr_f5) 
names(cr)=c("cr_att", "cr_ent", "cr_usf", "cr_irr", "cr_acc")

ave<-c(ave_f1,ave_f2,ave_f3,ave_f4,ave_f5)
names(ave)=c("ave_att", "ave_ent", "ave_usf", "ave_irr", "ave_acc")

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

