library(haven)
Ilustracion7_1 <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Ilustracion7_1.sav")

datos<-data.frame(Ilustracion7_1)
datos$grupo <- factor(datos$grupo)

#===========================
# Estimamos el MANOVA
#===========================

fit<-manova(cbind(y1,y2)~grupo,data=datos)

#===========================
# Pedimos matrices W y F
#===========================

summary(fit)$SS

#=============================================
# Aplicamos el proceso general de Field (2005)
#=============================================

# definimos las matrices que hemos sacado en el paso anterior

F = matrix(c(61.86667, 24.40, 24.40, 19.05), nrow=2,ncol=2)
W = matrix(c(14.8,1.6,1.6,9.2), nrow=2,ncol=2)  

# Invertimos la matriz de residuos Inv.W
Inv.W<-solve(W)

# Multiplicamos B(Inv.W)

F.InvW=F%*%(Inv.W)

# Obtenemos autovalores y autovectores de FW^-1

vec_y<-eigen(F.InvW)$vec
val_y<-eigen(F.InvW)$val

# con esto podemos calcular todos los estadísticos a partir de los autovalroes:

lambdaW=(1/(1+val_y[1:1])*(1/(1+val_y[2:2])))
Pillai=(val_y[1:1]/(1+val_y[1:1]))+(val_y[2:2]/(1+val_y[2:2]))
HotelingT=val_y[1:1]+val_y[2:2]
Roy=max(val_y[1:1],val_y[2:2])


#=====================================
# Pedimos todos los estadísticos 
#=====================================

summary(fit, test="Wilks")
summary(fit, test="Pillai")
summary(fit, test="Hotelling")
summary(fit, test="Roy")

#=====================================
# Comprobación de la normalidad 
#=====================================

library(mvnormtest)

#desagregamos la base por grupos
grupo1<-datos[1:4,2:3]
grupo2<-datos[5:7,2:3]
grupo3<-datos[8:12,2:3]

#El test necesita las variables en filas, transponemos
grupo1<-t(grupo1)
grupo2<-t(grupo2)
grupo3<-t(grupo3)

#Ejecutamos el test
mshapiro.test(grupo1)
mshapiro.test(grupo2)
mshapiro.test(grupo3)

#=========================================
# Comprobación de la igualdad covarianzas 
#=========================================

library(biotools)

boxM(datos[2:3],datos[,1])


#=========================================
# Test de esfericidad de Barlett
#=========================================

W = matrix(c(14.8,1.6,1.6,9.2), nrow=2,ncol=2)

COV.W=W/(3*(4-1))

R<-cov2cor(COV.W)

cortest.bartlett(R,n=9)

#=========================================
# Estadístico de ajuste eta^2
#=========================================

# Retomamos las matrices

F = matrix(c(61.86667, 24.40, 24.40, 19.05), nrow=2,ncol=2)
W = matrix(c(14.8,1.6,1.6,9.2), nrow=2,ncol=2)

T=F+W

# Calculamos determinantes:

detW=det(W)
detT=det(T)

eta2=1-detW/detT

#=========================================
# Analisis post hoc
#=========================================

# Generamos datos con pares de factores
datos12<-datos[ which(datos$grupo==1 | datos$grupo==2), ]
datos13<-datos[ which(datos$grupo==1 | datos$grupo==3), ]
datos23<-datos[ which(datos$grupo==2 | datos$grupo==3), ]

# Efectuamos manova en pares de grupos
fit12<-manova(cbind(y1,y2)~grupo,data=datos12)
fit13<-manova(cbind(y1,y2)~grupo,data=datos13)
fit23<-manova(cbind(y1,y2)~grupo,data=datos23)

summary.manova(fit12)
summary.manova(fit13)
summary.manova(fit23)

# Pruebas t

t.test(y1~grupo,data=datos12)
t.test(y2~grupo,data=datos12)

t.test(y1~grupo,data=datos13)
t.test(y2~grupo,data=datos13)

t.test(y1~grupo,data=datos23)
t.test(y2~grupo,data=datos23)

