#Leemos el fichero de datos del caso 2.1
library(RcmdrMisc)
library(haven)
Datos_2_1_Caso <- read_sav("Datos_2_1_Caso.sav")
View(Datos_2_1_Caso)

#Cuadro 2.3 ==========================================================================

#Calculamos las medias de v4a y v4b

summary(Datos_2_1_Caso)

#Cuadro 2.43 ==========================================================================

#Calculamos la significatividad de las diferencias de medias (Cuadro 2.4)
t.test(v2~v4a_d, alternative='two.sided', conf.level=.95, var.equal=TRUE, data=Datos_2_1_Caso)
t.test(v2~v4b_d, alternative='two.sided', conf.level=.95, var.equal=TRUE, data=Datos_2_1_Caso)

#Cuadro 2.5 ==========================================================================

#Calculamos la matriz de correlaciones (cuadro 2.5)
rcorr.adjust(Datos_2_1_Caso[,c("v2_d","v4a_d","v4b_d")], type="pearson", use="complete")

#Cuadro 2.6 ==========================================================================

#Calculamos las medias eliminando casos con valores perdidos (cuadro 2.6)

####Construimos una base de datos nueva que llamamos datos_listwise sin v4b

library(ForImp)
datos_listwise <- within(Datos_2_1_Caso,{v4b <- NULL})
datos_listwise <- ld(datos_listwise)
numSummary(datos_listwise[,c("c1", "v1", "v2", "v3", "v4a", "v5")], 
statistics=c("mean", "sd"))

#Cuadro 2.7 ==========================================================================

#Calculo de las medias por pareja (cuadro 2.7)
###Construimos dos bases de datos, una donde solo dejamos los casos donde v2 no tiene perdidos
###y otra donde v4a no los tiene, el resto de variables no tiene perdidos

datos_listwise_v2<-subset(Datos_2_1_Caso,!(is.na(Datos_2_1_Caso["v2"])))
datos_listwise_v4a<-subset(Datos_2_1_Caso,!(is.na(Datos_2_1_Caso["v4a"])))


numSummary(datos_listwise_v2[,c("v4a","v1","v2","v3","v5","c1")], statistics=c("mean"))
numSummary(datos_listwise_v4a[,c("v4a","v1","v2","v3","v5","c1")], statistics=c("mean"))

#para el resto de filas del cuadro 2.7

numSummary(Datos_2_1_Caso[,c("v4a","v1","v2","v3","v5","c1")], statistics=c("mean"))

#Cuadro 2.8 ================================================================s==========

#Realizamos la regresion del cuadro 2.8

cuadro2.8<-lm(v4a~v1+v3+v5+c1, data=Datos_2_1_Caso)
summary(cuadro2.8)

#Cuadro 2.20 ==========================================================================

library(haven)
Datos_2_1b_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_2_1b_Caso.sav")
View(Datos_2_1b_Caso)


library(car)
leveneTest(Datos_2_1b_Caso$v1,Datos_2_1b_Caso$c3,center=mean)
leveneTest(Datos_2_1b_Caso$v2,Datos_2_1b_Caso$c3,center=mean)
leveneTest(Datos_2_1b_Caso$v3,Datos_2_1b_Caso$c3,center=mean)
leveneTest(Datos_2_1b_Caso$v4,Datos_2_1b_Caso$c3,center=mean)
leveneTest(Datos_2_1b_Caso$v5,Datos_2_1b_Caso$c3,center=mean)



#Cuadro 2.20 ==========================================================================
#Obtenemos las matrices de covarianzas para el grupo de fumadores (1) y no fumadores (2)
library(stats)
by(Datos_2_1b_Caso[,c("v1","v2","v3","v4","v5")],Datos_2_1b_Caso$c3,cov)

#Cuadro 2.21 ==========================================================================
#Calculamos la M de Box
library(biotools)
boxM(Datos_2_1b_Caso[,c("v1","v2","v3","v4","v5")],Datos_2_1b_Caso$c3)


