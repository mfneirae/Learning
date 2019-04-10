library(haven)
Datos_7_1_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_7_1_Caso.sav")

#========================
# Carga librerías y datos
#========================

library(ggplot2)

datos<-data.frame(Datos_7_1_Caso)
datos$semana <- factor(datos$semana)
datos$tipsuper <- factor(datos$tipsuper, levels=c("UC", "UR", "R"))


#========================
# gráficos descriptivos
#========================


ggplot(datos, aes(x=tipsuper, y=cola))+
  stat_summary(fun.y=mean,geom="point",aes(group=semana))+
  stat_summary(fun.y = mean, geom = "line",aes(group= semana,linetype=semana)) + 
  labs(x = "Tipo de zona", y = "Ventas de cola")

ggplot(datos, aes(x=tipsuper, y=ron))+
  stat_summary(fun.y=mean,geom="point",aes(group=semana))+
  stat_summary(fun.y = mean, geom = "line",aes(group= semana,linetype=semana)) + 
  labs(x = "Tipo de zona", y = "Ventas de ron")

#========================
# Estimación del modelo
#========================

# Estimación MANOVA
fit<-manova(cbind(cola,ron)~semana*tipsuper,data=datos)
summary.manova(fit,intercept=TRUE,test="Wilks")
summary.manova(fit,intercept=TRUE,test="Wilks")$SS

# Efectos inter-sujetos
fit2<-aov(data=datos,cola~semana+tipsuper+semana*tipsuper)
fit3<-aov(data=datos,ron~semana+tipsuper+semana*tipsuper)
summary(fit2)
summary(fit3)

#========================
# Comprobación supuestos
#========================

# Normalidad
#========================

library(mvnormtest)

#desagregamos la base por grupos por grupos: tipo de tienda
grupoUC<-datos[1:12,3:4]
grupoUR<-datos[13:24,3:4]
grupoR<-datos[25:36,3:4]

#El test necesita las variables en filas, transponemos
grupoUC<-t(grupoUC)
grupoUR<-t(grupoUR)
grupoR<-t(grupoR)

#Ejecutamos el test
mshapiro.test(grupoUC)
mshapiro.test(grupoUR)
mshapiro.test(grupoR)


#desagregamos la base por grupos por grupos: Semana
#ordenamos la base por semana
datos<-datos[order(datos$semana),]

grupoSem1<-datos[1:18,3:4]
grupoSem2<-datos[19:36,3:4]

#El test necesita las variables en filas, transponemos
grupoSem1<-t(grupoSem1)
grupoSem2<-t(grupoSem2)


#Ejecutamos el test
mshapiro.test(grupoSem1)
mshapiro.test(grupoSem2)


#=========================================
# Comprobación de la igualdad covarianzas 
#=========================================

library(biotools)

boxM(datos[3:4],datos[,1])
boxM(datos[3:4],datos[,2])

#=========================================
# Test de esfericidad de Barlett
#=========================================

W = matrix(c(240,143,143,172), nrow=2,ncol=2)

COV.W=W/30 # df=30 para los residuos

R<-cov2cor(COV.W)

cortest.bartlett(R,n=30)
