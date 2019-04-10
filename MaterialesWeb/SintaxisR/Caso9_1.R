library(haven)
Datos_9_1_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_9_1_Caso.sav")

#========================
# Carga librerías y datos
#========================

library(ggplot2)
library(MASS)
library(descr)
library(biotools)
library(mvnormtest)

datos<-Datos_9_1_Caso


#====================================
# Visualizar datos-grafico
#====================================

etiqueta<-c("Fallido","Fallido","Fallido","Fallido","Fallido","Fallido","Fallido","Fallido",
            "No Fallido","No Fallido","No Fallido","No Fallido","No Fallido","No Fallido",
            "No Fallido","No Fallido")
caso<-c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16")

datos.grafico<-data.frame(datos$patrneto,datos$deudapen,etiqueta,caso)

# Original

ggplot(datos.grafico, aes(x=datos.deudapen, y=datos.patrneto,colour=etiqueta,label=caso)) +
  geom_point(size=2)+
  scale_color_manual(values=c("black","red"))+
  geom_text(check_overlap = F,hjust=0.5, vjust = -0.7, nudge_y = 0.0, nudge_x = 0.0)+
  expand_limits(x=c(0,8), y=c(0, 12.5))+
  labs (x="Deuda pendiente", y="Patrimonio neto")+
  guides(colour="legend",label="FALSE")

# con la función discriminante

ggplot(datos.grafico, aes(x=datos.deudapen, y=datos.patrneto,colour=etiqueta,label=caso)) +
  geom_point(size=2)+
  scale_color_manual(values=c("black","red"))+
  geom_text(check_overlap = F,hjust=0.5, vjust = -0.7, nudge_y = 0.0, nudge_x = 0.0)+
  expand_limits(x=c(0,8), y=c(0, 12.5))+
  labs (x="Deuda pendiente", y="Patrimonio neto")+
  guides(colour="legend",label="FALSE")+
  geom_abline(intercept = 3.400, slope = 0.900)


#====================================
# Ejecutar el análisis discriminante
#====================================

fit<-lda(data=datos,fallido~patrneto+deudapen)

# Obtenemos la pertenencia predicha y la añadimos al fichero de datos
fit.p<-predict(fit)$class
datos<-data.frame(datos,fit.p)

# Obtenemos la tabla cruzada
CrossTable(datos[,1],datos$fit.p,digits=2,format="SPSS",prop.c=FALSE,
prop.chisq =FALSE,prop.t = FALSE,
dnn=c("Grupo real","Grupo pronosticado"))

#====================================
# Significatividad -- MANOVA
#====================================

fit.manova<-manova(data=datos,cbind(deudapen,patrneto)~fallido)
summary((fit.manova),test="Wilks")

#============================================
# Cálculo de los coeficientes estandarizados
#============================================

summary((fit.manova),test="Wilks")$SS

std.b.deudapen=(sqrt(summary(fit.manova)$SS$Residuals[1,1]/14))*fit$scaling[2,1]
std.b.patrneto=(sqrt(summary(fit.manova)$SS$Residuals[2,2]/14))*fit$scaling[1,1]

#============================================
# Cálculo de la matriz de estructura
#============================================

SCPC.residual<-summary((fit.manova),test="Wilks")$SS$Residuals
SCPC.residual.varianzas<-SCPC.residual/14
SCPC.residual.correlaciones<-cov2cor(SCPC.residual.varianzas)

l.deudapen=SCPC.residual.correlaciones[1,1]*std.b.deudapen+SCPC.residual.correlaciones[1,2]*std.b.patrneto

l.patrneto=SCPC.residual.correlaciones[1,2]*std.b.deudapen+SCPC.residual.correlaciones[2,2]*std.b.patrneto

#============================================
# Comprobación de la homoscedasticidad
#============================================

boxM(datos[2:3],datos[,1])

#============================================
# Comprobación de la normalidad multivariante
#============================================


#desagregamos la base por grupos 
grupo1<-datos[1:8,2:3] 
grupo2<-datos[9:16,2:3] 

#El test necesita las variables en filas, transponemos
grupo1<-t(grupo1) 
grupo2<-t(grupo2) 

#Ejecutamos el test
mshapiro.test(grupo1)
mshapiro.test(grupo2)


#============================================
# Predicción de nuevos casos
#============================================

d<-c(10.1,9.7)
p<-c(6.8,2.2)
nuevos.casos<-data.frame(d,p)
colnames(nuevos.casos)<-c("patrneto","deudapen")
predict(fit,newdata=nuevos.casos)$class
predict(fit,newdata=nuevos.casos)$posterior

#============================================
# Cálculo de probabilidades a posteriori
#============================================

#Sin información a priori

predict(fit)$posterior

# Con información a priori fallidos=10% no fallidos=90%

predict(fit,prior=c(0.1,0.9))$posterior



