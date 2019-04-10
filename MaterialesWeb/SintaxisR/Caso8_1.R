#==================================
# librerias
#==================================

library(haven)
library(MASS)

#==================================
# Datos
#==================================


Datos_8_1_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_8_1_Caso.sav")
datos<-Datos_8_1_Caso

#==================================
# Estimacion modelo
#==================================

fit<-lm(data=datos,y~x)

#==================================
# Figura 8.1
#==================================


yhat<-fit$fitted.values
ggplot(data=datos,aes(x=x, y=y))+
  geom_smooth(method=lm,se=FALSE,color="black")+
  geom_point()+
  geom_segment(aes(x=x, xend=x, y=y, yend=yhat),linetype="dashed",colour="darkred")+
  labs(title="(a) Regresión lineal simple - Función ajustada",y="Consumo",x="Renta")+
  theme(legend.position="none")

yhat2<-3+0.2*x
ggplot(data=datos,aes(x=x, y=y))+
  geom_abline(intercept = 3,slope=0.2,color="black",size=1)+
  geom_point()+
  geom_segment(aes(x=x, xend=x, y=y, yend=yhat2),linetype="dashed",colour="darkred")+
  labs(title="(b) Función alternativa y=3+0.2x",y="Consumo",x="Renta")+
  theme(legend.position="none")


library(MASS)
y=c(1.5,2.5,2.5,3.9,6.1,7.1,6.1,7.5,9.5,10.2)
x=matrix(c(1,1,1,1,1,1,1,1,1,1,1,2,3,4,5,6,7,8,9,10),10,2)

beta<-ginv(t(x)%*%x)%*%t(x)%*%y
beta


#==================================
# Figura 8.2
#==================================


yhat<-fit$fitted.values
ggplot(data=datos,aes(x=x, y=y))+
  geom_smooth(method=lm,se=FALSE,color="black")+
  geom_abline(intercept = 5.69,slope=0,color="darkgray",size=1)+
  geom_point()+
  geom_segment(aes(x=x, xend=x, y=y, yend=yhat),linetype="dashed",colour="darkred")+
  labs(title="(b) Suma de cuadrados residuales",y="Ventas",x="Publicidad")+
  theme(legend.position="none")

yhat3<-5.69
ggplot(data=datos,aes(x=x, y=y))+
  geom_abline(intercept = 5.69,slope=0,color="darkgray",size=1)+
  geom_point()+
  geom_segment(aes(x=x, xend=x, y=y, yend=yhat3),linetype="dashed",colour="darkred")+
  labs(title="(a) Suma de cuadrados totales",y="Ventas",x="Publicidad")+
  theme(legend.position="none")

ggplot(data=datos,aes(x=x, y=y))+
  geom_smooth(method=lm,se=FALSE,color="black")+
  geom_abline(intercept = 5.69,slope=0,color="darkgray",size=1)+
  geom_point()+
  geom_segment(aes(x=x, xend=x, y=yhat, yend=yhat3),linetype="dashed",colour="darkred")+
  labs(title="(c) Suma de cuadrados explicada por el modelo",y="Ventas",x="Publicidad")+
  theme(legend.position="none")



