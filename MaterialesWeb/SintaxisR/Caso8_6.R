#=============================
# Librerias
#=============================

library(haven)
library(stargazer)
library(ggplot2)


#=============================
# Datos
#=============================

Datos_8_6_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_8_6_Caso.sav")
datos<-Datos_8_6_Caso


#=============================
# Modelo 1: ficticia 2 niveles
#=============================

fit1<-lm(log(salario)~mujer+educ,data=datos)
stargazer(fit1,type="text",report = "vct*")


#=============================
# Modelo 2: ficticia 3 niveles
#=============================

fit2<-lm(log(salario)~mediana+grande+educ,data=datos)
fit3<-lm(log(salario)~educ,data=datos)
stargazer(fit2,fit3,type="text",report = "vct*")
anova(fit2)
anova(fit3)

F<-((433.12-406.19)/2)/(406.19/(2000-1-3))
F
qf(0.99,2,2000-1-3)


#=============================
# Modelo 3: interacción
#=============================

fit4<-lm(log(salario)~educ+mujer:educ,data=datos)
stargazer(fit4,type="text",report = "vct*")

#===============================
# Modelo 4: Cambio estructural
#===============================

fit5<-lm(log(salario)~educ+mujer+mujer:educ,data=datos)
fit6<-lm(log(salario)~educ,data=datos)
stargazer(fit5,fit6,type="text")
anova(fit5)
anova(fit6)

F<-((433.12-393.00)/2)/(393.00/(2000-1-3))
F
qf(0.99,2,2000-1-3)


#===============================
# Figura 8.11
#===============================
x<-c(0,7.5)
y<-c(0,10)
datos.grafico<-data.frame(cbind(x,y))
ggplot(datos.grafico,aes(x,y))+
  geom_segment(x=0,xend=6,y=1,yend=5.8)+
  geom_segment(x=0,xend=6,y=5,yend=9.8)+
  geom_blank()+
  labs(x="educ",y="salario")+
  xlim(0, 6)+
  theme(legend.position="none")

#===============================
# Figura 8.12
#===============================
x<-c(0,7.5)
y<-c(0,10)
datos.grafico<-data.frame(cbind(x,y))
ggplot(datos.grafico,aes(x,y))+
  geom_segment(x=0,xend=6,y=2,yend=5.8)+
  geom_segment(x=0,xend=6,y=2,yend=9.8)+
  geom_blank()+
  labs(x="educ",y="salario")+
  xlim(0, 6)+
  theme(legend.position="none")

#===============================
# Figura 8.13
#===============================
x<-c(0,7.5)
y<-c(0,10)
datos.grafico<-data.frame(cbind(x,y))
ggplot(datos.grafico,aes(x,y))+
  geom_segment(x=0,xend=6,y=2,yend=5.8)+
  geom_segment(x=0,xend=6,y=4,yend=12.8)+
  geom_blank()+
  labs(x="educ",y="salario")+
  xlim(0, 6)+
  theme(legend.position="none")
