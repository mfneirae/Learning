

#============================
# Cargar librerias  
#============================

library(ggplot2)
library(LOGIT)
library(BaylorEdPsych)
library(ResourceSelection)
library(gmodels)

#============================
# Datos simulados
#============================

y<-c(0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1)
x<-c(1,2,3,4,5,6,7,8,9,10,8,9,10,11,12,13,14,15,16,17)
datos<-data.frame(x,y)

#============================
# Regresion lineal
#============================


r<-lm(y~x)




yhat<-r$fitted.values
ggplot(data=datos,aes(x=x, y=y))+
  geom_smooth(method=lm,se=FALSE,color="black")+
  geom_point()+
  geom_segment(aes(x=x, xend=x, y=y, yend=yhat),linetype="dashed",colour="darkred")+
  labs(title="Errores de la regresión lineal")+
  theme(legend.position="none")

#============================
# Regresion logística
#============================

fit<-glm(y~x,family=binomial)

yhat2<-fit$fitted.values
ggplot(data=datos,aes(x=x, y=y))+
  stat_smooth(method="glm", method.args=list(family="binomial"), colour="black",se=FALSE)+
  geom_point()+
  geom_segment(aes(x=x, xend=x, y=y, yend=yhat2),linetype="dashed",colour="darkred")+
  labs(title="Errores de la regresión logística")+
  expand_limits(y=c(-0.2, 1.2))+
  theme(legend.position="none")


#==================================
# Contraste significatividad global
#=================================


deviance.model<-fit$deviance
deviance.base<-fit$null.deviance
chi<-deviance.base-deviance.model
chi.df<-fit$df.null-fit$df.residual
sig.chi<-1-pchisq(chi,df=chi.df)

deviance.model
deviance.base
chi
chi.df
sig.chi

#==================================
# Odd ratios
#=================================


toOR(fit)


#==================================
# Indicadores de ajuste
#=================================

# McFadden R2
MFR2<-(deviance.base-deviance.model)/deviance.base

# Cox & Snell R2
CSR2<-1-exp((1/20)*((deviance.model)-(deviance.base)))

# Nagelkerke R2
NR2<-CSR2/(1-exp(-deviance.base/20))

#Matriz de confusión
predict.fit<-fit$fitted.values
predict.fit[predict.fit>=.50]<-1
predict.fit[predict.fit<.50]<-0
CrossTable(datos$y,predict.fit,prop.chisq=FALSE,prop.c=FALSE,prop.r=FALSE)


# Calculos automatizados de los indicadores de ajuste

PseudoR2(fit)
confusion_stat(predict.fit, datos$y)

