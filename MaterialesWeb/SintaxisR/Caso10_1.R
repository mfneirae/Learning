

#============================
# Cargar librerias  
#============================

library(ggplot2)
library(LOGIT)
library(BaylorEdPsych)
library(ResourceSelection)
library(gmodels)
library(LogisticDx)
library(Epi)
library(vcdExtra)
library(mfx)

#============================
# Datos 
#============================

library(haven)
Datos_10_1_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_10_1_Caso.sav")
datos<-Datos_10_1_Caso[,1:4]
datos <- na.omit(datos) #eliminamos casos con datos perdidos



#============================
# Descriptivos 
#============================
CrossTable(datos$pclass,datos$survived,chisq=TRUE,
           prop.chisq = FALSE, prop.c = FALSE, prop.t=FALSE)
CrossTable(datos$sex,datos$survived,chisq=TRUE,
           prop.chisq = FALSE, prop.c = FALSE, prop.t=FALSE)
t.test(datos$age~datos$survived)


#============================
# Regresion logística
#============================

# Transformamos en categórica la clase del pasaje
datos$pclass.f <- factor(datos$pclass)

# Estimamos la regresión logística
fit<-glm(data=datos,survived~pclass.f+age+sex,family=binomial)


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

#================================================================
# Contraste para la edad para ilustrar el método del ratio de MV
#================================================================

fit2<-glm(data=datos,survived~pclass.f+sex,family=binomial)
summary(fit2)

#==================================
# Odd ratios
#=================================


toOR(fit)

#==================================
# Efectos marginales
#=================================

logitmfx(formula = fit, data = datos)

#==================================
# Indicadores de ajuste
#=================================


#Matriz de confusión
predict.fit<-fit$fitted.values
predict.fit[predict.fit>=.50]<-1
predict.fit[predict.fit<.50]<-0
CrossTable(datos$survived,predict.fit,prop.chisq=FALSE,prop.c=FALSE,prop.r=FALSE)

# Calculos automatizados de los indicadores de ajuste

PseudoR2(fit)
confusion_stat(predict.fit, datos$survived)

#==================================
#  Discriminación y calibración
#=================================



# Test de Homer-Lemeshow

library(vcdExtra)
HL<-HLtest(fit,g=10)
HL
HL$table


# Curva ROC

library(Epi)
ROC(data=datos,form=survived~pclass.f+age+sex)



#==================================
#  Figura 10.2
#==================================

FPRa<-c(0,0,1)
TPRa<-c(0,1,1)
datos.a<-data.frame(FPRa,TPRa)

ggplot(datos.a, aes(x=FPRa, y=TPRa))+
  geom_line(aes(x=FPRa, y=TPRa),colour="Black",size=1)+
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype="dashed")+
  labs (x="FPR-Falsos positivos", y="TPR-Positivos acertados")+
  theme(aspect.ratio=1)+ 
  ggtitle("(a) Clasificador perfecto") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",size=11))


TPRb<-c(0,0.25,0.50,0.75,1.0)
FPRb<-c(0,0.25,0.50,0.75,1.0)
datos.b<-data.frame(FPRb,TPRb)

ggplot(datos.b, aes(x=FPRb, y=TPRb))+
 geom_line(colour="Black",size=1)+
 labs (x="FPR-Falsos positivos", y="TPR-Positivos acertados")+
  theme(aspect.ratio=1)+ 
  ggtitle("(b) Clasificador aleatorio") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",size=11))


FPRc<-c(0,0.25,0.50,0.75,1)
TPRc<-c(0,0.10,0.25,0.50,1)
datos.c<-data.frame(FPRc,TPRc)

ggplot(datos.c, aes(x=FPRc, y=TPRc))+
  geom_line(aes(x=FPRc, y=TPRc),colour="Black",size=1)+
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype="dashed")+
  labs (x="FPR-Falsos positivos", y="TPR-Positivos acertados")+
  theme(aspect.ratio=1)+ 
  ggtitle("(c) Clasificador peor que aleatorio") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",size=11))


FPRd<-c(0,0.125,0.30,0.50,1)
TPRd<-c(0,0.60,0.80,0.90,1)
datos.d<-data.frame(FPRd,TPRd)

ggplot(datos.d, aes(x=FPRd, y=TPRd))+
  geom_line(aes(x=FPRd, y=TPRd),colour="Black",size=1)+
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), linetype="dashed")+
  labs (x="FPR-Falsos positivos", y="TPR-Positivos acertados")+
  theme(aspect.ratio=1)+ 
  ggtitle("(d) Buen clasificador") + 
  theme(plot.title = element_text(lineheight=.8, face="bold",size=11))


