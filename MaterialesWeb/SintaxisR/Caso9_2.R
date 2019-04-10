library(haven)
Datos_9_2_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_9_2_Caso.sav")

#========================
# Carga librerías y datos
#========================

library(ggplot2)
library(MASS)
library(descr)
library(biotools)
library(mvnormtest)
library(klaR)
library(ggrepel)

datos<-Datos_9_2_Caso


#====================================
# Ejecutar el análisis discriminante
#====================================

fit<-lda(data=datos,categ~ingresos+patrneto+proviv+casado+salfijo)
head(print(fit))
plot(fit)

#====================================
# Calidad de la clasificación
#====================================

# Obtenemos la pertenencia predicha y la añadimos al fichero de datos
fit.p<-predict(fit)$class
datos<-data.frame(datos,fit.p)

# Obtenemos la tabla cruzada
CrossTable(datos[,1],datos$fit.p,digits=2,format="SPSS",prop.c=FALSE,
           prop.chisq =FALSE,prop.t = FALSE,
           dnn=c("Grupo real","Grupo pronosticado"))

#Porcentaje de clasificaciones correctas aer{biotools} da las incorrectas

1-aer(datos$categ, datos$fit.p)

#====================================
# MANOVA para significatividad
#====================================

fit.manova<-manova(data=datos,cbind(ingresos,patrneto,proviv,casado,salfijo)~categ)
summary((fit.manova),test="Wilks")

#============================================
# Cálculo de los coeficientes estandarizados
#============================================

summary((fit.manova),test="Wilks")$SS

#grados libertad residuos = 23

std.b.ingresosLD1=(sqrt(summary(fit.manova)$SS$Residuals[1,1]/23))*fit$scaling[1,1]
std.b.patrnetoLD1=(sqrt(summary(fit.manova)$SS$Residuals[2,2]/23))*fit$scaling[2,1]
std.b.provivLD1=(sqrt(summary(fit.manova)$SS$Residuals[3,3]/23))*fit$scaling[3,1]
std.b.casadoLD1=(sqrt(summary(fit.manova)$SS$Residuals[4,4]/23))*fit$scaling[4,1]
std.b.salfijoLD1=(sqrt(summary(fit.manova)$SS$Residuals[5,5]/23))*fit$scaling[5,1]

std.b.ingresosLD2=(sqrt(summary(fit.manova)$SS$Residuals[1,1]/23))*fit$scaling[1,2]
std.b.patrnetoLD2=(sqrt(summary(fit.manova)$SS$Residuals[2,2]/23))*fit$scaling[2,2]
std.b.provivLD2=(sqrt(summary(fit.manova)$SS$Residuals[3,3]/23))*fit$scaling[3,2]
std.b.casadoLD2=(sqrt(summary(fit.manova)$SS$Residuals[4,4]/23))*fit$scaling[4,2]
std.b.salfijoLD2=(sqrt(summary(fit.manova)$SS$Residuals[5,5]/23))*fit$scaling[5,2]

std.b.LD<-
  matrix(c(std.b.ingresosLD1,std.b.patrnetoLD1,std.b.provivLD1,std.b.casadoLD1,std.b.salfijoLD1,std.b.ingresosLD2,std.b.patrnetoLD2,std.b.provivLD2,std.b.casadoLD2,std.b.salfijoLD2),nrow=5,ncol=2)
rownames(std.b.LD)<-c("ingresos","patrneto","proviv","casado","salfijo")
colnames(std.b.LD)<-c("LD1","LD2")

std.b.LD


#============================================
# Cálculo de la matriz de estructura
#============================================

SCPC.residual<-summary((fit.manova),test="Wilks")$SS$Residuals
SCPC.residual.varianzas<-SCPC.residual/23
SCPC.residual.correlaciones<-cov2cor(SCPC.residual.varianzas)

l.ingresosLD1=SCPC.residual.correlaciones[1,1]*std.b.ingresosLD1+
              SCPC.residual.correlaciones[1,2]*std.b.patrnetoLD1+
              SCPC.residual.correlaciones[1,3]*std.b.provivLD1+
              SCPC.residual.correlaciones[1,4]*std.b.casadoLD1+
              SCPC.residual.correlaciones[1,5]*std.b.salfijoLD1

l.patrnetoLD1=SCPC.residual.correlaciones[2,1]*std.b.ingresosLD1+
              SCPC.residual.correlaciones[2,2]*std.b.patrnetoLD1+
              SCPC.residual.correlaciones[2,3]*std.b.provivLD1+
              SCPC.residual.correlaciones[2,4]*std.b.casadoLD1+
              SCPC.residual.correlaciones[2,5]*std.b.salfijoLD1

l.provivLD1=  SCPC.residual.correlaciones[3,1]*std.b.ingresosLD1+
              SCPC.residual.correlaciones[3,2]*std.b.patrnetoLD1+
              SCPC.residual.correlaciones[3,3]*std.b.provivLD1+
              SCPC.residual.correlaciones[3,4]*std.b.casadoLD1+
              SCPC.residual.correlaciones[3,5]*std.b.salfijoLD1

l.casadoLD1=  SCPC.residual.correlaciones[4,1]*std.b.ingresosLD1+
              SCPC.residual.correlaciones[4,2]*std.b.patrnetoLD1+
              SCPC.residual.correlaciones[4,3]*std.b.provivLD1+
              SCPC.residual.correlaciones[4,4]*std.b.casadoLD1+
              SCPC.residual.correlaciones[4,5]*std.b.salfijoLD1

l.salfijoLD1= SCPC.residual.correlaciones[5,1]*std.b.ingresosLD1+
              SCPC.residual.correlaciones[5,2]*std.b.patrnetoLD1+
              SCPC.residual.correlaciones[5,3]*std.b.provivLD1+
              SCPC.residual.correlaciones[5,4]*std.b.casadoLD1+
              SCPC.residual.correlaciones[5,5]*std.b.salfijoLD1 

l.ingresosLD2=SCPC.residual.correlaciones[1,1]*std.b.ingresosLD2+
  SCPC.residual.correlaciones[1,2]*std.b.patrnetoLD2+
  SCPC.residual.correlaciones[1,3]*std.b.provivLD2+
  SCPC.residual.correlaciones[1,4]*std.b.casadoLD2+
  SCPC.residual.correlaciones[1,5]*std.b.salfijoLD2

l.patrnetoLD2=SCPC.residual.correlaciones[2,1]*std.b.ingresosLD2+
  SCPC.residual.correlaciones[2,2]*std.b.patrnetoLD2+
  SCPC.residual.correlaciones[2,3]*std.b.provivLD2+
  SCPC.residual.correlaciones[2,4]*std.b.casadoLD2+
  SCPC.residual.correlaciones[2,5]*std.b.salfijoLD2

l.provivLD2=  SCPC.residual.correlaciones[3,1]*std.b.ingresosLD2+
  SCPC.residual.correlaciones[3,2]*std.b.patrnetoLD2+
  SCPC.residual.correlaciones[3,3]*std.b.provivLD2+
  SCPC.residual.correlaciones[3,4]*std.b.casadoLD2+
  SCPC.residual.correlaciones[3,5]*std.b.salfijoLD2

l.casadoLD2=  SCPC.residual.correlaciones[4,1]*std.b.ingresosLD2+
  SCPC.residual.correlaciones[4,2]*std.b.patrnetoLD2+
  SCPC.residual.correlaciones[4,3]*std.b.provivLD2+
  SCPC.residual.correlaciones[4,4]*std.b.casadoLD2+
  SCPC.residual.correlaciones[4,5]*std.b.salfijoLD2

l.salfijoLD2= SCPC.residual.correlaciones[5,1]*std.b.ingresosLD2+
  SCPC.residual.correlaciones[5,2]*std.b.patrnetoLD2+
  SCPC.residual.correlaciones[5,3]*std.b.provivLD2+
  SCPC.residual.correlaciones[5,4]*std.b.casadoLD2+
  SCPC.residual.correlaciones[5,5]*std.b.salfijoLD2 


l.LD<-
  matrix(c(l.ingresosLD1,l.patrnetoLD1,l.provivLD1,l.casadoLD1,l.salfijoLD1,
           l.ingresosLD2,l.patrnetoLD2,l.provivLD2,l.casadoLD2,l.salfijoLD2),nrow=5,ncol=2)
rownames(l.LD)<-c("ingresos","patrneto","proviv","casado","salfijo")
colnames(l.LD)<-c("LD1","LD2")

l.LD

#============================================
# Comprobación de la homoscedasticidad
#============================================

boxM(datos[2:6],datos$categ)

#============================================
# Comprobación de la normalidad multivariante
#============================================


#desagregamos la base por grupos 
grupo1<-datos[1:13,2:6] 
grupo2<-datos[14:20,2:6] 
grupo3<-datos[21:25,2:6]

#El test necesita las variables en filas, transponemos
grupo1<-t(grupo1) 
grupo2<-t(grupo2) 
grupo3<-t(grupo3)

#Ejecutamos el test
mshapiro.test(grupo1)
mshapiro.test(grupo2)
mshapiro.test(grupo3)

#============================================
# grafico de separación
#============================================
grupo<-c(rep("1",13),rep("2",7),rep("3",5))
etiqueta<-c(rep("Cumplidor",13),rep("Moroso",7),rep("Fallido",5))
individuo<-c(rep(1:25))

datos.grafico<-data.frame(datos,grupo,etiqueta,individuo,predict(fit)$x[,1],predict(fit)$x[,2])
library(plyr)
datos.grafico<-rename(datos.grafico, c("predict.fit..x...1."="LD1", "predict.fit..x...2."="LD2"))



ggplot(datos.grafico, aes(x=LD1, y=LD2,colour=etiqueta,label=individuo)) +
  geom_point(size=2)+
  scale_color_manual(values=c("black","red","blue"))+
  geom_text_repel()+
  expand_limits(x=c(-4,5), y=c(-2, 3))+
  labs (x="LD1", y="LD2")+
  guides(colour="legend",label=FALSE)+
  theme(legend.title=element_blank())

colores<-c("gray","white","gray")
partimat(data=datos.grafico,grupo~LD2+LD1,prec=1000,col.mean="darkred", image.colors =colores, display.points=TRUE)

plot(fit,dimen=1)

