library(haven)
Datos_4_3a_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_4_3_aCaso.sav")
Datos_4_3b_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_4_3_bCaso.sav")

datos_D1<-data.matrix(Datos_4_3a_Caso)
datos_D2<-data.matrix(Datos_4_3b_Caso)

rownames(datos_D1)<-colnames(datos_D1)
rownames(datos_D2)<-colnames(datos_D2)

datos<-list(datos_D1,datos_D2)


fit <- smacofIndDiff(delta=datos,type="ordinal",constraint = "indscal")
plot(fit, plot.type = "confplot")
plot(fit, plot.type = "bubbleplot")
plot(fit, plot.type = "stressplot")
plot(fit, plot.type = "Shepard")
plot(fit, plot.type = "resplot")
barplot(sort(fit$sps, decreasing = TRUE), main = "Stress per Subject", cex.names = 0.8) 

fit$gspace
fit$cweights
fit$stress
fit$sps
fit$delta
fit$obsdiss
fit$confdiss
fit$conf
fit$resmat
fit$rss
fit$spp
fit$ndim

#=============================
# Figura 4.9
#=============================

dim1<-c(0,0,0.1092108,2.608886)
dim2<-c(0,0,1.3466350,0.1602203)

datos.grafico<-data.frame(dim1,dim2)
datos.grafico$departamento<-c("DEP1","DEP2")

library(ggplot2)
ggplot(data=datos.grafico, aes(x=dim1, y=dim2, group=departamento)) +
  geom_line(aes(linetype=departamento),arrow = arrow(angle = 10, ends = "last", type = "closed",length=unit(4, "mm")))+
  geom_point()+
  scale_x_continuous(expand = c(0, 0), limits = c(0,3))+
  scale_y_continuous(expand = c(0, 0), limits = c(0,3))+
  xlab("Dimension 1") + ylab("Dimensión 2")+
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=0.5)
 

#=============================
# Cálculo cosenos directores
#=============================

Y1<-c(16795,3132,2545,2301,4212,1289,6286,4376,16575,10436,2607,6503,12707,2725,1434,5050,723)
Y2<-c(0.13,0.18,0.12,0.12,0.17,0.13,0.10,0.08,0.12,0.14,0.07,0.08,0.26,0.10,0.06,0.14,0.08)
X1<-c(fit$gspace[,1])
X2<-c(fit$gspace[,2])

summary(lm(Y1~X1+X2))
summary(lm(Y2~X1+X2))  

