# =======================================
# Librerias
# =======================================

library (lavaan)
library(semPlot)
library(ggplot2)
library(psych)
library(GPArotation)
library(Matrix)
library(nFactors)
library(paran)
library(descr)




# =======================================
# Datos Sharma, (1996)
# =======================================

library(haven)
Datos_12_2_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_12_2_Caso.sav")

datos<-Datos_12_2_Caso
R<-cor(datos[,3:12],use="complete.obs",method="pearson")


# =======================================
# Medidas de ajuste o de adecuación muestral
# =======================================

cortest.bartlett(R,n=95)
KMO(R)



# =======================================
# Determinación del número de factores
# =======================================

# autovalor>1, sedimentación, paralelo
paran(datos[3:12], iterations=5000,graph=TRUE,color=FALSE) 

# test de Barlett
nBartlett(R, alpha=0.01, N=95, details=TRUE)


# =======================================
# Estimación EFA ejes principales
# =======================================

fit.pa<-fa(datos[,3:12],nfactors=3,fm="pa",rotate="varimax",scores="regression")
fa.sort(fit.pa)
print(fit.pa)
plot(fit.pa) 


# =======================================
# Mapa perceptual
# =======================================

datos.grafico<-data.frame(fit.pa$scores,datos$BRAND)
datos.grafico$datos.BRAND<-as.character(datos.grafico$datos.BRAND)
  
attach(datos.grafico)
datos.grafico$tipo.bebida[datos.BRAND== "1" | datos.BRAND=="2"] <- "Colas"
datos.grafico$tipo.bebida[datos.BRAND== "3" | datos.BRAND=="4"] <- "Deportivas"
datos.grafico$tipo.bebida[datos.BRAND== "5" | datos.BRAND=="6"] <- "Te"
detach(datos.grafico)


# PA1 VS PA3
ggplot(datos.grafico)+
  geom_point(aes(x=PA1, y=PA3,color=datos.grafico$tipo.bebida,
              shape=datos.grafico$tipo.bebida),size=3)+
  #geom_text_repel(aes(x=Dim.1, y=Dim.2),label=rownames(datos.grafico2))+
  geom_vline(xintercept = 0,colour="darkgray")+
  geom_hline(yintercept = 0,colour="darkgray")+
  labs (x="1.Apaga sed (32%)", y="3.Funcional (35%)")+
  theme(legend.title=element_blank())

# PA1 VS PA2
ggplot(datos.grafico)+
  geom_point(aes(x=PA1, y=PA2,color=datos.grafico$tipo.bebida,
                 shape=datos.grafico$tipo.bebida),size=3)+
  #geom_text_repel(aes(x=Dim.1, y=Dim.2),label=rownames(datos.grafico2))+
  geom_vline(xintercept = 0,colour="darkgray")+
  geom_hline(yintercept = 0,colour="darkgray")+
  labs (x="1.Apaga sed (32%)", y="2.Sabor (33%)")+
  theme(legend.title=element_blank())

# PA2 VS PA3
ggplot(datos.grafico)+
  geom_point(aes(x=PA2, y=PA3,color=datos.grafico$tipo.bebida,
                 shape=datos.grafico$tipo.bebida),size=3)+
  #geom_text_repel(aes(x=Dim.1, y=Dim.2),label=rownames(datos.grafico2))+
  geom_vline(xintercept = 0,colour="darkgray")+
  geom_hline(yintercept = 0,colour="darkgray")+
  labs (x="2.Sabor (33%)", y="3.Funcional (32%)")+
  theme(legend.title=element_blank())


# =======================================================================
# Medias de las variables que configuran dada factor por tipo de bebida
# =======================================================================


datos.medias<-data.frame(datos,datos.grafico$tipo.bebida)
datos.medias<-rename(datos.medias, c("datos.grafico.tipo.bebida"="bebida"))


aggregate(cbind(X1,X2,X3,X4,X5,X7,X8,X9)~bebida, data=datos.medias, mean, na.rm=TRUE)
aggregate(cbind(F1=((X3+X7+X10)/3),F2=((X1+X4+X8)/3),F3=((X2+X5+X9)/3))~bebida, data=datos.medias, mean, na.rm=TRUE)
