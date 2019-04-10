#Leemos el fichero de datos del caso 2.2

library(haven)
Datos_2_2_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_2_2_Caso.sav")
View(Datos_2_2_Caso)

#Figura 2.1 ==========================================================================

#tipificamos la variable SYS y se llamar? Z.SYS

Datos_2_2_Caso <- local({
  .Z <- scale(Datos_2_2_Caso[,c("SYS")])
  within(Datos_2_2_Caso, {
    Z.SYS <- .Z[,1] 
  })
})

library(ggplot2)
ggplot(data=Datos_2_2_Caso, aes(x=CASO, y=Z.SYS, group=1,label = CASO)) +
  geom_line(size=1.25)+
  geom_text(aes(label=ifelse(Z.SYS>3,as.character(CASO),'')),hjust=2,vjust=0)+
  geom_hline(yintercept=3,size=1, linetype="dashed")+
labs(x="Número del caso", y="Valores SYS estandarizados")

#Figura 2.2 ==========================================================================

# Fit a linear model 
m <- lm(SYS ~ EDAD, data = Datos_2_2_Caso) 

# cbind the predictions to SYS 
mpi <- cbind(Datos_2_2_Caso, predict(m, interval = "prediction")) 

 
ggplot(mpi, aes(x = EDAD)) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr), 
              fill = "grey", alpha = 0.5) + 
  geom_point(aes(y = SYS)) + 
  geom_text(data=subset(Datos_2_2_Caso, EDAD > 1 & SYS > 2700),
            aes(EDAD,SYS,label=CASO),hjust=2,vjust=0)+
  geom_line(aes(y = fit), colour = "black", size = 1)+
  labs(x="Edad directivo", y="Remuneración directivo (miles euros)")
  
#Grafico 2.3 ==========================================================================

# Fit a linear model 
m2 <- lm(SYS ~ EXP_PTO, data = Datos_2_2_Caso) 

# cbind the predictions to SYS 
mpi2 <- cbind(Datos_2_2_Caso, predict(m2, interval = "prediction")) 


ggplot(mpi2, aes(x = EXP_PTO)) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr), 
              fill = "grey", alpha = 0.5) + 
  geom_point(aes(y = SYS)) + 
  geom_text(data=subset(Datos_2_2_Caso, EXP_PTO > 1 & SYS > 2700),
            aes(EXP_PTO,SYS,label=CASO),hjust=2,vjust=0)+
  geom_line(aes(y = fit), colour = "black", size = 1)+
  labs(x="Antigüedad en el puesto", y="Remuneración directivo (miles euros)")

#Grafico 2.4 ==========================================================================

# Fit a linear model 
m3 <- lm(SYS ~ BENEF, data = Datos_2_2_Caso) 

# cbind the predictions to SYS 
mpi3 <- cbind(Datos_2_2_Caso, predict(m3, interval = "prediction")) 


ggplot(mpi3, aes(x = BENEF)) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr), 
              fill = "grey", alpha = 0.5) + 
  geom_point(aes(y = SYS)) + 
  geom_text(data=subset(Datos_2_2_Caso, BENEF > -1000 & SYS > 2700),
            aes(BENEF,SYS,label=CASO),hjust=2,vjust=0)+
  geom_line(aes(y = fit), colour = "black", size = 1)+
  labs(x="Beneficio empresa (millones euros)", y="Remuneración directivo (miles euros)")

# Gráfico 2.6 ==========================================================================
#Dejamos en la base de datos "Datos" solo las 5 variables que vamos a utilizar en el análisis 
# SYS, EDAD, EXP_PTO, EXP_EMP, VENTAS y BENEF

Datos <- within(Datos_2_2_Caso, {
  CASO <- NULL
  ZBENEF <- NULL
  ZEDAD <- NULL
  ZEXP_EMP <- NULL
  ZEXP_PTO <- NULL
  ZSYS <- NULL
  ZVENTAS <- NULL
  Z.SYS <- NULL
})

mean<-colMeans(Datos)                             #Cálculo del vector de medias de las variables
Sx<-cov(Datos)                                    #Cálculo de la matriz de covarianzas
D2<-mahalanobis(Datos,mean,Sx,inverted = FALSE)   #Cálculo de la D2 de Mahalanobis
print(D2)                                                  #Muestra los valores de la D2
pchisq(D2, df=6, lower.tail=FALSE)                         #df=5 porque hay 5 variables. Muestra la signficatividad
qchisq(.99, df=6)                                          #Calcula el valor crítico
qchisq(.999, df=6) 


#Volvemos a añadir el numero del caso para poder hacer el gráfico con ese dato en el eje x
Datos$ObsNumber <- 1:100

library(ggplot2)
ggplot(data=Datos, aes(x=ObsNumber, y=D2, group=1,label = ObsNumber)) +
  geom_line(size=1.25)+
  geom_text(aes(label=ifelse(D2>16.8119,as.character(ObsNumber),'')),hjust=2,vjust=0)+
  geom_hline(yintercept=16.8119,size=1, linetype="dashed")+
  geom_text(aes(0,16.8119,label = "p<0.01", vjust = -1))+
  labs(x="Número del caso", y="D2 de Mahalanobis")

library(ggplot2)
ggplot(data=Datos, aes(x=ObsNumber, y=D2, group=1,label = ObsNumber)) +
  geom_line(size=1.25)+
  geom_text(aes(label=ifelse(D2>22.45774,as.character(ObsNumber),'')),hjust=2,vjust=0)+
  geom_hline(yintercept=22.45774,size=1, linetype="dashed")+
  geom_text(aes(0,22.45774,label = "p<0.001", vjust = -1))+
  labs(x="Número del caso", y="D2 de Mahalanobis")

#Gráfico 2.7 ==========================================================================

#Creamos la base de datos con los casos 14, 97 y la media de las variables (centroide)

centroide<-c(1121.67,56.93,23.79,8.96,4083.07,137.26)
caso14<-c(4657,60,38,11,11572,1618)
caso97<-c(684,62,36,2,12794,-1086)
datos.grafico27=data.frame(rbind(centroide,caso14,caso97))
colnames(datos.grafico27) = c("SYS","EDAD","EXP_PTO","EXP_EMP","VENTAS","BENEF")
rownames(datos.grafico27) = c("centroide", "caso14","caso97")

#Normalizamos las variables porque tienen escalas muy distitntas

scaled <- as.data.frame(lapply(datos.grafico27, ggplot2:::rescale01))

#Añadimos los nombres de las casos a la base de datos

rownames(scaled) = c("Centroide", "Caso 14","Caso 97")

#La l ibreria fmsb necesita que las dos primeras filas tengan el mínimo y el máximo
#para cada una de las 6 variables, como están normalizadas esos valores son 0 y 1

scaled=rbind(rep(1,3) , rep(0,3) , scaled)

library(fmsb)
colors_border=c( "black", "grey", "red") 
colors_in=c( "NA", "NA" , "NA" )
lineas=c(1,1,3)
grosores=c(4,4,2)
radarchart( scaled[-c(1,2),]  , axistype=0 , maxmin=F,
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=grosores , plty=lineas, pty=32, 
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
            #custom labels
            vlcex=0.7) 

legend(x=0.7, y=1.5, legend = rownames(scaled[-c(1,2),]), bty = "n", pch=1 , col=colors_border , text.col = "black", cex=0.8, pt.cex=1)

#Gráfico 2.12 ======================================================================
#Vemos la falta de normalidad de SYS
shapiro.test(Datos_2_2_Caso$SYS)

#Visualizamos la forma de la distribución con el histograma
library(ggplot2)
qplot(Datos_2_2_Caso$SYS,
      geom="histogram",
      binwidth = 500,  
      main = "Histograma de SYS", 
      xlab = "SYS", 
      ylab = "Frecuencia",
      fill=I("grey"),
      col=I("white"))

#Como vemos que tiene una asimetría izquierda transformamos según la figur 2.12 con un log

logSYS<-log(Datos_2_2_Caso$SYS)

#Calculamos el test de Shapiro y hemos que ya es normal

shapiro.test(logSYS)

#Visualizamos el gráfico

qplot(logSYS,
      geom="histogram",
      binwidth = 0.5,  
      main = "Histograma del log de SYS", 
      xlab = "logSYS",
      ylab = "Frecuencia",
      fill=I("grey"),
      col=I("white"))
