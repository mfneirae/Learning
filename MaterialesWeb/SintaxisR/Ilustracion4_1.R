library(haven)
Ilustracion4_1 <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Ilustracion4_1.sav")
View(Ilustracion4_1)

datos<-Ilustracion4_1
datos$CIUDAD<-NULL


# k número de dimensiones


fit <- cmdscale(datos,eig=TRUE, k=2) 

fit # muestra resultados

# Gráfico de la solución
x <- fit$points[,2]
y <- fit$points[,1]
plot(x, y, xlab="Coordenada 1", ylab="Coordenada 2", 
    	type="n")
text(x, y, labels = colnames(datos), cex=.7) 

library(ggplot2)

qplot(x, y, data = datos, colour = I("black"),label=colnames(datos), geom=c("text"),vjust = 0, nudge_y = 0.5, xlab="Coordenada 1", ylab="Coordenada 2", )
