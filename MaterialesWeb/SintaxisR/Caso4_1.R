# Introducimos la matriz de datos

datos<-matrix(c(
  0.0,1.0,2.1,6.1,5.2,
  1.0,0.0,2.4,6.9,5.3,
  2.1,2.4,0.0,5.1,4.1,
  6.1,6.9,5.1,0.0,3.1,
  5.2,5.3,4.1,3.1,0.0),
 ncol=5,nrow=5,byrow=T,dimnames=list(c("X1","X2","X3","X4","X5")))



#=======================
# Paquete SMACOF
#=======================


library(smacof)
fit2<-mds(delta=datos,ndim=2,type="interval")

#Generación del gráfico

x2 <- fit2$conf[,1]
y2 <- fit2$conf[,2]
plot(x, y, xlab="Coordenada 1", ylab="Coordenada 2", 
     type="n")
text(x, y, labels = rownames(datos), cex=.7) 

qplot(x2, y2, colour = I("black"),label=rownames(datos), geom=c("text"),vjust = 0, nudge_y = 0.5, xlab="Coordenada 1", ylab="Coordenada 2")

# Coordenadas 
print(fit2$conf)

# Disparidades
print(fit2$dhat)

# Distancias entre configuraciones
print(fit2$confdiss)

# Stress
print(fit2$stress)

# Stress por punto
print(fit2$spp)

#RSQ
dist<-cbind(c(fit2$dhat))
dism<-cbind(c(fit2$confdiss))
summary(lm(dist~dism))

plot(fit2, plot.type="Shepard", plot.dim = c(1,2), sphere = TRUE,
     bubscale = 0.1, col = 1, label.conf = list(label = TRUE, pos = 3, col = 1, cex = 0.8), shepard.x = NULL, identify = FALSE, type = "p", pch = 20, asp = 1, col.hist = NULL)


