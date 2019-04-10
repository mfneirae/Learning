library(haven)
Datos_4_2_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_4_2_Caso.sav")

#Quitamos zona de la base de datos
Datos_4_2_Caso$zona <- NULL
#Normalizamos los indicadores
datos.normalizados <- scale(Datos_4_2_Caso)
#Calcualamos la matriz de distancias
datos<-dist(datos.normalizados, method = "euclidean", diag = TRUE, upper = TRUE)

m <- as.matrix(datos) 
rownames(m) <- paste("Z", 1:12)
colnames(m) <- paste("Z", 1:12)

datos<-as.dist(m)

#=======================
# Paquete SMACOF
#=======================


library(smacof)
fit<-mds(delta=datos,ndim=2,type="ratio")

# Coordenadas 
print(fit$conf)

# Disparidades
print(fit$dhat)

# Distancias entre configuraciones
print(fit$confdiss)

# Stress
print(fit$stress)

# Stress por punto
print(fit$spp)

#RSQ
dist<-cbind(c(fit$dhat))
dism<-cbind(c(fit$confdiss))
summary(lm(dist~dism))


# gráfico: "confplot", "resplot", "Shepard", "stressplot", "bubbleplot"

plot(fit, plot.type="confplot", plot.dim = c(1,2), sphere = TRUE,
     bubscale = 0.1, col = 1, label.conf = list(label = TRUE, pos = 3, col = 1, cex = 0.8), shepard.x= NULL, identify = FALSE, type = "p", pch = 20, asp = 1, col.hist = NULL)


#=============================
# Figura 4.9
#=============================

# Estimamos el modelo para diferente número de dimensiones

fit1<-mds(delta=datos,ndim=1,type="ratio")
fit2<-mds(delta=datos,ndim=2,type="ratio")
fit3<-mds(delta=datos,ndim=3,type="ratio")
fit4<-mds(delta=datos,ndim=4,type="ratio")
fit5<-mds(delta=datos,ndim=5,type="ratio")
fit6<-mds(delta=datos,ndim=6,type="ratio")
fit7<-mds(delta=datos,ndim=7,type="ratio")

# Generamos un data.frame a partir de los vectores con la información de stress y dimensiones

stress<-c(fit1$stress,fit2$stress,fit3$stress,fit4$stress,fit5$stress,fit6$stress,fit7$stress)
dimension<-c(1,2,3,4,5,6,7)
datos.figura<-data.frame(dimension,stress)

# Pedimos el gráfico

library(ggplot2)
ggplot(data=datos.figura, aes(x=dimension, y=stress)) +
  geom_line(aes(linetype="solid"))+
  geom_point()+
  scale_x_discrete(limits = 1:7, labels=1:7)+
  xlab("Número de dimensiones") + ylab("Stress")+
  theme(legend.position="none")

#=============================
# Figuras 4.10-4.11
#=============================

D1<-fit$conf[,1]
D2<-fit$conf[,2]
datos.cluster<-data.frame(D1,D2)


#calculo de la distancia euclídea
matriz.dis.euclid<-dist(datos.cluster[,c("D1","D2")],method="euclidean",diag=TRUE)

#calculo de la distancia euclidea al cuadrado
matriz.dis.euclid2<-(matriz.dis.euclid)^2

round(print(matriz.dis.euclid2),2)

#efectuamos el cluster con método centroide
hclust.ward<-hclust(matriz.dis.euclid,method="ward.D2")
plot(hclust.ward,labels=datos.cluster$rownames,hang=-1,cex = 0.6)
rect.hclust(hclust.ward, k = 3, border = "red")


#=============================
# Figuras 4.12
#=============================


grupo.ward<-cutree(hclust.ward, k = 3, h = NULL)

datos.caso3.grupos<-cbind(datos.cluster,grupo.ward)
datos.caso3.grupos$id<-NULL

zona<-c("Z1","Z2","Z3","Z4","Z5","Z6","Z7","Z8","Z9","Z10","Z11","Z12")
datos.grafico<-data.frame(D1,D2,grupo.ward,zona)
datos.grafico$grupo.ward <- ifelse(datos.grafico$grupo.ward == 1, "cluster 1", ifelse(datos.grafico$grupo.ward  == 2, "cluster 2",ifelse(datos.grafico$grupo.ward  == 3, "cluster 3",99)))

ggplot(datos.grafico, aes(x=D1, y=D2,shape=grupo.ward,colour=grupo.ward, label=zona)) +
  geom_point(size=2)+
  scale_color_manual(values=c("black","red","blue"))+
  geom_text(check_overlap = F,hjust=0.5, vjust = -1.2, nudge_y = 0.0, nudge_x = 0.0, size=3)+
  expand_limits(x=c(-1.0,1.25), y=c(-1, 1))+
  labs (x="Dimension 1", y="Dimension 2")+
  guides(colour=FALSE, shape=FALSE)




