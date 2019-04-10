# Datos Caso 3.1 ==========================================================================

nombre.empresa<-c("E1","E2","E3","E4","E5","E6","E7","E8")
inversion<-c(16,12,10,12,45,50,45,50)
ventas<-c(10,14,22,25,10,15,25,27)

DatosCaso3.1<-data.frame(nombre.empresa,inversion,ventas)
DatosCaso3.1b<-data.frame(inversion,ventas)


# Figura 3.2 ==========================================================================
library(ggplot2)

qplot(inversion, ventas, data = DatosCaso3.1, colour = I("black"),label=nombre.empresa, geom=c("point", "text"),vjust = 0, nudge_y = 0.5)


#Cuadro 3.2 ==========================================================================
matriz.dis.euclid<-dist(DatosCaso3.1[,c("inversion","ventas")],method="euclidean",diag=TRUE)
round(print(matriz.dis.euclid),2)

#Cuadro 3.5 ==========================================================================

nombre.caso<-c("E1","E2","E3","E4","E5")
X1<-c(1,0,1,0,1)
X2<-c(1,1,1,0,1)
X3<-c(0,1,0,0,1)
X4<-c(0,1,1,1,0)

DatosCuadro3.5<-data.frame(nombre.caso,X1,X2,X3,X4)

library(ade4)

dist.binary(DatosCuadro3.5[,c("X1","X2","X3","X4")], method = 1, diag = TRUE, upper = FALSE)
dist.binary(DatosCuadro3.5[,c("X1","X2","X3","X4")], method = 2, diag = TRUE, upper = FALSE)

matriz.dis.binary<-dist(DatosCuadro3.5[,c("X1","X2","X3","X4")],method="binary",diag=TRUE)
round(print(matriz.dis.binary),2)


#Cuadro 3.7 ==========================================================================

nombre.empresa2<-c("E1","E2","E3","E4","E5","E6","E7","E8")
activos<-c(10.0e9,10.5e9,10.0e9,10.5e9,20.0e9,20.5e9,20.0e9,20.5e9)
trabajadores<-c(100,90,200,190,200,190,100,90)

DatosCuadro3.7<-data.frame(nombre.empresa2,activos,trabajadores)

matriz.dis.euclid2<-dist(DatosCuadro3.7[,c("activos","trabajadores")],method="euclidean",diag=TRUE)

#Cuadro 3.9 ==========================================================================

#Normalizamos los datos
DatosCuadro3.7.norm<-scale(DatosCuadro3.7[,c("activos","trabajadores")])
matriz.dis.euclid.norm<-dist(DatosCuadro3.7.norm[,c("activos","trabajadores")],method="euclidean",diag=TRUE)
round(print(matriz.dis.euclid.norm),2)

#Cuadro 3.10 ==========================================================================


library(stats)
#calculo de la distancia euclídea
matriz.dis.euclid<-dist(DatosCaso3.1[,c("inversion","ventas")],method="euclidean",diag=TRUE,label=DatosCaso3.1$nombre.empresa)

#calculo de la distancia euclidea al cuadrado
matriz.dis.euclid2<-(matriz.dis.euclid)^2

round(print(matriz.dis.euclid2),2)

#efectuamos el cluster con método centroide
hclust.centroide<-hclust(matriz.dis.euclid2,method="centroid")

#Cuadro 3.10 ==========================================================================


#Saca el historial de aglomeración del objeto hclust.centroide
data.frame(hclust.centroide[2:1])

#Figura 3.4 ==========================================================================


#dendograma centroide

plot.hclust<-plot(hclust.centroide,labels=DatosCaso3.1$nombre.empresa)
rect.hclust(hclust.centroide, k = 2, border = "red")



#Figura 3.5 ==========================================================================

#efectuamos el cluster con método vecino más cercano
hclust.cercano<-hclust(matriz.dis.euclid2,method="single")
data.frame(hclust.cercano[2:1])


#dendograma vecino más cercano

plot(hclust.cercano,labels=DatosCaso3.1$nombre.empresa)
rect.hclust(hclust.cercano, k = 2, border = "red")




#Figura 3.6 ==========================================================================

#efectuamos el cluster con método vecino más lejano
hclust.lejano<-hclust(matriz.dis.euclid2,method="complete")
data.frame(hclust.lejano[2:1])


#dendograma vecino más lejao

plot(hclust.lejano,labels=DatosCaso3.1$nombre.empresa)
rect.hclust(hclust.lejano, k = 2, border = "red")

#Cuadro 3.12 ==========================================================================

#efectuamos el cluster con método vinculación promedio
hclust.promedio<-hclust(matriz.dis.euclid2,method="average")
data.frame(hclust.promedio[2:1])


#dendograma vecino más lejano

plot(hclust.promedio,labels=DatosCaso3.1$nombre.empresa)
rect.hclust(hclust.promedio, k = 2, border = "red")

#Cuadro 3.13 ==========================================================================

#efectuamos el cluster con método de Ward
hclust.ward<-hclust(matriz.dis.euclid2,method="ward.D")
data.frame(hclust.ward[2:1])


#dendograma vecino más lejano

plot(hclust.ward,labels=DatosCaso3.1$nombre.empresa)
rect.hclust(hclust.ward, k = 2, border = "red")

#Figura 3.7 ==========================================================================

#Generación de la base de datos simulada

set.seed(1)
x<-rbind(matrix(rnorm(100,sd=0.1),ncol=2),
         matrix(rnorm(100,mean=1,sd=0.2),ncol=2),
         matrix(rnorm(100,mean=5,sd=0.1),ncol=2),
         matrix(rnorm(100,mean=7,sd=0.2),ncol=2))

DatosCaso3.2<-data.frame(x)
#Gráfico
ggplot(data=DatosCaso3.2, aes(X1,X2)) + geom_point()


#Cuadro 3.16 ==========================================================================

#Pedimos los indicadores para un jerárquico con distancia euclídea y método de ward

library(NbClust)
res<-NbClust(DatosCaso3.2, distance = "euclidean", min.nc=2, max.nc=8, method = "ward.D2", index = "alllong")

res$All.index
res$Best.nc
res$Best.partition



#Cuadro 3.20 ==========================================================================

#efectuamos el cluster con método centroide
kmeans.caso3.1<-kmeans(DatosCaso3.1b, 2) # solución dos conglomerados
#obtenemos las medias 
aggregate(DatosCaso3.1b,by=list(kmeans.caso3.1$cluster),FUN=mean)
# añadimos la pertenencia al cluster
DatosCaso3.1b <- data.frame(DatosCaso3.1b, kmeans.caso3.1$cluster)

print(kmeans.caso3.1$cluster)
print(kmeans.caso3.1$centers)
print(kmeans.caso3.1$totss)
print(kmeans.caso3.1$withins)
print(kmeans.caso3.1$tot.withins)
print(kmeans.caso3.1$betweenss)
print(kmeans.caso3.1$size)
print(kmeans.caso3.1$iter)

#Cuadro 3.21 ==========================================================================

library(psych)
fit.inversion<-aov(inversion ~ kmeans.caso3.1.cluster, data = DatosCaso3.1b)
fit.ventas<-aov(ventas ~ kmeans.caso3.1.cluster, data = DatosCaso3.1b)
summary(fit.inversion)
summary(fit.ventas)
print(model.tables(fit.inversion,"means"),digits=3)
print(model.tables(fit.ventas,"means"),digits=3)

#Cuadro 3.23 ==========================================================================

#Importamos el fichero de datos

library(haven)
Datos_3_3_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_3_3_Caso.sav")
View(Datos_3_3_Caso)

matriz.dis.euclid.caso3<-dist(Datos_3_3_Caso[,c("automovi","tvcolor","video","microond","lavavaji","telefono")],method="euclidean",diag=TRUE)
round(print(matriz.dis.euclid),2)

# Calculamos la distancia de Mahalanobis y su significatividad


mean<-colMeans(Datos_3_3_Caso[,c("automovi","tvcolor","video","microond","lavavaji","telefono")]) 

Sx<-cov(Datos_3_3_Caso[,c("automovi","tvcolor","video","microond","lavavaji","telefono")])                                    
D2<-mahalanobis(Datos_3_3_Caso[,c("automovi","tvcolor","video","microond","lavavaji","telefono")],mean,Sx,inverted = FALSE)   
print(D2)                                                  
pchisq(D2, df=6, lower.tail=FALSE)                         
qchisq(.99, df=6)                                          

#Figura 3.11 ==========================================================================

#==================================
#ESTIMACIONES DE LOS CONGLOMERADOS
#==================================

#estimacion con promedio

hclust.average.caso3<-hclust(matriz.dis.euclid.caso3,method="average")
data.frame(hclust.average.caso3[2:1])

plot(hclust.average.caso3,labels=Datos_3_3_Caso$id,xlab = NULL, ylab = "Height", sub=NULL, hang=-1,cex = 0.6)

rect.hclust(hclust.average.caso3, k = 2, border = "red")

#estimacion con Ward

hclust.ward.caso3<-hclust(matriz.dis.euclid.caso3,method="ward.D2")
data.frame(hclust.ward.caso3[2:1])

plot(hclust.ward.caso3,labels=Datos_3_3_Caso$id,xlab = NULL, ylab = "Height", sub=NULL, hang=-1,cex = 0.6)

rect.hclust(hclust.ward.caso3, k = 2, border = "red")

#estimacion con complete
hclust.complete.caso3<-hclust(matriz.dis.euclid.caso3,method="complete")
data.frame(hclust.complete.caso3[2:1])

plot(hclust.complete.caso3,labels=Datos_3_3_Caso$id,xlab = NULL, ylab = "Height", sub=NULL, hang=-1,cex = 0.6)

rect.hclust(hclust.complete.caso3, k = 2, border = "red")

#estimacion con single
hclust.single.caso3<-hclust(matriz.dis.euclid.caso3,method="single")
data.frame(hclust.single.caso3[2:1])

plot(hclust.single.caso3,labels=Datos_3_3_Caso$id,xlab = NULL, ylab = "Height", sub=NULL, hang=-1,cex = 0.6)

rect.hclust(hclust.single.caso3, k = 2, border = "red")

#estimacion con centroid
hclust.centroid.caso3<-hclust(matriz.dis.euclid.caso3,method="centroid")
data.frame(hclust.centroid.caso3[2:1])

plot(hclust.centroid.caso3,labels=Datos_3_3_Caso$id,xlab = NULL, ylab = "Height", sub=NULL, hang=-1,cex = 0.6)
rect.hclust(hclust.centroid.caso3, k = 2, border = "red")

#Cuadro 3.24 ==========================================================================

#==================================
#DETERMINACIÓN DEL NÚMERO DE GRUPOS
#==================================


library(NbClust)

Datos.NbClust<-Datos_3_3_Caso[,c("automovi","tvcolor","video","microond","lavavaji","telefono")]

res.wardD2<-NbClust(Datos.NbClust, distance = "euclidean", min.nc=2, max.nc=15, method = "ward.D2", index = "alllong")

res.wardD2$All.index
res.wardD2$Best.nc
res.wardD2$Best.partition


res.average<-NbClust(Datos.NbClust, distance = "euclidean", min.nc=2, max.nc=15, method = "average", index = "alllong")

res.average$All.index
res.average$Best.nc
res.average$Best.partition


res.complete<-NbClust(Datos.NbClust, distance = "euclidean", min.nc=2, max.nc=15, method = "complete", index = "alllong")

res.complete$All.index
res.complete$Best.nc
res.complete$Best.partition


#Figura 3.12 ==========================================================================
#Tomamos los datos del valor del CCC para cada método de la salida anterior

numero.cluster<-c(2,3,4,5,6,7,8,9,10,11,12,13,14,15)
ward.ccc    <-c(143.9,92.2,82.9,80.6,77.4,74.0,72.6,68.9,66.1,64.6,61.7,53.5,44.0,33.8)
average.ccc <-c(143.9,83.2,82.9,80.6,77.4,73.8,72.3,67.5,62.9,56.5,61.7,52.1,44.0,33.8)
complete.ccc<-c(143.9,92.2,82.9,79.0,77.4,74.0,67.8,67.8,66.1,64.6,61.7,53.5,44.0,33.8)

datos.figura3.12<-data.frame(numero.cluster,ward.ccc,average.ccc,complete.ccc)

metodo<-c("ward","ward","ward","ward","ward","ward","ward","ward","ward","ward","ward","ward","ward","ward","average","average","average","average","average","average","average","average","average","average","average","average","average","average","complete","complete","complete","complete","complete","complete","complete","complete","complete","complete","complete","complete","complete","complete")
ccc<-c(143.9,92.2,82.9,80.6,77.4,74.0,72.6,68.9,66.1,64.6,61.7,53.5,44.0,33.8,143.9,83.2,82.9,80.6,77.4,73.8,72.3,67.5,62.9,56.5,61.7,52.1,44.0,33.8,143.9,92.2,82.9,79.0,77.4,74.0,67.8,67.8,66.1,64.6,61.7,53.5,44.0,33.8)
cluster<-c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,2,3,4,5,6,7,8,9,10,11,12,13,14,15,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
datos.figura3.12b<-data.frame(metodo,ccc,cluster)

library(ggplot2)
ggplot(data=datos.figura3.12b, aes(x=cluster, y=ccc, group=metodo)) +
  geom_line(aes(linetype=metodo))+
  geom_point()+
  scale_x_continuous(breaks = round(seq(min(datos.figura3.12b$cluster), max(datos.figura3.12b$cluster), by = 1),1)) 


#Figura 3.13 ==========================================================================

plot(hclust.ward.caso3,labels=Datos_3_3_Caso$id,xlab = NULL, ylab = "Height", sub=NULL, hang=-1,cex = 0.6)
rect.hclust(hclust.ward.caso3, k = 2, border = "red")
plot(hclust.average.caso3,labels=Datos_3_3_Caso$id,xlab = NULL, ylab = "Height", sub=NULL, hang=-1,cex = 0.6)
rect.hclust(hclust.average.caso3, k = 2, border = "red")
plot(hclust.complete.caso3,labels=Datos_3_3_Caso$id,xlab = NULL, ylab = "Height", sub=NULL, hang=-1,cex = 0.6)
rect.hclust(hclust.complete.caso3, k = 2, border = "red")

#Cuadro 3.24 ==========================================================================
#======================================
#AÑADIMOS LOS GRUPOS A LA BASE DE DATOS
#======================================


grupo.ward<-cutree(hclust.ward.caso3, k = 2, h = NULL)

datos.caso3.grupos<-cbind(Datos_3_3_Caso,grupo.ward)
datos.caso3.grupos$id<-NULL

print(datos.caso3.grupos)

#======================================
#CALCULAMOS LOS CENTROIDES
#======================================

round(aggregate(datos.caso3.grupos,list(grupo.ward), mean ),2)



#Dejamos en la base de datos solo las 6 variables
datos.caso3.grupos.kmeans<-datos.caso3.grupos
datos.caso3.grupos.kmeans$grupo.ward<-NULL

#creamos dos vectores con los centroides

c1<-c(66.87,96.82,56.01,25.43,11.81,80.71)
c2<-c(70.70,98.53,63.47,44.70,22.43,90.23)

solucion<-kmeans(datos.caso3.grupos.kmeans,rbind(c1, c2))
print(solucion)

#Añadimos los centroides finales a la base de datos

datos.caso3.grupos.kmeans <- data.frame(datos.caso3.grupos.kmeans, solucion$cluster)

#Comprobamos la significatividad de las diferencias

t.test(automovi~solucion.cluster,data=datos.caso3.grupos.kmeans)
t.test(tvcolor~solucion.cluster,data=datos.caso3.grupos.kmeans)
t.test(video~solucion.cluster,data=datos.caso3.grupos.kmeans)
t.test(microond~solucion.cluster,data=datos.caso3.grupos.kmeans)
t.test(lavavaji~solucion.cluster,data=datos.caso3.grupos.kmeans)
t.test(telefono~solucion.cluster,data=datos.caso3.grupos.kmeans)

#Quitamos de la base de datos la variable con el grupo para hacer el pca


datos.pca<-datos.caso3.grupos.kmeans
datos.pca$solucion.cluster<-NULL

pca <- princomp(datos.pca, cor=T) # principal components analysis using correlation matrix
pc.comp <- pca$scores
pc.comp1 <- -1*pc.comp[,1] # principal component 1 scores (negated for convenience)
pc.comp2 <- -1*pc.comp[,2] # principal component 2 scores (negated for convenience)

plot(pc.comp1, pc.comp2,col=solucion$cluster)
points(solucion$centers, pch=16)

ccaa<-c("España","Andalucia","Aragón","Asturias","Baleares","Canarias","Cantabria",
        "CyL","CLM","Cataluña","Valencia","Extremadura","Galicia","Madrid","Murcia",
        "Navarra","P.Vasco","Rioja")

datos.grafico.pca<-data.frame(pc.comp1,pc.comp2,solucion$cluster,ccaa)
datos.grafico.pca$solucion.cluster <- ifelse(datos.grafico.pca$solucion.cluster == 1, "cluster 1", ifelse(datos.grafico.pca$solucion.cluster == 2, "cluster 2",99))

ggplot(datos.grafico.pca, aes(x=pc.comp1, y=pc.comp2,shape=solucion.cluster,colour=solucion.cluster, label=ccaa)) +
  geom_point(size=2)+
  scale_color_manual(values=c("black","red"))+
  geom_text(check_overlap = F,hjust=0.5, vjust = -0.7, nudge_y = 0.0, nudge_x = 0.0)+
  expand_limits(x=c(-4,5), y=c(-2, 3))+
  labs (x="Componente Principal 1", y="Componente principal 2")+
  guides(colour=FALSE, shape=FALSE)
  
  
