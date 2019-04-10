library(ca)
library(FactoMineR)
library(factoextra)
library(gplots)
library(graphics)
library(ggplot2)

#=============================
# Cargamos datos
#=============================
datos<-data.frame(smoke)
colnames(datos)<-c("No fuma","Poco","Medio","Mucho")
rownames(datos)<-c("D.Sr","D.Jr","Emp.Sr","Emp.Jr", "Secr.")
datos.tabla<-data.matrix(datos)

# Gráfico de mosaico
mosaicplot(datos, las=1, col="steelblue", main = "Descripción datos")

#=============================
# % horizontales y verticales
#=============================
library(gmodels)
CrossTable(datos.tabla, prop.t=FALSE,prop.chisq=FALSE,format="SAS")



#======================
#Paquete ca
#======================


fit.ca<-ca(datos)
summary(fit.ca, scree = TRUE, rows=TRUE, columns=TRUE)
print(fit.ca)
fviz_ca_biplot(fit.ca)+theme_grey()


#======================
#Paquete FactoMinerR
#======================

fit.factominer<-CA(datos,ncp=2,graph=TRUE)
summary(fit.factominer,graph=TRUE)
fviz_ca_biplot(fit.factominer)+theme_grey()

#======================================
# Descomposición en valores singulares
#======================================


a1<-c(0.02020,	-0.02538,	-0.02044,	0.03468)
a2<-c(-0.05098,	-0.04205,	 0.03645,	0.07865)
a3<-c( 0.15922,	-0.03948,	-0.07795,-0.07299)
a4<-c(-0.13394,	 0.05533,	 0.06404,	0.03413)
a5<-c( 0.05374,	 0.00510,	-0.02619,-0.04953)

A<-t(cbind(a1,a2,a3,a4,a5))

svd(A,n=5,nv=4)

#========================================
# Puntos fila y columna suplementarios
#========================================

col1.sup<-c(0,1,5,10,7)
col2.sup<-c(11,17,46,78,18)
fila.sup<-c(0.42,0.29,0.20,0.09,NA,NA)



datos.sup<-cbind(datos,col1.sup,col2.sup)
colnames(datos.sup)<-c("No fuma","Poco","Medio","Mucho","Alch.Si","Alch.No")

datos.sup<-rbind(datos.sup,fila.sup)
rownames(datos.sup)<-c("D.Sr","D.Jr","Emp.Sr","Emp.Jr", "Secr.","Media.Pob")

fit.sup <- CA (datos.sup, row.sup = 6:6, col.sup = 5:6,
              graph = FALSE)
summary(fit.sup)
fviz_ca_biplot(fit.sup) +
  theme_grey()

# coordenadas de las filas y columnassuplementarias
fit.sup$row.sup
fit.sup$col.sup
