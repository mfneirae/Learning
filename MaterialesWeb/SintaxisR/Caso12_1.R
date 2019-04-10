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



# =======================================
# Datos (Lawley y Maxwell, 1971)
# =======================================

#Conversión del vector de correlaciones en una matriz que llamamos datos.cor
#Definimos el vector que llamamos x

r <- c(1.000,
       .439,1.000,
       .410,.351,1.000,
       .288,.354,.164,1.000,
       .329,.320,.190,.595,1.000,
       .248,.329,.181,.470,.464,1.000)

#Convertimos el vector x en la matriz datos.cor
R<-lav_matrix_lower2full(r)

#Etiquetamos a las variables de la matriz

colnames(R) <- rownames(R) <- 
  c("Gae","Eng", "His","Ari","Alg","Geo")

# ============================================
# Descomposición en autovalores y autovectores
# ============================================

auto<-svd(R)
av<-Diagonal(6,auto$d)
LAMBDA=auto$u%*%sqrt(av)

# =======================================
# Estimación EFA componentes principales
# =======================================

fit.pca<-principal(R,nfactors=2,rotate="none",nobs=220)
print(fit.pca)

plot(fit.pca,labels=row.names(R),cex=.7, ylim=c(-.8,.8)) 

# =======================================
# Estimación EFA componentes principales
# =======================================

fit.pa<-fa(R,nfactors=2,fm="pa",rotate="none",n.obs=220)
print(fit.pa)
plot(fit.pa,labels=row.names(R),cex=.7, ylim=c(-.8,.8)) 

# =======================================
# Estimación EFA máxima verosimilitud
# =======================================

fit.ml<-fa(R,nfactors=2,fm="ml",rotate="none",n.obs=220)
print(fit.ml)
plot(fit.ml,labels=row.names(R),cex=.7, ylim=c(-.8,.8)) 


# =======================================
# Determinación del número de factores
# =======================================

# autovalor>1, sedimentación, paralelo
fa.parallel(R,fm="pa",n.obs=220,ylabel="Eigenvalues")
fa.parallel(R,fm="ml",n.obs=220,ylabel="Eigenvalues")

# test de Barlett
nBartlett(R, N=220, alpha=0.01, cor=TRUE, details=TRUE)

# =======================================
# Rotación de los ejes ORTOGONAL
# =======================================

PA1<-c(0.586,0.594,0.431,0.711,0.702,0.584)
PA2<-c(0.377,0.236,0.413,-0.333,-0.278,-0.184)

datos.grafico<-data.frame(PA1,PA2)
rownames(datos.grafico)<-rownames(R)

# grafico ejes originales
ggplot(datos.grafico)+
  geom_point(aes(x=PA1, y=PA2,colour="darkred"))+
  geom_text_repel(aes(x=PA1, y=PA2),label=rownames(datos.grafico))+
  geom_vline(xintercept = 0,colour="darkgray")+
  geom_hline(yintercept = 0,colour="darkgray")+
  labs (x="Dimensión 1 (37%)", y="Dimensión 2 (10%)")+
  theme(legend.position="none")+
  expand_limits(x=c(0,1), y=c(-0.5, 0.5))+
  theme(aspect.ratio=1)

# estimamos otra vez por PAF para tener la solución rotada
fit.pa<-fa(R,nfactors=2,fm="pa",rotate="varimax",n.obs=220)
fit.pa$rot.mat

#de la matriz de rotación obtenida vemos que el ángulo de rotación
# es cos(theta)=0.7861450-->theta=0.66624975
  

# grafico ejes rotados
ggplot(datos.grafico)+
  geom_point(aes(x=PA1, y=PA2,colour="darkred"))+
  geom_text_repel(aes(x=PA1, y=PA2),label=rownames(datos.grafico))+
  geom_vline(xintercept = 0,colour="darkgray")+
  geom_hline(yintercept = 0,colour="darkgray")+
  labs (x="Dimensión 1 (37%)", y="Dimensión 2 (10%)")+
  theme(legend.position="none")+
  expand_limits(x=c(0,1), y=c(-0.5, 0.5))+
  geom_abline(intercept = 0, slope = tan(0.66624975),linetype="dashed",colour="darkred")+
  geom_abline(intercept = 0, slope = tan(0.66624975+pi/2),linetype="dashed",colour="darkred")+
  theme(aspect.ratio=1)

# Ilustramos la rotacion con la matriz

T<-matrix(c(0.7861450, -0.6180421,0.6180421,0.7861450),2,2)
L<-matrix(c(0.59,0.59,0.43,0.71,0.70,0.58,0.38,0.24,0.41,-0.33,-0.28,-0.18),6,2)

LR<-L%*%T


# =======================================
# Rotación de los ejes OBLICUA
# =======================================

PA1<-c(0.586,0.594,0.431,0.711,0.702,0.584)
PA2<-c(0.377,0.236,0.413,-0.333,-0.278,-0.184)

datos.grafico<-data.frame(PA1,PA2)
rownames(datos.grafico)<-rownames(R)

# grafico ejes originales
ggplot(datos.grafico)+
  geom_point(aes(x=PA1, y=PA2,colour="darkred"))+
  geom_text_repel(aes(x=PA1, y=PA2),label=rownames(datos.grafico))+
  geom_vline(xintercept = 0,colour="darkgray")+
  geom_hline(yintercept = 0,colour="darkgray")+
  labs (x="Dimensión 1 (37%)", y="Dimensión 2 (10%)")+
  theme(legend.position="none")+
  expand_limits(x=c(0,1), y=c(-0.5, 0.5))+
  theme(aspect.ratio=1)

# estimamos otra vez por PAF para tener la solución rotada
fit.pa<-fa(R,nfactors=2,fm="pa",rotate="oblimin",n.obs=220)
fit.pa$rot.mat

#de la matriz de rotación obtenida vemos que el ángulo de rotación
# es cos(theta)=0.7861450-->theta=0.66624975


# grafico ejes rotados
ggplot(datos.grafico)+
  geom_point(aes(x=PA1, y=PA2,colour="darkred"))+
  geom_text_repel(aes(x=PA1, y=PA2),label=rownames(datos.grafico))+
  geom_vline(xintercept = 0,colour="darkgray")+
  geom_hline(yintercept = 0,colour="darkgray")+
  labs (x="Dimensión 1 (37%)", y="Dimensión 2 (10%)")+
  theme(legend.position="none")+
  expand_limits(x=c(0,1), y=c(-0.5, 0.5))+
  geom_abline(intercept = 0, slope = tan(0.66624975),linetype="dashed",colour="darkred")+
  geom_abline(intercept = 0, slope = tan(-0.4),linetype="dashed",colour="darkred")+
  theme(aspect.ratio=1)

# =======================================
# Medidas de ajuste o de adecuación muestral
# =======================================

cortest.bartlett(R,n=220)
KMO(R)

# calculo de la matriz reproducida para PAF y varimax

LAMBDA<-matrix(c(0.23,0.32,0.08,0.76,0.72,0.57,0.66,0.55,0.59,0.18,0.22,0.22),6,2)
Rrep<-LAMBDA%*%t(LAMBDA)
Rres<-R-Rrep

#Calculo del SRMR OJO! psych solo tiene en cuenta los residuos fuera de la diagonal
residuos<- fit.pa$residual[row(fit.pa$residual)!=col(fit.pa$residual)] # solo fuera diagonal

SRMR<-sqrt(mean(residuos^2))
SRMR

# =======================================
# Puntuaciones factoriales (solo coeficientes)
# =======================================

factor.scores(R,fit.pca,method="Thurstone")

