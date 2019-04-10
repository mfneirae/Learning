
#===========================
# Cargamos librerías
#===========================
library(ggplot2)
library(ggrepel)
library(factoextra)
library(psych)
library(FactoMineR)



#===========================
# Datos Sharma (1996), p.59
#===========================

# Datos originales
x1<-c(16,12,13,11,10,9,8,7,5,3,2,0)
x2<-c(8,10,6,2,8,-1,4,6,-3,-1,-3,0)

# Datos centrados
x1_m<-x1-mean(x1)
x2_m<-x2-mean(x2)

# Matriz datos originales
X<-matrix(c(x1,x2),ncol=2,nrow=12)

# Matriz datos centrados
Xm<-matrix(c(x1_m,x2_m),ncol=2,nrow=12)

# SSCP, S y R (datos centrados)
SSCP<-t(Xm) %*% (Xm)
S<-SSCP/(12-1)
R<-cov2cor(S)


#===========================
# Figura 11.1
#===========================



library(ggplot2)
library(ggrepel)
caso<-c(rep(1:12))
xb=x1_m*cos(10*pi/180)+x2_m*sin(10*pi/180) #proyección sobre x1*
xb1=xb*cos(10*pi/180) #coordenada x de la proyección sobre eje original
xb2=xb*sin(10*pi/180) #coordenada y de la proyección sobre eje original

datos.grafico<-data.frame(Xm,caso,xb,xb1,xb2)

ggplot(datos.grafico)+
  geom_point(aes(x=x1_m, y=x2_m))+
  geom_point(aes(x=xb1, y=xb2),colour="darkred")+
  geom_text(aes(x=x1_m,y=x2_m,label=caso,hjust=0.5, vjust = -0.7))+
  geom_text_repel(aes(x=xb1,y=xb2,label=caso,colour="darkred"))+
  expand_limits(x=c(-10,10), y=c(-10, 10))+
  geom_abline(intercept = 0, slope = tan(10*pi/180),linetype="dashed",colour="darkred")+
  geom_vline(xintercept = 0,colour="darkgray")+
  geom_hline(yintercept = 0,colour="darkgray")+
  labs (x="X1 (centrada)", y="X2 (centrada)")+
  theme(legend.position="none")+
  theme(aspect.ratio=1)

#===========================
# Figura 11.2
#===========================

a<-0
b<-0
c<-0

for (i in 1:91) 
{
  a[i]<-var(x1_m*cos((i-1)*pi/180)+x2_m*sin((i-1)*pi/180))
  b[i]<-(i-1)
  c[i]<-(a[i]/44.182)*100
}


datos.grafico2<-data.frame(cbind(a,b,c))
colnames(datos.grafico2)<-c("Varianza","Angulo","Porcentaje")

ggplot(datos.grafico2)+
  geom_line (aes(x=Angulo, y=Porcentaje))+
  expand_limits(x=c(0,100))+
  labs (x="Angulo de X* frente a X", y="Porcentaje")+
  theme(legend.position="none")+
  theme(aspect.ratio=1)+
  geom_vline(xintercept = 43.261,linetype="dashed")+
  geom_point(x=43.261,y=87.312,colour="darkred",size=2)+
  annotate(geom="text", x=60, y=90, label="Varianza=87.31%",
         color="darkred",size=3)+
  annotate(geom="text", x=60, y=50, label="Ángulo=43.26º",
           color="darkred",size=3)


#===========================
# Extracción del segundo CP
#===========================

# Calculo de los 2 componentes principales finales
cp1=x1_m*cos(43.261*pi/180)+x2_m*sin(43.261*pi/180) #proyección sobre x1*
cp2=x1_m*(-sin(43.261*pi/180))+x2_m*cos(43.261*pi/180) #proyección sobre x2*

mean(cp1) 
mean(cp2) 
var(cp1) 
var(cp2)

Xcp<-matrix(c(cp1,cp2),ncol=2,nrow=12)

# SSCP, S y R (datos centrados)
SSCP2<-t(Xcp) %*% (Xcp)
S2<-SSCP2/(12-1)
R2<-cov2cor(S2)


#===========================
# Figura 11.3
#===========================



library(ggplot2)
library(ggrepel)
caso<-c(rep(1:12))
xb=x1_m*cos(10*pi/180)+x2_m*sin(10*pi/180) #proyección sobre x1*
xb1=xb*cos(10*pi/180) #coordenada x de la proyección sobre eje original
xb2=xb*sin(10*pi/180) #coordenada y de la proyección sobre eje original

datos.grafico3<-datos.grafico

ggplot(datos.grafico3)+
  geom_point(aes(x=x1_m, y=x2_m))+
  geom_text(aes(x=x1_m,y=x2_m,label=caso,hjust=0.5, vjust = -0.7))+
  expand_limits(x=c(-10,10), y=c(-10, 10))+
  geom_abline(intercept = 0, slope = tan(43.261*pi/180),linetype="dashed",colour="darkred")+
  geom_abline(intercept = 0, slope = tan(133.261*pi/180),linetype="dashed",colour="darkred")+
  geom_vline(xintercept = 0,colour="darkgray")+
  geom_hline(yintercept = 0,colour="darkgray")+
  labs (x="X1 (centrada)", y="X2 (centrada)")+
  theme(legend.position="none")+
  theme(aspect.ratio=1)

#===========================
# Estimación del PCA
#===========================


fit<-prcomp(X)
fit2<-PCA(X)



#===========================
# Figura 11.4
#===========================
library(MASS)
mu <- c(0,0)                         # Mean
Sigma <- matrix(c(1, .8, .8, 1), 2)  # Covariance matrix
# > Sigma
# [,1] [,2]
# [1,]  1.0  0.1
# [2,]  0.1  1.0

# Generate sample from N(mu, Sigma)
bivn <- mvrnorm(100, mu = mu, Sigma = Sigma )
colnames(bivn)<-c("x","y")
datos.grafico5<-data.frame(bivn)

ggplot(datos.grafico5)+
  geom_point(aes(x=x, y=y))+
  #geom_text(aes(x=x1_m,y=x2_m,label=caso,hjust=0.5, vjust = -0.7))+
  expand_limits(x=c(-1,1), y=c(-1,1))+
  #geom_abline(intercept = 0, slope = tan(43.261*pi/180),linetype="dashed",colour="darkred")+
  #geom_abline(intercept = 0, slope = tan(133.261*pi/180),linetype="dashed",colour="darkred")+
  geom_vline(xintercept = 0,colour="darkgray")+
  geom_hline(yintercept = 0,colour="darkgray")+
  labs (x="X1 (centrada)", y="X2 (centrada)")+
  theme(legend.position="none")+
  theme(aspect.ratio=1)+
  stat_ellipse(aes(x,y))



#===========================
# Figura 11.5a
#===========================

# Datos internos círculo
r <- runif(50)
t <- 2*pi*runif(50)
a <- r*cos(t) ; b <- r*sin(t)

# Datos trazar círculo externo

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
circulo <- circleFun(c(0,0),2,npoints = 100)

datos.grafico4<-data.frame(cbind(a,b,circulo))

ggplot(datos.grafico4)+
  geom_point(aes(x=a, y=b))+
  #geom_text(aes(x=x1_m,y=x2_m,label=caso,hjust=0.5, vjust = -0.7))+
  expand_limits(x=c(-1,1), y=c(-1,1))+
  #geom_abline(intercept = 0, slope = tan(43.261*pi/180),linetype="dashed",colour="darkred")+
  #geom_abline(intercept = 0, slope = tan(133.261*pi/180),linetype="dashed",colour="darkred")+
  geom_vline(xintercept = 0,colour="darkgray")+
  geom_hline(yintercept = 0,colour="darkgray")+
  labs (x="X1 (centrada)", y="X2 (centrada)")+
  theme(legend.position="none")+
  theme(aspect.ratio=1)+
  geom_path(aes(x,y))

#===========================
# Figura 11.5b
#===========================

correlatedValue = function(x, r){
  r2 = r**2
  ve = 1-r2
  SD = sqrt(ve)
  e  = rnorm(length(x), mean=0, sd=SD)
  y  = r*x + e
  return(y)
}

set.seed(5)
x = rnorm(50)
y = correlatedValue(x=x, r=.99)

datos.grafico5<-data.frame(cbind(x,y))


ggplot(datos.grafico5)+
  geom_point(aes(x=x, y=y))+
  #geom_text(aes(x=x1_m,y=x2_m,label=caso,hjust=0.5, vjust = -0.7))+
  expand_limits(x=c(-1,1), y=c(-1,1))+
  #geom_abline(intercept = 0, slope = tan(43.261*pi/180),linetype="dashed",colour="darkred")+
  #geom_abline(intercept = 0, slope = tan(133.261*pi/180),linetype="dashed",colour="darkred")+
  geom_vline(xintercept = 0,colour="darkgray")+
  geom_hline(yintercept = 0,colour="darkgray")+
  labs (x="X1 (centrada)", y="X2 (centrada)")+
  theme(legend.position="none")+
  theme(aspect.ratio=1)+
  stat_ellipse(aes(x,y))