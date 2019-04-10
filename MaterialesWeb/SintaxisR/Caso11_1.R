#===========================
# Lectura datos
#===========================

library(haven)
Datos_11_1_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_11_1_Caso.sav")

datos<-Datos_11_1_Caso

#===========================
# Cargamos librerías
#===========================

library(ggplot2)
library(ggrepel)
library(factoextra)
library(psych)
library(FactoMineR)
library(pastecs)
library(corrplot)
library(paran)
library(nFactors)

#==============================
# Descriptivos de las variables
#==============================

#Descriptivos
stat.desc(datos,basic=FALSE) 

#Matriz correlacioens

R<-cor(datos[2:10], method="pearson") 

cor.mtest <- function(mat, conf.level = 0.95){
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
  diag(p.mat) <- 0
  diag(lowCI.mat) <- diag(uppCI.mat) <- 1
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
      p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
      lowCI.mat[i,j] <- lowCI.mat[j,i] <- tmp$conf.int[1]
      uppCI.mat[i,j] <- uppCI.mat[j,i] <- tmp$conf.int[2]
    }
  }
  return(list(p.mat, lowCI.mat, uppCI.mat))
}

res1 <- cor.mtest(R,0.95)
corrplot(R, p.mat = res1[[1]], sig.level=0.05,typ="lower",tl.col = "black",tl.cex = 0.7)

#===========================
#  Test de Barlett
#===========================
cortest.bartlett(R,n=24)


#===========================
# Estimamos PCA
#===========================

fit<-PCA(datos[2:10],scale.unit=TRUE,ncp=9,graph=TRUE)
head(fit)

# Scree plot
fviz_eig(fit, geom="line")+
  theme_grey()

# Análisis paralelo (Horn, 1965)
paran(datos[2:10], iterations=5000,graph=TRUE,color=FALSE) 

#Test de Lawley
nBartlett(R, N=24, alpha=0.01, cor=TRUE, details=TRUE)
     
#==================================
# Representación gráfica individuos
#==================================


datos.grafico<-data.frame(fit$ind$coord[,1:2],datos$Pais)
colnames(datos.grafico)<-c("Dim.1","Dim.2","pais")

ggplot(datos.grafico)+
  geom_point(aes(x=Dim.1, y=Dim.2,colour="darkred"))+
  geom_text_repel(aes(x=Dim.1, y=Dim.2),label=datos.grafico$pais)+
  geom_vline(xintercept = 0,colour="darkgray")+
  geom_hline(yintercept = 0,colour="darkgray")+
  labs (x="Dimensión 1 (40.01%)", y="Dimensión 2 (23.46%)")+
  theme(legend.position="none")
  

#==================================
# Representación gráfica variables
#==================================



datos.grafico2<-data.frame(fit$var$coord[,1:2])

ggplot(datos.grafico2)+
  geom_point(aes(x=Dim.1, y=Dim.2,colour="darkred"))+
  geom_text_repel(aes(x=Dim.1, y=Dim.2),label=rownames(datos.grafico2))+
  geom_vline(xintercept = 0,colour="darkgray")+
  geom_hline(yintercept = 0,colour="darkgray")+
  labs (x="Dimensión 1 (40.01%)", y="Dimensión 2 (23.46%)")+
  theme(legend.position="none")


fviz_pca_var(fit, col.var="contrib")+
scale_color_gradient2(low="white", mid="blue",high="red", midpoint=10.0)+
theme_gray()

#============================================
# Representación conjunta variables y objetos
#============================================



library(ggbiplot)

ggbiplot(fit, obs.scale = 1, var.scale = 1,circle=TRUE)+
  scale_color_discrete(name = '')+
  expand_limits(x=c(-8,4), y=c(-2.5, 2.5))+
  labs (x="Dimensión 1 (40.01%)", y="Dimensión 2 (23.46%)")+
  geom_text_repel(aes(x=datos.grafico$Dim.1, y=datos.grafico$Dim.2), label=datos.grafico$pais,size=3)



