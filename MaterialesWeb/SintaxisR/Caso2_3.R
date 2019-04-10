library(haven)
Datos_2_3_Caso <- read_sav("MaterialesWeb/Datos/Datos_2_3_Caso.sav")
View(Datos_2_3_Caso)


#Cuadro 2.13 y figura 1.5 =============================================================


mean<-colMeans(Datos_2_3_Caso)                             #Cálculo del vector de medias de las variables
Sx<-cov(Datos_2_3_Caso)                                    #Cálculo de la matriz de covarianzas
D2<-mahalanobis(Datos_2_3_Caso,mean,Sx,inverted = FALSE)   #Cálculo de la D2 de Mahalanobis
                                                           #Como le hemos dado la matriz de covarianzas Sx
                                                           #actúa por defecto el modificador inverted=FALSE
                                                           #si le diéramos Sx ya invertida: inverted=TRUE
print(D2)                                                  #Muestra los valores de la D2
pchisq(D2, df=5, lower.tail=FALSE)                         #df=5 porque hay 5 variables. Muestra la signficatividad
qchisq(.99, df=5)                                          #Calcula el valor crítico

#Cuadro 2.14 ==========================================================================

library(car)
library(psych)
library(pastecs)

round(stat.desc(Datos_2_3_Caso[,c("consumo","motor","cv","peso","acel")],basic=FALSE,norm=TRUE),digits=3)

#Cuadro 2.15 ==========================================================================

qnorm(seq(from=.05,to=.95,by=.1))

#Gráfico 2.9 ==========================================================================

cuantiles_peso<-c(qnorm(seq(from=.05,to=.95,by=.1)))
peso<-c(1144,1145,1149,1168,1231,1283,1437,1447,1451,1475)
datos.grafico29<-data.frame(cbind(peso,cuantiles_peso))

library(ggplot2)
library(ggpmisc)

ggplot(datos.grafico29,(aes(peso,cuantiles_peso)))+
  geom_point()+
  labs(x="Peso coche",y="Cuantiles")+
  geom_smooth(method=lm,alpha=0.4,colour="black",fill="grey",formula=y~x)+
  stat_poly_eq(aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
             formula = y~x, parse = TRUE)
  
#Cuadro 2.17 ==========================================================================

library(stats)
shapiro.test(Datos_2_3_Caso$peso)

library(nortest)
lillie.test(Datos_2_3_Caso$peso)

library(goftest)
cvm.test(Datos_2_3_Caso$peso, "pnorm")
ad.test(Datos_2_3_Caso$peso, "pnorm")


#Cuadro 2.18 ==========================================================================

cuantiles_peso_chiq<-qchisq(seq(from=.05,to=.95,by=.1),5,lower.tail = T)

#La distancia de mahalanobis D2 se ha calculado en esta misma sintaxis en cuadro 2.13
#tienen que estar ordenadas de menor a mayor, le llamamos D2_ord

D2_ord<-sort(D2)

datos.grafico210<-data.frame(cbind(D2_ord,cuantiles_peso_chiq))

#Figura 2.10 ==========================================================================

library(ggplot2)
library(ggpmisc)

ggplot(datos.grafico210,(aes(cuantiles_peso_chiq,D2_ord)))+
  geom_point()+
  labs(x="cuantiles",y="D2")+
  geom_smooth(method=lm,alpha=0.4,colour="black",fill="grey",formula=y~x)+
  stat_poly_eq(aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
               formula = y~x, parse = TRUE)


cor(D2_ord,cuantiles_peso_chiq)


#Cuadro 2.19 ==========================================================================

#Calculo de los test de Mardia, Henze-Zirkler y Royston

library(MVN)
mardiaTest(Datos_2_3_Caso, qqplot = TRUE)
hzTest(Datos_2_3_Caso, qqplot = TRUE)
roystonTest(Datos_2_3_Caso, qqplot = TRUE)

#Figura 2.14==========================================================================
library(graphics)


panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = 2) 
  text(.8, .8, Signif, cex=cex-1.5, col=2) 
}

pairs(data, lower.panel=panel.smooth, upper.panel=panel.cor)




ggcorplot(
  data = data[,c("consumo","motor","cv","peso","acel")],
  var_text_size = 5,
  cor_text_limits = c(5,10))
