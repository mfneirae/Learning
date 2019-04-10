#======================
# Carga de datos
#======================

library(haven)
Datos_6_1_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_6_1_Caso.sav")

datos<-Datos_6_1_Caso

# Para cuando el test necesita los datos como vector y no data.frame

resultado2<-c(6,7,8,7,9,8,9,8,9,9,8,7,8,9)
metodo2<-c(1,1,1,1,2,2,2,2,2,3,3,3,3,3)


#======================
# Carga de librerías
#======================

library(car)          # Para el test de Levene
library(compute.es)   # Para el tamaño del los efectos
library(ggplot2)      # Gráficos
library(multcomp)     # Pruebas post hoc
library(WRS2)         # Test robustos
library(pastecs)      # Descriptivos
library(onewaytests)  # Para test de Brown-Forsythe, Welch y Kruskal-Wallis

#======================
# Estimación del ANOVA
#======================

fit<-aov(data=datos, resultado~metodo)
summary(fit)
plot(fit)

#======================
# Test de Levene
#======================

library(car) 
leveneTest(datos$resultado,datos$metodo,center=mean)


#=========================
# Test de Welch
#=========================

welch.test(resultado2,metodo2)

#=========================
# Test de Brown-Forsythe
#=========================

bf.test(resultado2,metodo2)

#=========================
# Test de Kruskal-Wallis
#=========================

kw.test(resultado2,metodo2)

#=========================
# Pruebas post hoc
#=========================

# Basados en bonferroni
pairwise.t.test(resultado2,metodo2,p.adjust="bonferroni")
pairwise.t.test(resultado2,metodo2,p.adjust="holm")

# Tukey

posthoc.tukey<-TukeyHSD(fit)
print(posthoc.tukey)
plot(posthoc.tukey)

# bootstrapping y medias recortadas
library(DTK)
resultado2<-c(6,7,8,7,9,8,9,8,9,9,8,7,8,9)
metodo2<-gl.unequal(n=3,k=c(4,5,5))
datos2<-data.frame(resultado2,metodo2)

lincon(datos2)
mcppb20(datos2)
