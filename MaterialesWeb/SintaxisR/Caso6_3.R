#======================
# Carga de datos
#======================

library(haven)
Datos_6_3_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_6_3_Caso.sav")

datos<-Datos_6_3_Caso




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
library(nortest)      # Para test de Kolmogorov-Smirnov
library(plyr)
library(dplyr)

#========================
# descripción grupos
#========================

datos$niveduc <- factor(datos$niveduc,
                       levels = c(0,1,2,3,4),
                       labels = c("Sin Estudios", "Primarios", "Secundarios", "Grado", "Master"))
datos$sexo <- factor(datos$sexo,
                       levels = c(1,2),
                       labels = c("Hombre", "Mujer"))


ddply(datos, c("niveduc","sexo"), summarise,
      N    = length(tvhoras),
      mean = mean(tvhoras),
      sd   = sd(tvhoras),
      se   = sd / sqrt(N)
)

#========================
# gráficos descriptivos
#========================


ggplot(datos, aes(x=niveduc, y=tvhoras))+
  stat_summary(fun.y=mean,geom="point",aes(group=sexo))+
  stat_summary(fun.y = mean, geom = "line",aes(group= sexo,linetype=sexo)) + 
  labs(x = "Nivel educativo", y = "Número de horas de televisión")




#======================
# Test de Levene
#======================

leveneTest(datos$tvhoras,datos$sexo,center=mean)
leveneTest(datos$tvhoras,datos$sexo,center=mean)

#========================================
# Test de Shapiro y de Kolmogorov-Smirnov
#=========================================


shapiro.test(datos$tvhoras)
lillie.test(datos$tvhoras)


#======================
# Estimación del ANOVA
#======================


fit<-aov(data=datos, tvhoras~niveduc+sexo+niveduc*sexo)
summary(fit)
plot(fit)


#=========================
# Pruebas post hoc
#=========================

# Basados en bonferroni

pairwise.t.test(datos$tvhoras,datos$niveduc,p.adjust="bonferroni")
pairwise.t.test(datos$tvhoras,datos$niveduc,p.adjust="holm")

# Tukey

posthoc.tukey<-TukeyHSD(fit)
print(posthoc.tukey)
plot(posthoc.tukey)


# bootstrapping y medias recortadas
library(DTK)

metodo2<-gl.unequal(n=5,k=c(74,527,87,184,94))
datos2<-data.frame(datos$tvhoras,datos$niveduc)

lincon(datos2)
mcppb20(datos2)
