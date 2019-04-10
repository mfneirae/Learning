#======================
# Carga de datos
#======================

library(haven)
Datos_6_2_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_6_2_Caso.sav")

datos<-Datos_6_2_Caso


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

datos$habito <- factor(datos$habito,
                       levels = c(1,2,3),
                       labels = c("Fuma", "Ha dejado fumar", "No fuma"))

# Para "fumar perjudica la salud"
ddply(datos, c("habito"), summarise,
      N    = length(opinion_perjudica),
      mean = mean(opinion_perjudica),
      sd   = sd(opinion_perjudica),
      se   = sd / sqrt(N)
)

# Para "Deben subirse los impuestos sobre el tabaco"
ddply(datos, c("habito"), summarise,
      N    = length(opinion_impuestos),
      mean = mean(opinion_impuestos),
      sd   = sd(opinion_impuestos),
      se   = sd / sqrt(N)
)




#========================
# gráficos descriptivos
#========================

# Para "fumar perjudica la salud"
ggplot(datos,aes(habito,opinion_perjudica))+
  stat_summary(fun.y=mean,geom="point")+
  stat_summary(fun.y=mean,geom="line",aes(group=1),linetype="dashed")+
  stat_summary(fun.data=mean_cl_boot,geom="errorbar",width=0.1)+
  labs(x="Hábito", y="Opinión-Fumar perjudica salud")

# Para "Deben subirse los impuestos sobre el tabaco"
ggplot(datos,aes(habito,opinion_impuestos))+
  stat_summary(fun.y=mean,geom="point")+
  stat_summary(fun.y=mean,geom="line",aes(group=1),linetype="dashed")+
  stat_summary(fun.data=mean_cl_boot,geom="errorbar",width=0.1)+
  labs(x="Hábito", y="Opinión-Deben subirse impuestos tabaco")

#======================
# Test de Levene
#======================

# Para "fumar perjudica la salud"
leveneTest(datos$opinion_perjudica,datos$habito,center=mean)

# Para "Deben subirse los impuestos sobre el tabaco"
leveneTest(datos$opinion_impuestos,datos$habito,center=mean)

#========================================
# Test de Shapiro y de Kolmogorov-Smirnov
#=========================================

# Para "fumar perjudica la salud"
shapiro.test(datos$opinion_perjudica)
lillie.test(datos$opinion_perjudica)

# Para "Deben subirse los impuestos sobre el tabaco"
shapiro.test(datos$opinion_impuestos)
lillie.test(datos$opinion_impuestos)

#======================
# Estimación del ANOVA
#======================

# Para "fumar perjudica la salud"
fit1<-aov(data=datos, opinion_perjudica~habito)
summary(fit1)
plot(fit1)

# Para "Deben subirse los impuestos sobre el tabaco"
fit2<-aov(data=datos, opinion_impuestos~habito)
summary(fit2)
plot(fit2)



#=========================
# Test de Welch
#=========================

# Para "fumar perjudica la salud"
welch.test(datos$opinion_perjudica,datos$habito)

# Para "Deben subirse los impuestos sobre el tabaco"
welch.test(datos$opinion_impuestos,datos$habito)


#=========================
# Test de Brown-Forsythe
#=========================

# Para "fumar perjudica la salud"
bf.test(datos$opinion_perjudica,datos$habito)

# Para "Deben subirse los impuestos sobre el tabaco"
bf.test(datos$opinion_impuestos,datos$habito)


#=========================
# Test de Kruskal-Wallis
#=========================

# Para "fumar perjudica la salud"
kw.test(datos$opinion_perjudica,datos$habito)

# Para "Deben subirse los impuestos sobre el tabaco"
kw.test(datos$opinion_impuestos,datos$habito)



#=========================
# Pruebas post hoc
#=========================

# Basados en bonferroni
# Para "fumar perjudica la salud" (no tiene sentido medias iguales)

# Para "Deben subirse los impuestos sobre el tabaco"
pairwise.t.test(datos$opinion_impuestos,datos$habito,p.adjust="bonferroni")
pairwise.t.test(datos$opinion_impuestos,datos$habito,p.adjust="holm")

# Tukey

posthoc.tukey<-TukeyHSD(fit2)
print(posthoc.tukey)
plot(posthoc.tukey)


# bootstrapping y medias recortadas
library(DTK)

metodo2<-gl.unequal(n=3,k=c(107,14,120))
datos2<-data.frame(datos$opinion_impuestos,datos$habito)

lincon(datos2)
mcppb20(datos2)
