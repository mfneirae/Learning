#======================
# Carga de datos
#======================

library(haven)
Datos_6_1_Ilustracion <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Ilustracion6_1.sav")

datos<-Datos_6_1_Ilustracion


#======================
# Carga de librerías
#======================


library(ggplot2)      # Gráficos

#========================
# etiquetado grupos
#========================

datos$niveduc1 <- factor(datos$niveduc1,
                       levels = c(0,1,2,3,4),
                       labels = c("Sin Estudios", "Primarios", "Secundarios", "Grado", "Master"))
datos$sexo1 <- factor(datos$sexo1,
                       levels = c(1,2),
                       labels = c("Hombre", "Mujer"))

datos$niveduc2 <- factor(datos$niveduc2,
                        levels = c(0,1,2,3,4),
                        labels = c("Sin Estudios", "Primarios", "Secundarios", "Grado", "Master"))
datos$sexo2 <- factor(datos$sexo2,
                     levels = c(1,2),
                     labels = c("Hombre", "Mujer"))
datos$niveduc3 <- factor(datos$niveduc3,
                        levels = c(0,1,2,3,4),
                        labels = c("Sin Estudios", "Primarios", "Secundarios", "Grado", "Master"))
datos$sexo3 <- factor(datos$sexo3,
                     levels = c(1,2),
                     labels = c("Hombre", "Mujer"))
datos$niveduc4 <- factor(datos$niveduc4,
                        levels = c(0,1,2,3,4),
                        labels = c("Sin Estudios", "Primarios", "Secundarios", "Grado", "Master"))
datos$sexo4 <- factor(datos$sexo4,
                     levels = c(1,2),
                     labels = c("Hombre", "Mujer"))
datos$niveduc5 <- factor(datos$niveduc5,
                        levels = c(0,1,2,3,4),
                        labels = c("Sin Estudios", "Primarios", "Secundarios", "Grado", "Master"))
datos$sexo5 <- factor(datos$sexo5,
                     levels = c(1,2),
                     labels = c("Hombre", "Mujer"))



#========================
# gráficos descriptivos
#========================

#Ningún efecto
ggplot(datos, aes(x=niveduc1, y=horas1))+
  stat_summary(fun.y=mean,geom="point",aes(group=sexo1))+
  stat_summary(fun.y = mean, geom = "line",aes(group= sexo1,linetype=sexo1)) + 
  labs(x = "Nivel educativo", y = "Número de horas de televisión")+
  expand_limits(y=c(0, 7))

# Nivel estudios significativo, Sexo no
ggplot(datos, aes(x=niveduc2, y=horas2))+
  stat_summary(fun.y=mean,geom="point",aes(group=sexo2))+
  stat_summary(fun.y = mean, geom = "line",aes(group= sexo2,linetype=sexo2)) + 
  labs(x = "Nivel educativo", y = "Número de horas de televisión")+
  expand_limits(y=c(0, 7))

# Sexo significativo, nivel estudios no
ggplot(datos, aes(x=niveduc3, y=horas3))+
  stat_summary(fun.y=mean,geom="point",aes(group=sexo3))+
  stat_summary(fun.y = mean, geom = "line",aes(group= sexo3,linetype=sexo3)) + 
  labs(x = "Nivel educativo", y = "Número de horas de televisión")+
  expand_limits(y=c(0, 7))

# Sexo significativo, nivel estudios significativo
ggplot(datos, aes(x=niveduc4, y=horas4))+
  stat_summary(fun.y=mean,geom="point",aes(group=sexo4))+
  stat_summary(fun.y = mean, geom = "line",aes(group= sexo4,linetype=sexo4)) + 
  labs(x = "Nivel educativo", y = "Número de horas de televisión")+
  expand_limits(y=c(0, 7))

# Todos efectos significativos
ggplot(datos, aes(x=niveduc5, y=horas5))+
  stat_summary(fun.y=mean,geom="point",aes(group=sexo5))+
  stat_summary(fun.y = mean, geom = "line",aes(group= sexo5,linetype=sexo5)) + 
  labs(x = "Nivel educativo", y = "Número de horas de televisión")+
  expand_limits(y=c(0, 7))



