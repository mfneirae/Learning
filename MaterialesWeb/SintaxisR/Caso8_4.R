#=============================
# Librerias
#=============================

library(haven)
library(stargazer)


#=============================
# Librerias
#=============================

Datos_8_4_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_8_4_Caso.sav")
datos<-Datos_8_4_Caso

#=============================
# Estimacion
#=============================

fit.g<-lm(log(salario)~educ+exper+antigue+edad,data=datos)
fit.r<-lm(log(salario)~educ+exper,data=datos)
stargazer(fit.g,fit.r,type="text")

aov.g<-anova(fit.g)
aov.r<-anova(fit.r)

F<-((143.979-139.486)/2)/(139.486/(935-1-4))
qf(0.99,2,930)

