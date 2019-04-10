library(haven)

Datos_4_4_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_4_4_Caso.sav")

library(smacof)
datos<-Datos_4_4_Caso
fit <- smacofRect(datos, itmax = 1000)
plot(fit, joint = TRUE, xlim = c(-5, 5), asp=.6)
fit$conf.row
fit$conf.col

fit$stress
fit$spp.row
fit$spp.col


#==============================
#Figura 4.14
#==============================

D1<-c(fit$conf.col[,1],fit$conf.row[,1])
D2<-c(fit$conf.col[,2],fit$conf.row[,2])
tipo<-c("F","F","F","F","F","F","F","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A")
datos.grafico<-data.frame(D1,D2,tipo)

library(ggplot2)
library(ggrepel) #evita solapamientos de etiquetas

ggplot(datos.grafico, aes(x=D1, y=D2,shape=tipo,colour=tipo, label=row.names(datos.grafico))) +
  geom_point(size=2)+
  scale_color_manual(values=c("black","red"))+
  geom_text_repel(nudge_y = 0.0, nudge_x = 0.0, size=3)+
  expand_limits(x=c(-1.0,1.25), y=c(-1, 1))+
  labs (x="Dimension 1", y="Dimension 2")+
  guides(colour=FALSE, shape=FALSE)
