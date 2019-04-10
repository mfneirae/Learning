library(ca)
library(FactoMineR)
library(factoextra)
library(gplots)
library(graphics)
library(ggplot2)

#=============================
# Cargamos datos
#=============================
# Cargamos datos tea del paquete FactoMinerR
library(haven)
Datos_5_2_Caso <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_5_2_Caso.sav")

datos<-data.frame(Datos_5_2_Caso)

for (i in 1:ncol(datos)) datos[,i]=as.factor(datos[,i])


#=============================
# Estimación mjca{CA}
#=============================
fit <- mjca(datos,graph=TRUE,method="adjusted")

fit
summary(fit)


#=============================
# Gráfico
#=============================


cats = apply(datos, 2, function(x) nlevels(as.factor(x)))

fit_vars_df = data.frame(fit$colcoord, Variable = rep(names(cats), cats))
rownames(fit_vars_df) = fit$levelnames

# plot
ggplot(data = fit_vars_df, 
       aes(x = X1, y = X2, label = rownames(fit_vars_df),shape=Variable,colour=Variable)) +
  geom_point(size=2)+
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  scale_colour_brewer(palette="Set1")+
  geom_text(check_overlap = F,hjust=0.5, vjust = -0.7, nudge_y = 0.0, nudge_x = 0.0)+
  ggtitle("")+
  expand_limits(x=c(-2.5,2), y=c(-2.5, 2.5))+
  labs (x="Dimension 1", y="Dimension 2")+
  guides(colour=FALSE, shape=FALSE)+
  theme_gray()


#=============================
# Estimación MCA{FactoMineR}
#=============================

fit2<-MCA(datos, ncp=5, graph=FALSE)
fit2
summary(fit2, nb.dec=3, ncp=2, nbelements=Inf)


# Descriptivos
# ============


for (i in 1:ncol(datos)) {
  plot(datos[,i], main=colnames(datos)[i],
       ylab = "Frecuencia", col="grey", las = 2)
}


# Screeplot
# ==========

fviz_screeplot(fit2,geom="line")+
  theme_grey()


# Gráfico conjunto con individuos
# ===============================


fviz_mca_biplot(fit2) +
  ggtitle("")+
  expand_limits(x=c(-1.5,2), y=c(-1.5, 1.5))+
  labs (x="Dimension 1", y="Dimension 2")+
  guides(colour=FALSE, shape=FALSE)+
  theme_gray()

# Extraemos variables para gráficos adicionales
# =============================================
var <- get_mca_var(fit2)
print(var)

# Gráfico de correlacioens variables-ejes

plot(fit2, choix = "var")

# Gráfico de biplot solo con variables sin individuos

fviz_mca_var(fit2, col.var="black", shape.var = 15)+
  ggtitle("")+
  expand_limits(x=c(-1.5,2), y=c(-1.5, 1.5))+
  labs (x="Dimension 1", y="Dimension 2")+
  guides(colour=FALSE, shape=FALSE)+
  theme_gray()

# Contribución de las variables a las dimensiones
# =============================================
fviz_contrib(fit2, choice = "var", axes = 1)
fviz_contrib(fit2, choice = "var", axes = 2)
fviz_contrib(fit2, choice = "var", axes = 1:2)

fviz_mca_var(fit2, col.var = "contrib")+
  expand_limits(x=c(-1.5,2), y=c(-1.5, 1.5))+
  labs (x="Dimension 1", y="Dimension 2")+
  guides(shape=FALSE)+
  theme_gray()

# Calidad de la representación de las variables
# =============================================

fviz_cos2(fit2, choice = "var", axes = 1)
fviz_cos2(fit2, choice = "var", axes = 2)
fviz_cos2(fit2, choice = "var", axes = 1:2)

# Representación de loS individuos separados por
# su intención de volver
# =============================================

fviz_mca_ind(fit2,label="none",habillage=datos$rep)+
  ggtitle("")+
  expand_limits(x=c(-1.5,2), y=c(-1.5, 1.5))+
  labs (x="Dimension 1", y="Dimension 2")+
  guides(shape=FALSE)+
  theme_gray()


# Representación de loS individuos separados por
# su intención de volver junto a las variables
# =============================================

fviz_mca_biplot(fit2,habillage=datos$rep,shape.var=15, label="var")+
  ggtitle("")+
  expand_limits(x=c(-1.5,2.0), y=c(-1.5, 1.5))+
  labs (x="Dimension 1", y="Dimension 2")+
  guides(shape=FALSE)+
  theme_gray()

# Explicación de las dimensiones
# =============================================


res.desc <- dimdesc(fit2, axes = c(1,2))
res.desc$`Dim 1`
res.desc$`Dim 2`


