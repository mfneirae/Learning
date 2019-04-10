#Estimación del caso 16.1 mediante el paquete plspm

library(haven)
datos <- read_sav("~/Dropbox/1_DATOS_PORT/libros/@AMA_R_v2/Datos/Datos_16_1_Caso.sav")
View(Datos_16_1_Caso)

#===================================
# Nombramos al fichero de datos
#===================================
#para llamar siempre al fichero de datos "datos"

library(lavaan)
library(matrixpls)
library(plspm)
library(semPLS)


#===================================
# Matriz de relaciones estructurales
#===================================

facuso  =c(0,0,0,0,0)
utiper  =c(1,0,0,0,0)
dep     =c(1,1,0,0,0)
actitud =c(1,1,1,0,0)
intcomp =c(0,1,1,1,0)

# creamos la matriz fusionando filas
modelo.path = rbind(facuso,utiper,dep,actitud,intcomp)

# ponemos nombres a las columnas (optional)
colnames(modelo.path) = rownames(modelo.path)


#===================================
# Matriz de medida
#===================================

# Asociación de indicadores a factores
# El orden de los indicadores es el de la base de datos
# El orden de los factores en que hemos definido en la matriz

modelo.blocks = list(c(7,9,10,12), 1:6, 24:29, c(13,14,16,17,18,19,20,22), 23:23)

# Caracter formativo (B) o reflectivo (A) de los bloques de constructos (ojo uno por constructo)

modelo.modes = c("A","A","B","A","A")

#===================================
# Estimamos el modelo
#===================================
#plspm(Data, path_matrix, blocks, modes = NULL,
#      scaling = NULL, scheme = "centroid", scaled = TRUE,
#      tol = 1e-06, maxiter = 100, plscomp = NULL,
#      boot.val = FALSE, br = NULL, dataset = TRUE)
# boot.val=TRUE indica que queremos que realice un bootstrapping de br=X submuestras

modelo.pls = plspm(datos, modelo.path, modelo.blocks, modes = modelo.modes,
                   scheme="path", boot.val=TRUE, br=200,tol = 1e-06, maxiter = 100)

summary(modelo.pls)


#===================================
# Graficos del modelo
#===================================

#Estructural
plot(modelo.pls, arr.width = 0.1)

#Medida
plot(modelo.pls, what = "loadings", arr.width = 0.1)

#===================================
# Información adicional
#===================================

#Valores de las variables latentes
print(modelo.pls$scores) 

#======================================
# Diagnóstico colinealidad formativos
#======================================

library(car)

colinealidad <- lm(facuso1~dep1+dep2+dep3+dep4+dep5+dep6, data=datos)
vif(colinealidad)

#======================================
# Potencia de las regresiones
#======================================

# N= (tamaño muestral) 
# u = número de variables independientes de la mayor regresión
# V = N -u -1
# f2 = 0.15 (efecto medio)
# sig.level es elnivel de significación, normalmente 0.05

#En nuestro ejemplo tenemos 464 y la regresión más complicada es la del constructo formativo
#con 6 regresores por lo que v=464-6-1=457. Se deja como NULL lo que se quiere calcular

library(pwr)
pwr.f2.test(u =6, v = 457 , f2 =0.15 , sig.level = 0.05, power = NULL)

#Si quisiéramos saber la muestra necesaria para alcanzar una potencia del 80%

library(pwr)
pwr.f2.test(u =6, v = NULL , f2 =0.15 , sig.level = 0.05, power = 0.80)

