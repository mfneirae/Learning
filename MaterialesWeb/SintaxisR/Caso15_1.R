#==============================
# Cargamos librerías
#==============================

library(sem)
#==============================
# Cargamos datos Bagozzi (1980)
#==============================


datos <- readMoments(diag=TRUE, 
                       names=c("x1", "x2", "x3","x4", "x5", "x6", "x7", "x8"))  
  1.000
  0.418   1.000
  0.394   0.627   1.000
  0.129   0.202   0.266   1.000
  0.189   0.284   0.208   0.365   1.000
  0.544   0.281   0.324   0.201   0.161   1.000
  0.507   0.225   0.314   0.172   0.174   0.546   1.000
 -0.357  -0.156  -0.038  -0.199  -0.277  -0.294  -0.174  1.000
  
 
#==============================
# Definimos modelo
#==============================
 
modelo<-specifyEquations()
#Medida=======================================
x1=1*desempeno         
x2=1*satisfaccion
x3=lam1*satisfaccion
x4=1*motivacion
x5=lam2*motivacion
x6=1*autoestima
x7=lam3*autoestima
x8=1*iverbal
#Estructural==================================
satisfaccion=beta1*desempeno+gam1*motivacion
desempeno=beta2*satisfaccion+gam2*autoestima+gam3*iverbal
#Varianzas factores============================
V(motivacion)=phi1
V(autoestima)=phi2
V(iverbal)=phi3
#Varianzas errores=============================
V(x1)=0
V(x2)=the22
V(x3)=the33
V(x4)=the44
V(x5)=the55
V(x6)=the66
V(x7)=the77
V(x8)=0
#Varianzas Disturbances=======================
V(desempeno)=psi1
V(satisfaccion)=psi2
#Covarianzas error relación no recursiva======
C(desempeno,satisfaccion)=psi12
#Covarianzas factores=========================
C(motivacion,autoestima)=phi12
C(motivacion,iverbal)=phi13
C(autoestima,iverbal)=phi23


opt<-options(fit.indices = c("GFI", "AGFI", "RMSEA", "NFI", "NNFI", "CFI", "RNI", 
                             "IFI", "SRMR", "AIC", "AICc", "BIC", "CAIC"))

fit<-sem(modelo,datos,N=122,warn=TRUE,standardized=TRUE)
summary(fit)


#==============================
# Indicadores de ajuste
#==============================

standardizedCoefficients(fit)

#==============================
# Residuos
#==============================


a<-round(standardized.residuals(fit),4)
b<-c(a[lower.tri(a)])

qplot(b,
      geom="histogram",
      binwidth = 0.01,  
      main = "Gráfico de residuos", 
      xlab = "Tamaño del residuo",
      ylab = "frecuencia",
      breaks=seq(-0.5,0.5,by=0.05),
      fill=I("black"), 
      col=I("white"), 
      alpha=I(.7),
      xlim=c(-0.5,0.5))

#==============================
# Índices de modificación
#==============================

MI<-modIndices(fit)
print(MI,n.largest=5,round=3)


#==============================
# Generamos el gráfico
#==============================

library(DiagrammeR)
path.diagram(fit,style="ram",standardized=TRUE)
