library(magrittr)
library(ggplot2)
library(dplyr)            #librerias
library(ggpubr)
library(corrplot)
library(readxl)

#leer datos Guerrero
setwd("~/Actuaria Anahuac/7mo semestre/Practica estadistica")

DatosGuerrero <- read_excel("DatosGuerrero.xlsx",
                            sheet = "Guerrero",range="b1:O24")
#Asignar los nombres de las filas
matrizdatos<-as.matrix(DatosGuerrero)
row.names(matrizdatos)<-c(1997:2019)

# Generar el algoritmo Multimensional Scaling
mds <- matrizdatos %>%
  dist() %>%          
  cmdscale() %>%
  as.data.frame()
colnames(mds) <- c("Dim.1Guerrero", "Dim.2Guerrero")
# Plotear las dos dimensiones
ggscatter(mds, x = "Dim.1Guerrero", y = "Dim.2Guerrero", 
          label = rownames(matrizdatos),
          repel = TRUE)

#matriz de distancias
a<-summary(cmdscale(matrizdmatrizdatos))
distG<-dist(matrizdatos)
View(as.matrix(distG))

#matriz de correlacion
corrplot(cor(matrizdatos),method="circle",type="upper",order="hclust")

#lo mismo con Yucatan
Datosyucatan <- read_excel("DatosGuerrero.xlsx",
                           sheet = "Yucatan",range="b1:O24")
#Asignar los nombres de las filas
matrizdatosy<-as.matrix(Datosyucatan)
row.names(matrizdatosy)<-c(1997:2019)

# Generar el algoritmo Multimensional Scaling
mdsy <- matrizdatosy %>%
  dist() %>%          
  cmdscale() %>%
  as.data.frame()
colnames(mdsy) <- c("Dim.1yucatan", "Dim.2yucatan")
# Plotear las dos dimensiones
ggscatter(mdsy, x = "Dim.1yucatan", y = "Dim.2yucatan", 
          label = rownames(matrizdatosy),
          repel = TRUE)

#matriz de distancias
distG<-dist(matrizdatosy)
View(as.matrix(distG))

#matriz de correlacion
corrplot(cor(matrizdatosy),method="circle",type="upper",order="hclust")
