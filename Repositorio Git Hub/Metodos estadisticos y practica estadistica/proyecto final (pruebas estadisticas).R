library(nortest)
nacimientos<-read.csv(file.choose())
attach(nacimientos)
defunciones<-read.csv(file.choose())
attach(defunciones)
#procederemos a verificar normalidad en las variables

#filtramos los datos
nacfilt<-subset(nacimientos,edo_captura=="DISTRITO FEDERAL" & sexo_nac_vivo!="SIN INFORMACIÃ“N" & talla_nac_vivo<98 & peso_nac_vivo<9000)
nacfilth<-subset(nacimientos,edo_captura=="DISTRITO FEDERAL" & sexo_nac_vivo=="HOMBRE" & talla_nac_vivo<98 & peso_nac_vivo<9000)
nacfiltm<-subset(nacimientos,edo_captura=="DISTRITO FEDERAL" & sexo_nac_vivo=="MUJER" & talla_nac_vivo<98 & peso_nac_vivo<9000)
peso_niñas<-nacfiltm$peso_nac_vivo
peso_niños<-nacfilth$peso_nac_vivo
talla_niños<-nacfilth$talla_nac_vivo
talla_niñas<-nacfiltm$talla_nac_vivo

#aplicaremos las pruebas para probar normalidad en el peso de las niñas
lillie.test(peso_niñas)
lillie.test(peso_niñas[peso_niñas>quantile(peso_niñas,.01) & peso_niñas<quantile(peso_niñas,.99)])
lillie.test(peso_niñas[peso_niñas>quantile(peso_niñas,.05) & peso_niñas<quantile(peso_niñas,.95)])
lillie.test(peso_niñas[peso_niñas>quantile(peso_niñas,.10) & peso_niñas<quantile(peso_niñas,.90)])

#aplicaremos las pruebas para probar normalidad en el peso de los niños
lillie.test(peso_niños)
lillie.test(peso_niños[peso_niños>quantile(peso_niños,.01) & peso_niños<quantile(peso_niños,.99)])
lillie.test(peso_niños[peso_niños>quantile(peso_niños,.05) & peso_niños<quantile(peso_niños,.95)])
lillie.test(peso_niños[peso_niños>quantile(peso_niños,.10) & peso_niños<quantile(peso_niños,.90)])

#aplicaremos las pruebas para probar normalidad en la talla de las niñas
lillie.test(talla_niñas)
lillie.test(talla_niñas[talla_niñas>quantile(talla_niñas,.01) & talla_niñas<quantile(talla_niñas,.99)])
lillie.test(talla_niñas[talla_niñas>quantile(talla_niñas,.05) & talla_niñas<quantile(talla_niñas,.95)])
lillie.test(talla_niñas[talla_niñas>quantile(talla_niñas,.10) & talla_niñas<quantile(talla_niñas,.90)])

#aplicaremos las pruebas para probar normalidad en la talla de las niños
lillie.test(talla_niños)
lillie.test(talla_niños[talla_niños>quantile(talla_niños,.01) & talla_niños<quantile(talla_niños,.99)])
lillie.test(talla_niños[talla_niños>quantile(talla_niños,.05) & talla_niños<quantile(talla_niños,.95)])
lillie.test(talla_niños[talla_niños>quantile(talla_niños,.10) & talla_niños<quantile(talla_niños,.90)])

#1
wilcox.test(peso_niñas,mu=3500)
hist(peso_niñas,xlab="Peso",ylab="Histograma",main="Peso de las niñas recién nacidas", col="pink")
mean(peso_niñas)

#2
lillie.test(peso_niñas)
plot(density(peso_niñas),xlab="Peso",ylab="Densidad",main="Peso de las niñas recién nacidas", col="pink3")

#3 
wilcox.test(peso_niñas,peso_niños)
plot(density(peso_niñas),xlab="Peso",ylab="Densidad",main="Peso", col="pink3")
lines(density(peso_niños),col="blue3")


#4

leveneTest(peso_nac_vivo~ sexo_nac_vivo,data=nacfilt)
boxplot(peso_niñas, peso_niños,
        main = "Boxplot del peso de niñas y niños",
        at = c(1,2),
        names = c("niñas","niños"),
        col = c("pink","blue"),
        notch = TRUE
)

#5
wilcox.test(talla_niños,mu=47)
hist(talla_niños,xlab="Talla",ylab="Histograma",main="Talla de los niños recién nacidos", col="blue")

#6
lillie.test(talla_niños)
plot(density(talla_niños),xlab="Talla",ylab="Densidad",main="Talla de los niños recién nacidos", col="blue3")

#7
wilcox.test(talla_niñas,talla_niños)
plot(density(talla_niñas),xlab="Talla",ylab="Densidad",main="Talla", col="pink3")
lines(density(talla_niños),col="blue3")

#8
leveneTest(talla_nac_vivo~ sexo_nac_vivo,data=nacfilt)
boxplot(talla_niñas, talla_niños,
        main = "Boxplot de la talla de niñas y niños",
        at = c(1,2),
        names = c("niñas","niños"),
        col = c("pink","blue"),
        notch=TRUE
)

#9
ks.test(talla_niñas,talla_niños)
plot(ecdf(talla_niñas),xlab="Talla",ylab="Distribución",main="Distribución empírica de talla",col="pink3")
lines(ecdf(talla_niños),col="blue3")
#10
ks.test(peso_niñas,peso_niños)
plot(ecdf(peso_niñas),xlab="Peso",ylab="Distribución",main="Distribución empírica de peso",col="pink")
lines(ecdf(peso_niños),col="blue")

#defunciones
decfilt<-subset(defunciones,ENT_REGIS==9 & EDO_CIVIL!=8 & EDO_CIVIL!=9 & EDAD>=4000 & EDAD<=4120)

#verificamos normalidad
lillie.test(decfilt$EDAD)

#11
wilcox.test(decfilt$EDAD,mu=4040,alternative="less")
hist(decfilt$EDAD,xlab="Edad",ylab="Histograma",main="Defunciones", col="red")

#12
lillie.test(decfilt$EDAD)
plot(density(decfilt$EDAD),xlab="Edad",ylab="Densidad",main="Defunciones", col="red")

#13
defh<-subset(decfilt,SEXO==1)
defm<-subset(decfilt,SEXO==2)
wilcox.test(defh$EDAD,defm$EDAD,alternative="greater")
boxplot(defh$EDAD,defm$EDAD,
        main = "Boxplot de edades por defunciones",
        names = c("Hombre","Mujer"),
        col = c("blue","pink")
)


#14
wilcox.test(EDAD~ SEXO,data=decfilt)
plot(density(defh$EDAD),xlab="Edad",ylab="Densidad",main="Defunciones", col="blue3")
lines(density(defm$EDAD),col="pink3")


#15
kruskal.test(EDAD~ EDO_CIVIL,data=decfilt)
boxplot(EDAD~ EDO_CIVIL,data=decfilt,
        main = "Boxplot de edades por estado civil",
        names = c("solter@","divorciad@","viud@","Union libre","Casad@","Separad@"),
        col = c("blue","pink","red","forestgreen","green","gold"),
        xlab="Estado Civil",
        las=2,
        horizontal=TRUE
)

