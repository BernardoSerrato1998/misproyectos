
###########################regresion lineal manual, es decir, sin la libreria lm()###########


tabla<-read.csv("datos.csv",header=T)

xbarrah<-mean(tabla$Estatura.H,na.rm=TRUE)
ybarrah<-mean(tabla$Peso.H,na.rm=TRUE)
xbarram<-mean(tabla$Estatura.M)
ybarram<-mean(tabla$Peso.M)
cont1<-cont2<-0

for (i in 1:26) {
  cont1<-(tabla$Estatura.H[i]-xbarrah)*(tabla$Peso.H[i]-ybarrah)+cont1
  cont2<-(tabla$Estatura.H[i]-xbarrah)^2+cont2
}

b1h<-cont1/cont2
b0h<-ybarrah-(xbarrah*b1h)

cont1<-cont2<-0
for(j in 1:28) {
  cont1<-cont1+(tabla$Estatura.M[j]-xbarram)*(tabla$Peso.M[j]-ybarram)
  cont2<-cont2+(tabla$Estatura.M[j]-xbarram)^2
}
b1m<-cont1/cont2
b0m<-ybarram-(xbarram*b1m)

mincuah<-b0h+b1h*tabla$Estatura.H
mincuam<-b0m+b1m*tabla$Estatura.M

plot(tabla$Estatura.H,tabla$Peso.H,main="Hombres",xlab="estatura",ylab="peso")
lines(tabla$Estatura.H,mincuah)
plot(tabla$Estatura.M,tabla$Peso.M,main="Mujeres",xlab="estatura",ylab="peso")
lines(tabla$Estatura.M,mincuam)

errorh<-tabla$Peso.H-mincuah
errorm<-tabla$Peso.M-mincuam

hist(errorh,breaks=10,main="Histograma de errores de hombres",xlab="errores")
hist(errorm,breaks=10,main="Histograma de errores de mujeres",xlab="errores")

cont1<-cont2<-cont3<-0
for(v in 1:26){
  cont1<-cont1+(tabla$Peso.H[v]-ybarrah)^2
  cont2<-cont2+(tabla$Peso.H[v]-mincuah[v])^2
  cont3<-cont3+(mincuah[v]-ybarrah)^2
}
r2h<-(cont3)/(cont2+cont3)
cont1<-cont2<-cont3<-0
for(v in 1:28){
  cont1<-cont1+(tabla$Peso.M[v]-ybarram)^2
  cont2<-cont2+(tabla$Peso.M[v]-mincuam[v])^2
  cont3<-cont3+(mincuam[v]-ybarram)^2
}

r2m<-(cont3)/(cont2+cont3)

print(r2h)
print(r2m)

sumeh<-sum(errorh,na.rm=T)
sumem<-sum(errorm)

print(sumeh)
print(sumem)
cbind(tabla,mincuah,mincuam,errorh,errorm)
