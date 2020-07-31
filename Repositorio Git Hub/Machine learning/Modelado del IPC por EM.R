#trabajo de algoritmo EM
#p1 es un vector de probabilidades a posrteriori de Omega 1 con xi
#p2 es un vector de probabilidades a posteriori de Omega 2 con xi
#m1 es miu 1            #m2 es miu
#s1 es sigma 1 cuadrada    #s2 es sigma 2 cuadrada
#leer los datos
library(readxl)
IPCUSD <- read_excel("IPCUSD.xlsm",
                     range = "A1:E1258")
#se crean vectores
alphas<-numeric(2000)
mius1<-numeric(2000)
mius2<-numeric(2000)
sigmas1<-numeric(2000)
sigmas2<-numeric(2000)

#valores iniciales
alpha1<-.7
alpha2<-1-alpha1
m1<-quantile(IPCUSD$IPC,probs= 0.5)
m2<-quantile(IPCUSD$IPC, 0.75)
s1<-(max(IPCUSD$IPC)+min(IPCUSD$IPC))/4 
s2<-sd(IPCUSD$IPC)


alphas[1]<-alpha1
mius1[1]<-m1
mius2[1]<-m2
sigmas1[1]<-s1
sigmas2[1]<-s2

#vectores de probabilidades a posteriori
l<-length(IPCUSD$IPC)
p1<-numeric(l)
p1x<-numeric(l)
p1xx<-numeric(l)
p2<-numeric(l)
p2x<-numeric(l)
p2xx<-numeric(l)
#ciclo para definir los valores de las probabilidades a posteriori,
#probabilidades a posteriori con x[i], probabilidades a posteriori con(x[i]-m)^2
for (i in 1:l){
  p1[i]<-(alpha1*dnorm((IPCUSD$IPC[i]-m1)/s1)/s1)/((alpha1*dnorm((IPCUSD$IPC[i]-m1)/s1)/s1)+(alpha2)*dnorm((IPCUSD$IPC[i]-m2)/s2)/s2)
  p2[i]<-1-p1[i]
  p1x[i]<-p1[i]*IPCUSD$IPC[i]
  p1xx[i]<-p1[i]*(IPCUSD$IPC[i]-m1)^2
  p2x[i]<-p2[i]*IPCUSD$IPC[i]
  p2xx[i]<-p2[i]*(IPCUSD$IPC[i]-m2)^2
}
alpha1<-mean(p1)
alpha2<-mean(p2)
m1.<-(sum(p1x))/(sum(p1))
m2.<-(sum(p2x))/(sum(p2))
s1.<-sqrt((sum(p1xx))/(sum(p1)))
s2.<-sqrt((sum(p2xx))/(sum(p2)))

alphas[2]<-alpha1
mius1[2]<-m1.
mius2[2]<-m2.
sigmas1[2]<-s1.
sigmas2[2]<-s2.
k=0

e<-.0000001
#se hace el criterio de convergencia
while(abs(m1-m1.)>e ){
  k=k+1
  m1<-m1.
  s1<-s1.
  m2<-m2.
  s2<-s2.
  for (i in 1:l){
    p1[i]<-(alpha1*dnorm((IPCUSD$IPC[i]-m1)/s1)/s1)/((alpha1*dnorm((IPCUSD$IPC[i]-m1)/s1)/s1)+(alpha2)*dnorm((IPCUSD$IPC[i]-m2)/s2)/s2)
    p2[i]<-1-p1[i]
    p1x[i]<-p1[i]*IPCUSD$IPC[i]
    p1xx[i]<-p1[i]*(IPCUSD$IPC[i]-m1)^2
    p2x[i]<-p2[i]*IPCUSD$IPC[i]
    p2xx[i]<-p2[i]*(IPCUSD$IPC[i]-m2)^2
  }
  alpha1<-mean(p1)
  alpha2<-mean(p2)
  m1.<-(sum(p1x))/(sum(p1))
  m2.<-(sum(p2x))/(sum(p2))
  s1.<-sqrt((sum(p1xx))/(sum(p1)))
  s2.<-sqrt((sum(p2xx))/(sum(p2)))
  
  alphas[k+2]<-alpha1
  mius1[k+2]<-m1.
  mius2[k+2]<-m2.
  sigmas1[k+2]<-s1.
  sigmas2[k+2]<-s2.
}

#concatenar todos los valores y ponerlos en tablitas
nombres<-c("iteraciones","media1","media2","desv1","desv2","alpha","alpha2","epsilon")
y<-c(k,m1.,m2.,s1.,s2.,alpha1,alpha2,e)
names(y)<-nombres
r<-format(y,round(4),scientific = F)
View(r)
total<-cbind(alphas,mius1,mius2,sigmas1,sigmas2)
View(total)

#obtener densidades gaussianas
teorica<-c(seq(min(IPCUSD$IPC),max(IPCUSD$IPC),by=.0001))
teorica1<-numeric(length(teorica))
for(i in 1:length(teorica)){
  teorica1[i]<-((alpha1*dnorm((teorica[i]-m1)/s1)/s1)+(alpha2)*dnorm((teorica[i]-m2)/s2)/s2)
}

#graficar densidades gaussianas vs empiricas
dene<-density(IPCUSD$IPC)
plot(dene$x,dene$y,type="l",col="red",xlab="IPC",ylab="Densidad",main="Densidad empírica vs Mixtura Gaussiana")
lines(teorica,teorica1,col="blue")
legend(.02,30,lwd=c(1,1),col=c("blue","red"),legend=c("Gaussiana","Empírica"))

#graficar densidades normales vs empiricas
teorica2<-numeric(length(teorica))
for(i in 1:length(teorica)){
  teorica2[i]<-dnorm((teorica[i]-mean(IPCUSD$IPC))/sd(IPCUSD$IPC))/sd(IPCUSD$IPC)
}
plot(dene$x,dene$y,type="l",col="red",xlab="IPC",ylab="Densidad",main="Densidad empírica vs Normal")
lines(teorica,teorica2,col="black")
legend(.02,30,lwd=c(1,1),col=c("black","red"),legend=c("Normal","Empírica"))

#graficar distribuciones gaussianas vs empiricas
teorica11<-numeric(length(teorica))
for(i in 1:length(teorica)){
  teorica11[i]<-alpha1*pnorm((teorica[i]-m1)/s1)+(alpha2)*pnorm((teorica[i]-m2)/s2)
}
plot(ecdf(IPCUSD$IPC),col="red")
lines(teorica,teorica11,type="l",col="blue")
legend(0,.04,lwd=c(1,1),col=c("blue","red"),legend=c("Gaussiana","Empírica"))

#graficar distribuciones normales vs empiricas
teorica22<-numeric(length(teorica))
for(i in 1:length(teorica)){
  teorica22[i]<-pnorm((teorica[i]-mean(IPCUSD$IPC))/sd(IPCUSD$IPC))
}
plot(ecdf(IPCUSD$IPC),col="red")
lines(teorica,teorica22,type="l",col="black")
legend(0,.04,lwd=c(1,1),col=c("black","red"),legend=c("Gaussiana","Empírica"))



