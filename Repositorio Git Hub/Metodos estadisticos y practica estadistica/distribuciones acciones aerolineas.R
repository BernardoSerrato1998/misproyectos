library(readxl)
library(moments)
aerolinea1 <- read.csv(file.choose(),header=TRUE)
aerolinea2 <- read.csv(file.choose(),header=TRUE)

AAL<-aerolinea1$Open
DAL<-aerolinea2$Open
aerolinea1$Date<-as.Date(aerolinea1$Date,"%Y-%m-%d")
aerolinea2$Date<-as.Date(aerolinea1$Date,"%Y-%m-%d")
plot(AAL,DAL,main = "Similitud entre AAL-DAL",xlab ="American Airlines",ylab = "Delta Airlines",col=c("blue","red"))
plot(AAL,main = "Similitud entre AAL-DAL",type="l",xlab ="Años",ylab = "Valor de las acciones",xaxt='n',col="blue")
lines(DAL,col="red")
axis(1,at=1259/5*c(0:5),labels=c("2014","2015","2016","2017","2018","2019"))
boxplot(AAL,DAL,names = c("American Airlines","Delta Airlines"), col=c('blue', 'red'))
hist(AAL,main = paste("Histograma del precio de las acciones de American Airlines"), col="blue")
hist(DAL,main = paste("Histograma del precio de las acciones de Delta Airlines"),col="red")

d1AAL <- density(AAL)
d2AAL <- density(rnorm(length(AAL),mean(AAL),sd(AAL)))
plot(range(d1AAL$x, d2AAL$x), range(d1AAL$y, d2AAL$y), type = "n", xlab = "Densidades AAL-normal",
     ylab = "Densidad")
lines(d1AAL, col = "blue")
lines(d2AAL)

d1DAL <- density(DAL)
d2DAL <- density(rnorm(length(DAL),mean(DAL),sd(DAL)))
plot(range(d1DAL$x, d2DAL$x), range(d1DAL$y, d2DAL$y), type = "n", xlab = "Densidades DAL-normal",
     ylab = "Densidad")
lines(d1DAL, col = "red")
lines(d2DAL)

nombres=c("Var(AAL)","Var(DAL)","Media(AAL)","Media(DAL)","Asimetría(AAL)", "Asimetría(DAL)","Kurtosis(AAL)","Kurtosis(DAL)","Covarianza","Correlación")
numeros<-c(var(AAL),var(DAL), mean(AAL), mean(DAL), skewness(AAL), skewness(DAL),kurtosis(AAL),kurtosis(DAL),cov(AAL,DAL), cor(AAL,DAL))
names(numeros)<-nombres

numeros
summary(AAL)
summary(DAL)


