#proyecto regresion
library(readxl)
install.packages("dummies")

library(dummies)
if(!require(olsrr)){install.packages("olsrr")}
library(olsrr)
if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)
#leer la base de datos
basegastos <- read_excel(file.choose())
attach(basegastos)


#separar la base de datos en .7 y .3
muestra=sample(1:nrow(basegastos),0.7*nrow(basegastos))
trainage=basegastos[muestra,]
testage=basegastos[-muestra,]

#todas las posibles regresiones
gm.gastos = exp(mean(log(trainage$charges)))
log.g = log(trainage$charges)
reg.gastos <- lm(charges ~ ., data = trainage)
all.reg <- as.data.frame(ols_step_all_possible(reg.gastos))
all.reg = format(all.reg, digits=3)
all.regs = all.reg[,c(3,4,5,7,11)]
all.regs

#paso a paso
#1er modelo
reg.a<-(lm(charges~smoker+bmi:smoker+children+age:smoker:bmi:children,data=trainage))
stepw.gastos = ols_step_both_p(reg.a, pent=0.15, prem=0.15, 
                             details=TRUE)
stepw.gastos
plot(stepw.gastos)

#eliminacion hacia atras
backward.gastos = ols_step_backward_p(reg.a, prem=0.15, 
                                    details=TRUE)
backward.gastos

#regresion
reg.a<-(lm(charges~smoker+bmi:smoker+children+age:smoker:bmi:children,data=trainage))
summary(reg.a)
par(mfrow=c(2,2))
plot(reg.a)
par(mfrow=c(1,1))
anova(reg.a)
#prediccion
reg.pred<-(lm(charges~smoker+bmi:smoker+children+age:smoker:bmi:children,data=testage))
prediccion<-predict(reg.pred, newdata = testage, 
                    interval ="prediction")
prediccion
#grafica de los valores reales contra los predecidos
plot(prediccion[,1],type="l",main="Predicción del modelo vs datos empíricos",xlab="Valores",ylab="Intervalo de predicción",col="blue")
lines(testage$charges)
#grafica de intervalos de predccion
plot(prediccion[,1],type="l",main="Predicción",xlab="Valores",ylab="Intervalo de predicción",col="blue")
lines(prediccion[,2],col="red")
lines(prediccion[,3],col="red")
#muestra mas pequeña del intervalo de predicción
plot(prediccion[,1],type="l",main="Predicción",xlab="Valores",ylab="Intervalo de predicción",
     col="blue",xlim=c(0,50))
lines(prediccion[,2],col="red")
lines(prediccion[,3],col="red")

#grafica de intervalos de confianza al 95%
confianza<-predict(reg.pred, newdata = testage, 
                    interval ="confidence")
plot(confianza[,1],type="l",main="Intervalos de confianza",ylab="Valores",col="blue")
lines(confianza[,2],col="red")
lines(confianza[,3],col="red")

#muestra mas pequeña del intervalo de confianza
plot(confianza[,1],type="l",main="Intervalos de confianza",ylab="Valores",col="blue",xlim=c(0,50))
lines(confianza[,2],col="red")
lines(confianza[,3],col="red")

