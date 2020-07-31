tbulg<-read.csv("tbulg.csv",header = T)
qx<-tbulg$qx
qx1<-qx
px<-1-qx
f<-length(px)
int<-.05
#el seguro tendrá 7 variables (tipo1.tipo2,int,vida,e1,e2,n,m,b)
#tipo1[pnu,anualidad,prima]
#tipo2 para pnu [ordinario,temporal,puro,mixto,diferido, diferido] 
#tipo2 para anualidad[vitalicia ant, temporal ant, diferida ant, diferida temporal]
#tipo2 para primas[prima ordinario, prima temporal, prima dotal puro, prima dotal mixto, prima diferido]
#int: se proporciona la tasa de interés, e.g. .05
#vida[simple, conjuntas,ultimo]
# e1: edad de la persona 1, e2: edad de la persona 2, n: plazo del seguro, m: diferimiento, b: s.a.
lx<-numeric(f)
lx[1]<-100000
for(i in 2:f){
  lx[i]<-lx[i-1]*px[i-1]
}

w<-0:110
#valores de vidas simples
dx<-numeric(f)
Dx<-numeric(f)
Cx<-numeric(f)

for(i in 1:f){
  dx[i]<-lx[i]*qx[i]
  Dx[i]<-((1+int)^(-w[i]))*lx[i]
  Cx[i]<-((1+int)^(-(w[i]+1)))*dx[i]
}
Mx<-numeric(f)
Mx[1]<-sum(Cx)
for(i in 2:f){
  Mx[i]<-Mx[i-1]-Cx[i-1]
}

Nx<-numeric(f)
Nx[1]<-sum(Dx)
for(i in 2:f){
  Nx[i]<-Nx[i-1]-Dx[i-1]
}
#valores de vidas múltiples
#vidas conjuntas para personas de la misma edad xx
pxxvc<-numeric(f)
qxxvc<-numeric(f)

for(i in 1:f){
  pxxvc[i]<-(px[i])^2
  qxxvc[i]<-1-pxxvc[i]
}

Cxxvc<-numeric(f)
Dxxvc<-numeric(f)
for (i in 1:f) {
  Cxxvc[i] <-(Cx[i]*dx[i])
  Dxxvc[i]<-Dx[i]*lx[i]
}

Mxxvc<-numeric(f)
Mxxvc[1]<-sum(Cxxvc)
for(i in 2:f){
  Mxxvc[i]<Mxxvc[i-1]-Cxxvc[i-1]
}

Nxxvc<-numeric(f)
Nxxvc[1]<-sum(Dxxvc)
for(i in 2:f){
  Nxxvc[i]<Nxxvc[i-1]-Dxxvc[i-1]
}
#valores de estatus de ultimo sobreviviente
qxxus<-numeric(f)
pxxus<-numeric(f)
qxxus<-(qx)^2
pxxus<-1-qxxus
#empiezan los "if" que seran los filtros para lo que quiere el usuario
seguro<-function(t,e,n=NULL,m=NULL){
  if(t=="ordinario"){
    return(Mx[e+1]/Dx[e+1])
  }
  else if (t=="temporal" & !is.null(n)){
    return((Mx[e+1]-Mx[e+n+1])/Dx[e+1])
  }
  else if (t=="puro" & !is.null(n)){
    return(Dx[e+n+1]/Dx[e+1])
  }
  else if (t=="mixto" & !is.null(n)){
    return((Mx[e+1]-Mx[e+n+1]+Dx[e+n+1])/Dx[e+1])
  }
  else if (t=="diferido" & !is.null(n)){
    return(Mx[e+n+1]/Dx[e+1])
  }
  else if (t=="diferido temporal" & !is.null(n) & !is.null(m)){
    return((Mx[e+m+1]-Mx[e+n+m+1])/Dx[e+1])
  }
  else{
    return("Datos no identificados")
  }
}

cbind(w,qx,px,lx,dx,Mx,Cx,Dx)
