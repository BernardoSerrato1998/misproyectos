numerosal<-function(it,seed){
  x<-c()
  u<-c()
  s<-c()
  x[1]<-seed
for(i in 2:(it+1)){
  x[i]<-x[i-1]^2
  u<-as.numeric(unlist(strsplit(as.character(x[i]), "")))
  if(length(u)==8){x[i]<-as.numeric(paste(u[3:6],collapse=''))
  s[i-1]<-x[i]/10000
  }else if(length(u)<8){
    p<-numeric(8)
    p[c(8-length(u)+1:7)]<-u
    u<-p
    x[i]<-as.numeric(paste(u[3:6],collapse=''))
    s[i-1]<-x[i]/10000
  }
}
  print(s)
  print(paste("La media aritmetica es",mean(s)))
  hist(s)
}

              #--------------------resultados--------------------------------------

numerosal(5,5733)
numerosal(10,1111)
numerosal(50,1111)
numerosal(100,1111)