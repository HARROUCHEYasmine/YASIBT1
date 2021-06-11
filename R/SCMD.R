#´chaine de markov a temps discrete
#'@export
#'@param z numeric vector representing the steps
#´@param mu numeric vector the initial distribution
#´@param p transition matrix
#´@param n numeric number representing number of steps


SCMD<-function(z,mu,P,n)
{
  y<-c(rep(0,n+1))
  t<-c(0:n)
  y[1]<-rdist(z,mu)
  for(i in 1:n){
    y[i+1]<-rdist(z,P[y[i],])
  }
  
  plot(t,y,type = "p",main = "graphe des états pour tous temps de 0 à n" ,pch=5,xlim=c(0,n),ylim=c(0,length(mu)+1),xlab = "temps" ,ylab = "états",col="black")
  plot(t,y,type = "b",main = "graphe des états pour tous temps de 0 à n" ,pch=5,xlim=c(0,n),ylim=c(0,length(mu)+1),xlab = "temps" ,ylab = "états",col="red")
  par(mfrow = c(2,1))
  return(y)
}

