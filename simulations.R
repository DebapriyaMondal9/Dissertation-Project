#White Noise
wn=rnorm(1000)
plot(wn,type="l",main="White Noise")

#AR(1) processes

coef=c(-0.9,-0.4,-0.1,0.1,0.4,0.9)
ar=matrix(NA,ncol=6,nrow=1000)
ar[1,]=rnorm(6)

for(i in 2:1000)
{
  for(j in 1:6){
  ar[i,j]=coef[j]*ar[i-1,j]+wn[i]
  }
}

par(mfrow=c(2,3))
for(i in 1:6)
{
  plot(ar[,i],type="l",main=paste("phi=",coef[i]))
} 


#MA(1) process, theta=0.6
ma=arima.sim(list(order=c(0,0,1),ma=0.6),n=1000)
plot(ma,type="l",main="MA")


#ARMA
runif(6,-1,1)
ar.coef=list(-0.8,0.3,-0.1,-0.3,0.4,0.8)
ma.coef=list(-0.9,-0.2,0.1,0.2,-0.3,0.7)

arma=matrix(NA,nrow=1000,ncol=6)
for(i in 1:6)
{
  arma[,i]=arima.sim(list(order=c(1,0,1),ar=ar.coef[[i]],ma=ma.coef[[i]]),n=1000)
}


par(mfrow=c(2,3))
for(i in 1:6)
{
  plot(arma[,i],type="l",main=paste("phi=",ar.coef[i],",theta=",ma.coef[i]))
}


#ACF
par(mfrow=c(2,3))
for(i in 1:6)
{
  acf(arma[,i])
}

#PACF
par(mfrow=c(2,3))
for(i in 1:6)
{
  pacf(arma[,i])
}



#ARIMA

ar.coef=list(0.6,0.2,0.8,0.3,0.5,0.7)
ma.coef=list(-0.4,0.1,-0.3,0.2,-0.1,-0.2)

arima=matrix(NA,nrow=1000,ncol=6)
for(i in 1:6)
{
  arima[,i]=arima.sim(list(order=c(1,1,1),ar=ar.coef[[i]],ma=ma.coef[[i]]),n=999)
}

par(mfrow=c(2,3))
for(i in 1:6)
{
  plot(arima[,i],type="l",main=paste("phi=",ar.coef[i],",theta=",ma.coef[i]))
}


#ACF
par(mfrow=c(2,3))
for(i in 1:6)
{
  acf(arima[,i])
}

#PACF
par(mfrow=c(2,3))
for(i in 1:6)
{
  pacf(arima[,i])
}


#ARFIMA
runif(6,-0.5,0.5)
ar.coef=list(-0.6,NULL,NULL,0.8,0.9,-0.5)
ma.coef=list(0.9,-0.5,NULL,NULL,NULL,0.7)
dfrac=list(0.2,-0.2,0.3,-0.49,0.49,-0.4)

arfima=matrix(NA,nrow=1000,ncol=6)

for(i in 1:6)
{
  arfima[,i]=arfima.sim(1000,model=list(phi=ar.coef[[i]],dfrac=dfrac[[i]],theta=ma.coef[[i]]))

}


par(mfrow=c(2,3))
for(i in 1:6)
{
  plot(arfima[,i],type="l",main=paste("phi=",ar.coef[i],",theta=",ma.coef[i],",dfrac=",dfrac[i]))
}


#ACF
par(mfrow=c(2,3))
for(i in 1:6)
{
  acf(arfima[,i])
}

#PACF
par(mfrow=c(2,3))
for(i in 1:6)
{
  pacf(arfima[,i])
}


#TAR
library(TSA)

phi1=c(0.7,-0.1,0.4,0.6,0.5,-0.9)
phi2=c(-0.7,0.2,0.3,-0.6,-0.5,0.9)
thd=c(0.5,0.4,-0.1,0.7,0.6,-0.3)

tar=matrix(NA,nrow=1000,ncol=6)

for(i in 1:6)
{
  tar[,i]=tar.sim(n=1000, thd=thd[i], Phi1=phi1[i], Phi2=phi2[i], sigma2=1,p=1,d=1,sigma1=1)$y

}

par(mfrow=c(2,3))
for(i in 1:6)
{
  plot(tar[,i],type="l")
}


#ACF
par(mfrow=c(2,3))
for(i in 1:6)
{
  acf(tar[,i])
}



#SETAR
library(tsDyn)
B=matrix(c(0.5,0.3,-0.3,0.6,0.2,-0.4,0.1,-0.5,-0.2,0.7,-0.5,0.4),byrow=T,nrow=6)
Thresh=c(0.5,-0.2,0.1,0.8,-0.3,0.2)
setar=matrix(NA,nrow=1000,ncol=6)
for(i in 1:6)
{
  setar[,i]=setar.sim(n=1000,B=B[i,],type="simul",include="none",lag=1,nthresh=1,Thresh=Thresh[i])

}

par(mfrow=c(2,3))
for(i in 1:6)
{
  plot(setar[,i],type="l",main=paste("phi1=",B[i,1],",phi2=",B[i,2],",thd=",Thresh[i]))
}

#ACF
par(mfrow=c(2,3))
for(i in 1:6)
{
  acf(setar[,i])
}


#ARMA with trend

arma_tren=arima.sim(list(order=c(1,0,1),ar=0.7,ma=0.5),n=1000)
t=1:1000

#linear
lin=-0.6+0.3*t
arma_tren1=arma_tren+lin
plot(arma_tren1,type="l")

#quadratic
quad=0.5+0.3*t-0.4*t^2
arma_tren2=arma_tren+quad
plot(arma_tren2,type="l")



#ARMA with seasonality
arma_season=arima.sim(list(order=c(1,0,1),ar=0.8,ma=0.1),n=1000)
t=1:1000
j=12
plot(arma_season,type="l")
arma_season1=3*sin(2*pi*t/j)+arma_season
plot(arma_season1,type="l")


df=data.frame(wn,ar[,1],ar[,2],ar[,3],ar[,4],ar[,5],ar[,6],ma,arma[,1],arma[,2],arma[,3],arma[,4],arma[,5],arma[,6],arima[,1],arima[,2],arima[,3],arima[,4],arima[,5],arima[,6],arfima[,1],arfima[,2],arfima[,3],arfima[,4],arfima[,5],arfima[,6],tar[,1],tar[,2],tar[,3],tar[,4],tar[,5],tar[,6],setar[,1],setar[,2],setar[,3],setar[,4],setar[,5],setar[,6],arma_tren1,arma_tren2,arma_season1)

library("writexl")

write_xlsx(df,"C:\\Users\\hp\\OneDrive\\Documents\\Simulations.xlsx")
