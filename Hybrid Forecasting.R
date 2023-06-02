library(forecast)
library(Metrics)
library(DescTools)

data=read.csv("C://Users//hp//OneDrive//Documents//Simulations.csv")
attach(data)

#white noise

wn1=ts(wn,frequency=12,start=c(1947,1))
train1=wn[1:850]
test1=wn[851:1000]
train_ts1=ts(train1,frequency=12,start=c(1947,1))
test_ts1=ts(test1,frequency=12,start=c(2013,9))


fit1=auto.arima(train_ts1)
sar_fore=forecast(fit1,150)$mean
resid=fit1$residuals
hwd=ets(resid,model="ZAA",damped=T)
hwd_fore=forecast(hwd,150)$mean
fore=sar_fore+hwd_fore

plot(wn1,type="l")
lines(fore,lty=2,col="red")

accu1=list(mae(test1,fore),rmse(test1,fore),TheilU(test1,fore))



#AR1

train1=matrix(NA,nrow=850,ncol=6)
test1=matrix(NA,nrow=150,ncol=6)

ar1=ts(ar,frequency=12,start=c(1947,1))
colnames(ar)= paste0("AR_sim", 1:ncol(ar))

for(i in 1:6)
{
  train1[,i]=ar[,i][1:850]
  test1[,i]=ar[,i][851:1000]
}
train1
test1
train_ts1=ts(train1,frequency=12,start=c(1947,1))
test_ts1=ts(test1,frequency=12,start=c(2013,9))

ar.1=as.list(ar1)

fore=vector("list",6)
sar_fore=vector("list",6)
resid=vector("list",6)
hwd_fore=list()

for(i in 1:6)
{
  fit1=auto.arima(train_ts1[,i])
  sar_fore[[i]]=forecast(fit1,150)$mean
  resid[[i]]=fit1$residuals
  hwd=ets(resid[[i]],model="ZAA",damped=T)
  hwd_fore[[i]]=forecast(hwd,150)$mean
  fore[[i]]=sar_fore[[i]]+hwd_fore[[i]]
}

par(mfrow=c(2,3))
for(i in 1:6)
{
  plot(ar.1[[1]],type="l")
  lines(fore[[i]],col="blue")
}

mae1=list()
rmse1=list()
u1=list()
for(i in 1:6)
{
  mae1[[i]]=mae(test1[,i],fore[[i]])
  rmse1[[i]]=rmse(test1[,i],fore[[i]])
  u1[[i]]=TheilU(test1[,i],fore[[i]])
}

mae1
rmse1
u1


#MA

ma1=ts(ma,frequency=12,start=c(1947,1))
train2=ma[1:850]
test2=ma[851:1000]
train_ts2=ts(train2,frequency=12,start=c(1947,1))
test_ts2=ts(test2,frequency=12,start=c(2013,9))

fit1=auto.arima(train_ts2)
sar_fore=forecast(fit1,150)$mean
resid=fit1$residuals
hwd=ets(resid,model="ZAA",damped=T)
hwd_fore=forecast(hwd,150)$mean
fore=sar_fore+hwd_fore

plot(ma1,type="l")
lines(fore,lty=2,col="red")

accu1=list(mae(test2,fore),rmse(test2,fore),TheilU(test2,fore))




#ARMA

train1=matrix(NA,nrow=850,ncol=6)
test1=matrix(NA,nrow=150,ncol=6)

arma1=ts(arma,frequency=12,start=c(1947,1))
colnames(arma)= paste0("ARMA_sim", 1:6)

for(i in 1:6)
{
  train1[,i]=arma[,i][1:850]
  test1[,i]=arma[,i][851:1000]
}
train1
test1
train_ts1=ts(train1,frequency=12,start=c(1947,1))
test_ts1=ts(test1,frequency=12,start=c(2013,9))

arma.1=as.list(arma1)

fore1=vector("list",6)
sar_fore1=vector("list",6)
resid1=vector("list",6)
hwd_fore1=list()

for(i in 1:6)
{
  fit1=auto.arima(train_ts1[,i])
  sar_fore1[[i]]=forecast(fit1,150)$mean
  resid1[[i]]=fit1$residuals
  hwd=ets(resid1[[i]],model="ZAA",damped=T)
  hwd_fore1[[i]]=forecast(hwd,150)$mean
  fore1[[i]]=sar_fore1[[i]]+hwd_fore1[[i]]
}

par(mfrow=c(2,3))
for(i in 1:6)
{
  plot(arma.1[[1]],type="l")
  lines(fore1[[i]],col="blue")
}

mae1=list()
rmse1=list()
u1=list()
for(i in 1:6)
{
  mae1[[i]]=mae(test1[,i],fore1[[i]])
  rmse1[[i]]=rmse(test1[,i],fore1[[i]])
  u1[[i]]=TheilU(test1[,i],fore1[[i]])
}

mae1
rmse1
u1



#ARIMA

train1=matrix(NA,nrow=850,ncol=6)
test1=matrix(NA,nrow=150,ncol=6)

arima1=ts(arima,frequency=12,start=c(1947,1))
colnames(arima)= paste0("ARIMA_sim", 1:6)

for(i in 1:6)
{
  train1[,i]=arima[,i][1:850]
  test1[,i]=arima[,i][851:1000]
}
train1
test1
train_ts1=ts(train1,frequency=12,start=c(1947,1))
test_ts1=ts(test1,frequency=12,start=c(2013,9))

arima.1=as.list(arima1)



fore1=vector("list",6)
sar_fore1=vector("list",6)
resid1=vector("list",6)
hwd_fore1=list()

for(i in 1:6)
{
  fit1=auto.arima(train_ts1[,i])
  sar_fore1[[i]]=forecast(fit1,150)$mean
  resid1[[i]]=fit1$residuals
  hwd=ets(resid1[[i]],model="ZAA",damped=T)
  hwd_fore1[[i]]=forecast(hwd,150)$mean
  fore1[[i]]=sar_fore1[[i]]+hwd_fore1[[i]]
}

par(mfrow=c(2,3))
for(i in 1:6)
{
  plot(arima.1[[i]],type="l")
  lines(fore1[[i]],col="blue")
}

mae1=list()
rmse1=list()
u1=list()
for(i in 1:6)
{
  mae1[[i]]=mae(test1[,i],fore1[[i]])
  rmse1[[i]]=rmse(test1[,i],fore1[[i]])
  u1[[i]]=TheilU(test1[,i],fore1[[i]])
}

mae1
rmse1
u1



#ARFIMA

train1=matrix(NA,nrow=850,ncol=6)
test1=matrix(NA,nrow=150,ncol=6)

arfima1=ts(arfima,frequency=12,start=c(1947,1))
colnames(arfima)= paste0("ARFIMA_sim", 1:6)

for(i in 1:6)
{
  train1[,i]=arfima[,i][1:850]
  test1[,i]=arfima[,i][851:1000]
}
train1
test1
train_ts1=ts(train1,frequency=12,start=c(1947,1))
test_ts1=ts(test1,frequency=12,start=c(2013,9))

arfima.1=as.list(arfima1)



fore1=vector("list",6)
sar_fore1=vector("list",6)
resid1=vector("list",6)
hwd_fore1=list()

for(i in 1:6)
{
  fit1=auto.arima(train_ts1[,i])
  sar_fore1[[i]]=forecast(fit1,150)$mean
  resid1[[i]]=fit1$residuals
  hwd=ets(resid1[[i]],model="ZAA",damped=T)
  hwd_fore1[[i]]=forecast(hwd,150)$mean
  fore1[[i]]=sar_fore1[[i]]+hwd_fore1[[i]]
}

par(mfrow=c(2,3))
for(i in 1:6)
{
  plot(arfima.1[[i]],type="l")
  lines(fore1[[i]],col="blue")
}

mae1=list()
rmse1=list()
u1=list()
for(i in 1:6)
{
  mae1[[i]]=mae(test1[,i],fore1[[i]])
  rmse1[[i]]=rmse(test1[,i],fore1[[i]])
  u1[[i]]=TheilU(test1[,i],fore1[[i]])
}

mae1
rmse1
u1




#TAR

train1=matrix(NA,nrow=850,ncol=6)
test1=matrix(NA,nrow=150,ncol=6)

tar1=ts(tar,frequency=12,start=c(1947,1))
colnames(tar)= paste0("TAR_sim", 1:6)

for(i in 1:6)
{
  train1[,i]=tar[,i][1:850]
  test1[,i]=tar[,i][851:1000]
}
train1
test1
train_ts1=ts(train1,frequency=12,start=c(1947,1))
test_ts1=ts(test1,frequency=12,start=c(2013,9))

tar.1=as.list(tar1)


fore1=vector("list",6)
sar_fore1=vector("list",6)
resid1=vector("list",6)
hwd_fore1=list()

for(i in 1:6)
{
  fit1=auto.arima(train_ts1[,i])
  sar_fore1[[i]]=forecast(fit1,150)$mean
  resid1[[i]]=fit1$residuals
  hwd=ets(resid1[[i]],model="ZAA",damped=T)
  hwd_fore1[[i]]=forecast(hwd,150)$mean
  fore1[[i]]=sar_fore1[[i]]+hwd_fore1[[i]]
}

par(mfrow=c(2,3))
for(i in 1:6)
{
  plot(tar.1[[i]],type="l")
  lines(fore1[[i]],col="blue")
}

mae1=list()
rmse1=list()
u1=list()
for(i in 1:6)
{
  mae1[[i]]=mae(test1[,i],fore1[[i]])
  rmse1[[i]]=rmse(test1[,i],fore1[[i]])
  u1[[i]]=TheilU(test1[,i],fore1[[i]])
}

mae1
rmse1
u1




#SETAR

train1=matrix(NA,nrow=850,ncol=6)
test1=matrix(NA,nrow=150,ncol=6)

setar1=ts(setar,frequency=12,start=c(1947,1))
colnames(setar)= paste0("SETAR_sim", 1:6)

for(i in 1:6)
{
  train1[,i]=setar[,i][1:850]
  test1[,i]=setar[,i][851:1000]
}
train1
test1
train_ts1=ts(train1,frequency=12,start=c(1947,1))
test_ts1=ts(test1,frequency=12,start=c(2013,9))

setar.1=as.list(setar1)

fore1=vector("list",6)
sar_fore1=vector("list",6)
resid1=vector("list",6)
hwd_fore1=list()

for(i in 1:6)
{
  fit1=auto.arima(train_ts1[,i])
  sar_fore1[[i]]=forecast(fit1,150)$mean
  resid1[[i]]=fit1$residuals
  hwd=ets(resid1[[i]],model="ZAA",damped=T)
  hwd_fore1[[i]]=forecast(hwd,150)$mean
  fore1[[i]]=sar_fore1[[i]]+hwd_fore1[[i]]
}

par(mfrow=c(2,3))
for(i in 1:6)
{
  plot(setar.1[[i]],type="l")
  lines(fore1[[i]],col="blue")
}

mae1=list()
rmse1=list()
u1=list()
for(i in 1:6)
{
  mae1[[i]]=mae(test1[,i],fore1[[i]])
  rmse1[[i]]=rmse(test1[,i],fore1[[i]])
  u1[[i]]=TheilU(test1[,i],fore1[[i]])
}

mae1
rmse1
u1



#ARMA with trend
#linear


art1=ts(arma_tren1,frequency=12,start=c(1947,1))
train1=arma_tren1[1:850]
test1=arma_tren1[851:1000]
train_ts1=ts(train1,frequency=12,start=c(1947,1))
test_ts1=ts(test1,frequency=12,start=c(2013,9))


fit1=auto.arima(train_ts1)
sar_fore=forecast(fit1,150)$mean
resid=fit1$residuals
hwd=ets(resid,model="ZAA",damped=T)
hwd_fore=forecast(hwd,150)$mean
fore=sar_fore+hwd_fore

plot(art1,type="l")
lines(fore,lty=2,col="red")

accu1=list(mae(test1,fore),rmse(test1,fore),TheilU(test1,fore))



#quadratic
art2=ts(arma_tren2,frequency=12,start=c(1947,1))
train1=arma_tren2[1:850]
test1=arma_tren2[851:1000]
train_ts1=ts(train1,frequency=12,start=c(1947,1))
test_ts1=ts(test1,frequency=12,start=c(2013,9))


fit1=auto.arima(train_ts1)
sar_fore=forecast(fit1,150)$mean
resid=fit1$residuals
hwd=ets(resid,model="ZAA",damped=T)
hwd_fore=forecast(hwd,150)$mean
fore=sar_fore+hwd_fore

plot(art2,type="l")
lines(fore,lty=2,col="red")

accu1=list(mae(test1,fore),rmse(test1,fore),TheilU(test1,fore))




#ARMA with seasonality

ars=ts(arma_season1,frequency=12,start=c(1947,1))
train1=arma_season1[1:850]
test1=arma_season1[851:1000]
train_ts1=ts(train1,frequency=12,start=c(1947,1))
test_ts1=ts(test1,frequency=12,start=c(2013,9))


fit1=auto.arima(train_ts1)
sar_fore=forecast(fit1,150)$mean
resid=fit1$residuals
hwd=ets(resid,model="ZAA",damped=T)
hwd_fore=forecast(hwd,150)$mean
fore=sar_fore+hwd_fore

plot(ars,type="l")
lines(fore,lty=2,col="red")

accu1=list(mae(test1,fore),rmse(test1,fore),TheilU(test1,fore))
