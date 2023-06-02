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

#HoltWinters
HW1=HoltWinters(train_ts1)
HW1.forecast=forecast(HW1,150)$mean

plot(wn1,type="l")
lines(HW1$fitted[,1],lty=2,col="blue")
lines(HW1.forecast,lty=2,col="red")


#SARIMA

fit1=auto.arima(train_ts1)
arima_forecast1=forecast(fit1,150)$mean

plot(wn1,type="l")
lines(fit1$fitted,lty=2,col="blue")
lines(arima_forecast1,lty=2,col="red")


#Damped HoltWinters

hw1=ets(train_ts1,model="ZAA",damped=T)
hwd1.forecast=forecast(hw1,150)$mean

plot(wn1,type="l")
lines(hw1$fitted,lty=2,col="blue")
lines(hwd1.forecast,lty=2,col="red")

#Accuracy

accu1=list(mae(test1,HW1.forecast),rmse(test1,HW1.forecast),TheilU(test1,HW1.forecast))
accu2=list(mae(test1,arima_forecast1),rmse(test1,arima_forecast1),TheilU(test1,arima_forecast1))
accu3=list(mae(test1,hwd1.forecast),rmse(test1,hwd1.forecast),TheilU(test1,hwd1.forecast))




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


#HoltWinters forecasting

HW1.forecast=list()
fit1=list()
for(i in 1:6)
{
  HW1=HoltWinters(train_ts1[,i])
  HW1.forecast[[i]]=forecast(HW1,150)$mean
  fit1[[i]]=HW1$fitted[,1]
}

par(mfrow=c(3,2))
for(i in 1:6)
{
  plot(ar.1[[i]],type="l")
  lines(fit1[[i]],lty=2,col="blue")
  lines(HW1.forecast[[i]],lty=2,col="red")
}


#SARIMA

fit2=list()
sarima=list()
for(i in 1:6)
{
  sar=auto.arima(train_ts1[,i])
  sarima[[i]]=forecast(sar,150)$mean
  fit2[[i]]=sar$fitted
}

par(mfrow=c(2,3))
for(i in 1:6)
{
  plot(ar.1[[i]],type="l")
  lines(fit2[[i]],lty=2,col="blue")
  lines(sarima[[i]],lty=2,col="red")
}


#Damped HoltWinters

fit3=list()
hwd1=list()
for(i in 1:6)
{
  hwd=ets(train_ts1[,i],model="ZAA",damped=T)
  hwd1[[i]]=forecast(hwd,150)$mean
  fit3[[i]]=hwd$fitted
}

for(i in 1:6)
{
  plot(ar.1[[i]],type="l")
  lines(fit3[[i]],lty=2,col="blue")
  lines(hwd1[[i]],lty=2,col="red")
}




#Accuracy


mae1=list()
rmse1=list()
u1=list()
for(i in 1:6)
{
  mae1[[i]]=mae(test1[,i],HW1.forecast[[i]])
  rmse1[[i]]=rmse(test1[,i],HW1.forecast[[i]])
  u1[[i]]=TheilU(test1[,i],HW1.forecast[[i]])
}

mae1
rmse1
u1


mae2=list()
rmse2=list()
u2=list()
for(i in 1:6)
{
  mae2[[i]]=mae(test1[,i],sarima[[i]])
  rmse2[[i]]=rmse(test1[,i],sarima[[i]])
  u2[[i]]=TheilU(test1[,i],sarima[[i]])
}

mae2
rmse2
u2


mae3=list()
rmse3=list()
u3=list()
for(i in 1:6)
{
  mae3[[i]]=mae(test1[,i],hwd1[[i]])
  rmse3[[i]]=rmse(test1[,i],hwd1[[i]])
  u3[[i]]=TheilU(test1[,i],hwd1[[i]])
}

mae3
rmse3
u3




#MA

ma1=ts(ma,frequency=12,start=c(1947,1))
train2=ma[1:850]
test2=ma[851:1000]
train_ts2=ts(train2,frequency=12,start=c(1947,1))
test_ts2=ts(test2,frequency=12,start=c(2013,9))

par(mfrow=c(3,1))
#HoltWinters

HW1=HoltWinters(train_ts2)
HW1.forecast=forecast(HW1,150)$mean

plot(ma1,type="l")
lines(HW1$fitted[,1],lty=2,col="blue")
lines(HW1.forecast,lty=2,col="red")

#SARIMA

fit1=auto.arima(train_ts2)
arima_forecast1=forecast(fit1,150)$mean

plot(ma1,type="l")
lines(fit1$fitted,lty=2,col="blue")
lines(arima_forecast1,lty=2,col="red")


#Damped HoltWinters

hw1=ets(train_ts2,model="ZAA",damped=T)
hwd1.forecast=forecast(hw1,150)$mean

plot(ma1,type="l")
lines(hw1$fitted,lty=2,col="blue")
lines(hwd1.forecast,lty=2,col="red")

accu1=list(mae(test2,HW1.forecast),rmse(test2,HW1.forecast),TheilU(test2,HW1.forecast))
accu2=list(mae(test2,arima_forecast1),rmse(test2,arima_forecast1),TheilU(test2,arima_forecast1))
accu3=list(mae(test2,hwd1.forecast),rmse(test2,hwd1.forecast),TheilU(test2,hwd1.forecast))



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


#HoltWinters

HW2.forecast=list()
fit4=list()
for(i in 1:6)
{
  HW1=HoltWinters(train_ts1[,i])
  HW2.forecast[[i]]=forecast(HW1,150)$mean
  fit4[[i]]=HW1$fitted[,1]
}

par(mfrow=c(3,2))
for(i in 1:6)
{
  plot(arma.1[[i]],type="l")
  lines(fit4[[i]],lty=2,col="blue")
  lines(HW2.forecast[[i]],lty=2,col="red")
}


#SARIMA

fit5=list()
sarima1=list()
for(i in 1:6)
{
  sar=auto.arima(train_ts1[,i])
  sarima1[[i]]=forecast(sar,150)$mean
  fit5[[i]]=sar$fitted
}

par(mfrow=c(2,3))
for(i in 1:6)
{
  plot(arma.1[[i]],type="l")
  lines(fit5[[i]],lty=2,col="blue")
  lines(sarima1[[i]],lty=2,col="red")
}


#Damped HoltWinters

fit6=list()
hwd2=list()
for(i in 1:6)
{
  hwd=ets(train_ts1[,i],model="ZAA",damped=T)
  hwd2[[i]]=forecast(hwd,150)$mean
  fit6[[i]]=hwd$fitted
}

for(i in 1:6)
{
  plot(arma.1[[i]],type="l")
  lines(fit6[[i]],lty=2,col="blue")
  lines(hwd2[[i]],lty=2,col="red")
}

#Accuracy

mae4=list()
rmse4=list()
u4=list()
for(i in 1:6)
{
  mae4[[i]]=mae(test1[,i],HW2.forecast[[i]])
  rmse4[[i]]=rmse(test1[,i],HW2.forecast[[i]])
  u4[[i]]=TheilU(test1[,i],HW2.forecast[[i]])
}

mae4
rmse4
u4

mae5=list()
rmse5=list()
u5=list()
for(i in 1:6)
{
  mae5[[i]]=mae(test1[,i],sarima1[[i]])
  rmse5[[i]]=rmse(test1[,i],sarima1[[i]])
  u5[[i]]=TheilU(test1[,i],sarima1[[i]])
}

mae5
rmse5
u5

mae6=list()
rmse6=list()
u6=list()
for(i in 1:6)
{
  mae6[[i]]=mae(test1[,i],hwd2[[i]])
  rmse6[[i]]=rmse(test1[,i],hwd2[[i]])
  u6[[i]]=TheilU(test1[,i],hwd2[[i]])
}

mae6
rmse6
u6



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


#HoltWinters

HW2.forecast=list()
fit4=list()
for(i in 1:6)
{
  HW1=HoltWinters(train_ts1[,i])
  HW2.forecast[[i]]=forecast(HW1,150)$mean
  fit4[[i]]=HW1$fitted[,1]
}

par(mfrow=c(3,2))
for(i in 1:6)
{
  plot(arima.1[[i]],type="l")
  lines(fit4[[i]],lty=2,col="blue")
  lines(HW2.forecast[[i]],lty=2,col="red")
}


#SARIMA

fit5=list()
sarima1=list()
for(i in 1:6)
{
  sar=auto.arima(train_ts1[,i])
  sarima1[[i]]=forecast(sar,150)$mean
  fit5[[i]]=sar$fitted
}

for(i in 1:6)
{
  plot(arima.1[[i]],type="l")
  lines(fit5[[i]],lty=2,col="blue")
  lines(sarima1[[i]],lty=2,col="red")
}


#Damped HoltWinters

fit6=list()
hwd2=list()
for(i in 1:6)
{
  hwd=ets(train_ts1[,i],model="ZAA",damped=T)
  hwd2[[i]]=forecast(hwd,150)$mean
  fit6[[i]]=hwd$fitted
}

for(i in 1:6)
{
  plot(arima.1[[i]],type="l")
  lines(fit6[[i]],lty=2,col="blue")
  lines(hwd2[[i]],lty=2,col="red")
}

#Accuracy

mae4=list()
rmse4=list()
u4=list()
for(i in 1:6)
{
  mae4[[i]]=mae(test1[,i],HW2.forecast[[i]])
  rmse4[[i]]=rmse(test1[,i],HW2.forecast[[i]])
  u4[[i]]=TheilU(test1[,i],HW2.forecast[[i]])
}

mae4
rmse4
u4

m1=matrix(c(mae6,rmse6,u6),byrow=F,nrow=6)

mae5=list()
rmse5=list()
u5=list()
for(i in 1:6)
{
  mae5[[i]]=mae(test1[,i],sarima1[[i]])
  rmse5[[i]]=rmse(test1[,i],sarima1[[i]])
  u5[[i]]=TheilU(test1[,i],sarima1[[i]])
}

mae5
rmse5
u5

mae6=list()
rmse6=list()
u6=list()
for(i in 1:6)
{
  mae6[[i]]=mae(test1[,i],hwd2[[i]])
  rmse6[[i]]=rmse(test1[,i],hwd2[[i]])
  u6[[i]]=TheilU(test1[,i],hwd2[[i]])
}

mae6
rmse6
u6





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


#HoltWinters

HW3.forecast=list()
fit7=list()
for(i in 1:6)
{
  HW1=HoltWinters(train_ts1[,i])
  HW3.forecast[[i]]=forecast(HW1,150)$mean
  fit7[[i]]=HW1$fitted[,1]
}

HW1=HoltWinters(train_ts1[,5],gamma=F)
HW3.forecast[[5]]=forecast(HW1,150)$mean
fit7[[5]]=HW1$fitted[,1]

par(mfrow=c(3,2))
for(i in 1:6)
{
  plot(arfima.1[[i]],type="l")
  lines(fit7[[i]],lty=2,col="blue")
  lines(HW3.forecast[[i]],lty=2,col="red")
}



#SARIMA

fit8=list()
sarima2=list()
for(i in 1:6)
{
  sar=auto.arima(train_ts1[,i])
  sarima2[[i]]=forecast(sar,150)$mean
  fit8[[i]]=sar$fitted
}

for(i in 1:6)
{
  plot(arfima.1[[i]],type="l")
  lines(fit8[[i]],lty=2,col="blue")
  lines(sarima2[[i]],lty=2,col="red")
}


#Damped HoltWinters

fit9=list()
hwd3=list()
for(i in 1:6)
{
  hwd=ets(train_ts1[,i],model="ZAA",damped=T)
  hwd3[[i]]=forecast(hwd,150)$mean
  fit9[[i]]=hwd$fitted
}

for(i in 1:6)
{
  plot(arfima.1[[i]],type="l")
  lines(fit9[[i]],lty=2,col="blue")
  lines(hwd3[[i]],lty=2,col="red")
}


#Accuracy

mae7=list()
rmse7=list()
u7=list()
for(i in 1:6)
{
  mae7[[i]]=mae(test1[,i],HW3.forecast[[i]])
  rmse7[[i]]=rmse(test1[,i],HW3.forecast[[i]])
  u7[[i]]=TheilU(test1[,i],HW3.forecast[[i]])
}

mae7
rmse7
u7

mae8=list()
rmse8=list()
u8=list()
for(i in 1:6)
{
  mae8[[i]]=mae(test1[,i],sarima2[[i]])
  rmse8[[i]]=rmse(test1[,i],sarima2[[i]])
  u8[[i]]=TheilU(test1[,i],sarima2[[i]])
}

mae8
rmse8
u8

mae9=list()
rmse9=list()
u9=list()
for(i in 1:6)
{
  mae9[[i]]=mae(test1[,i],hwd3[[i]])
  rmse9[[i]]=rmse(test1[,i],hwd3[[i]])
  u9[[i]]=TheilU(test1[,i],hwd3[[i]])
}

mae9
rmse9
u9




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


#HoltWinters

HW4.forecast=list()
fit10=list()
for(i in 1:6)
{
  HW1=HoltWinters(train_ts1[,i])
  HW4.forecast[[i]]=forecast(HW1,150)$mean
  fit10[[i]]=HW1$fitted[,1]
}

par(mfrow=c(3,2))
for(i in 1:6)
{
  plot(tar.1[[i]],type="l")
  lines(fit10[[i]],lty=2,col="blue")
  lines(HW4.forecast[[i]],lty=2,col="red")
}



#SARIMA

fit11=list()
sarima3=list()
for(i in 1:6)
{
  sar=auto.arima(train_ts1[,i])
  sarima3[[i]]=forecast(sar,150)$mean
  fit11[[i]]=sar$fitted
}

for(i in 1:6)
{
  plot(tar.1[[i]],type="l")
  lines(fit11[[i]],lty=2,col="blue")
  lines(sarima3[[i]],lty=2,col="red")
}


#Damped HoltWinters

fit12=list()
hwd4=list()
for(i in 1:6)
{
  hwd=ets(train_ts1[,i],model="ZAA",damped=T)
  hwd4[[i]]=forecast(hwd,150)$mean
  fit12[[i]]=hwd$fitted
}

for(i in 1:6)
{
  plot(tar.1[[i]],type="l")
  lines(fit12[[i]],lty=2,col="blue")
  lines(hwd4[[i]],lty=2,col="red")
}



#Accuracy

mae10=list()
rmse10=list()
u10=list()
for(i in 1:6)
{
  mae10[[i]]=mae(test1[,i],HW4.forecast[[i]])
  rmse10[[i]]=rmse(test1[,i],HW4.forecast[[i]])
  u10[[i]]=TheilU(test1[,i],HW4.forecast[[i]])
}

mae10
rmse10
u10

mae11=list()
rmse11=list()
u11=list()
for(i in 1:6)
{
  mae11[[i]]=mae(test1[,i],sarima3[[i]])
  rmse11[[i]]=rmse(test1[,i],sarima3[[i]])
  u11[[i]]=TheilU(test1[,i],sarima3[[i]])
}

mae11
rmse11
u11

mae12=list()
rmse12=list()
u12=list()
for(i in 1:6)
{
  mae12[[i]]=mae(test1[,i],hwd4[[i]])
  rmse12[[i]]=rmse(test1[,i],hwd4[[i]])
  u12[[i]]=TheilU(test1[,i],hwd4[[i]])
}

mae12
rmse12
u12


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


#HoltWinters

HW5.forecast=list()
fit13=list()
for(i in 1:6)
{
  HW1=HoltWinters(train_ts1[,i])
  HW5.forecast[[i]]=forecast(HW1,150)$mean
  fit13[[i]]=HW1$fitted[,1]
}

par(mfrow=c(3,2))
for(i in 1:6)
{
  plot(setar.1[[i]],type="l")
  lines(fit13[[i]],lty=2,col="blue")
  lines(HW5.forecast[[i]],lty=2,col="red")
}



#SARIMA

fit14=list()
sarima4=list()
for(i in 1:6)
{
  sar=auto.arima(train_ts1[,i])
  sarima4[[i]]=forecast(sar,150)$mean
  fit14[[i]]=sar$fitted
}

for(i in 1:6)
{
  plot(setar.1[[i]],type="l")
  lines(fit14[[i]],lty=2,col="blue")
  lines(sarima4[[i]],lty=2,col="red")
}


#Damped HoltWinters

fit15=list()
hwd5=list()
for(i in 1:6)
{
  hwd=ets(train_ts1[,i],model="ZAA",damped=T)
  hwd5[[i]]=forecast(hwd,150)$mean
  fit15[[i]]=hwd$fitted
}

for(i in 1:6)
{
  plot(setar.1[[i]],type="l")
  lines(fit15[[i]],lty=2,col="blue")
  lines(hwd5[[i]],lty=2,col="red")
}

#Accuracy

mae13=list()
rmse13=list()
u13=list()
for(i in 1:6)
{
  mae13[[i]]=mae(test1[,i],HW5.forecast[[i]])
  rmse13[[i]]=rmse(test1[,i],HW5.forecast[[i]])
  u13[[i]]=TheilU(test1[,i],HW5.forecast[[i]])
}

mae13
rmse13
u13

mae14=list()
rmse14=list()
u14=list()
for(i in 1:6)
{
  mae14[[i]]=mae(test1[,i],sarima4[[i]])
  rmse14[[i]]=rmse(test1[,i],sarima4[[i]])
  u14[[i]]=TheilU(test1[,i],sarima4[[i]])
}

mae14
rmse14
u14

mae15=list()
rmse15=list()
u15=list()
for(i in 1:6)
{
  mae15[[i]]=mae(test1[,i],hwd5[[i]])
  rmse15[[i]]=rmse(test1[,i],hwd5[[i]])
  u15[[i]]=TheilU(test1[,i],hwd5[[i]])
}

mae15
rmse15
u15


#ARMA with trend
#linear


art1=ts(arma_tren1,frequency=12,start=c(1947,1))
train1=arma_tren1[1:850]
test1=arma_tren1[851:1000]
train_ts1=ts(train1,frequency=12,start=c(1947,1))
test_ts1=ts(test1,frequency=12,start=c(2013,9))

#HoltWinters
HW1=HoltWinters(train_ts1)
HW1.forecast=forecast(HW1,150)$mean

plot(art1,type="l")
lines(HW1$fitted[,1],lty=2,col="blue")
lines(HW1.forecast,lty=2,col="red")

#SARIMA

fit1=auto.arima(train_ts1)
arima_forecast1=forecast(fit1,150)$mean

plot(art1,type="l")
lines(fit1$fitted,lty=2,col="blue")
lines(arima_forecast1,lty=2,col="red")


#Damped HoltWinters

hw1=ets(train_ts1,model="ZAA",damped=T)
hwd1.forecast=forecast(hw1,150)$mean

plot(art1,type="l")
lines(hw1$fitted,lty=2,col="blue")
lines(hwd1.forecast,lty=2,col="red")

#Accuracy

accu1=list(mae(test1,HW1.forecast),rmse(test1,HW1.forecast),TheilU(test1,HW1.forecast))
accu2=list(mae(test1,arima_forecast1),rmse(test1,arima_forecast1),TheilU(test1,arima_forecast1))
accu3=list(mae(test1,hwd1.forecast),rmse(test1,hwd1.forecast),TheilU(test1,hwd1.forecast))



#quadratic

art2=ts(arma_tren2,frequency=12,start=c(1947,1))
train1=arma_tren2[1:850]
test1=arma_tren2[851:1000]
train_ts1=ts(train1,frequency=12,start=c(1947,1))
test_ts1=ts(test1,frequency=12,start=c(2013,9))

#HoltWinters
HW1=HoltWinters(train_ts1)
HW1.forecast=forecast(HW1,150)$mean

plot(art2,type="l")
lines(HW1$fitted[,1],lty=2,col="blue")
lines(HW1.forecast,lty=2,col="red")

#SARIMA

fit1=auto.arima(train_ts1)
arima_forecast1=forecast(fit1,150)$mean

plot(art2,type="l")
lines(fit1$fitted,lty=2,col="blue")
lines(arima_forecast1,lty=2,col="red")


#Damped HoltWinters

hw1=ets(train_ts1,model="ZAA",damped=T)
hwd1.forecast=forecast(hw1,150)$mean

plot(art2,type="l")
lines(hw1$fitted,lty=2,col="blue")
lines(hwd1.forecast,lty=2,col="red")

#Accuracy

accu1=list(mae(test1,HW1.forecast),rmse(test1,HW1.forecast),TheilU(test1,HW1.forecast))
accu2=list(mae(test1,arima_forecast1),rmse(test1,arima_forecast1),TheilU(test1,arima_forecast1))
accu3=list(mae(test1,hwd1.forecast),rmse(test1,hwd1.forecast),TheilU(test1,hwd1.forecast))



#ARMA with seasonality

ars=ts(arma_season1,frequency=12,start=c(1947,1))
train1=arma_season1[1:850]
test1=arma_season1[851:1000]
train_ts1=ts(train1,frequency=12,start=c(1947,1))
test_ts1=ts(test1,frequency=12,start=c(2013,9))


#HoltWinters
HW1=HoltWinters(train_ts1)
HW1.forecast=forecast(HW1,150)$mean

plot(ars,type="l")
lines(HW1$fitted[,1],lty=2,col="blue")
lines(HW1.forecast,lty=2,col="red")

#SARIMA

fit1=auto.arima(train_ts1)
arima_forecast1=forecast(fit1,150)$mean

plot(ars,type="l")
lines(fit1$fitted,lty=2,col="blue")
lines(arima_forecast1,lty=2,col="red")


#Damped HoltWinters

hw1=ets(train_ts1,model="ZAA",damped=T)
hwd1.forecast=forecast(hw1,150)$mean

plot(ars,type="l")
lines(hw1$fitted,lty=2,col="blue")
lines(hwd1.forecast,lty=2,col="red")

#Accuracy

accu1=list(mae(test1,HW1.forecast),rmse(test1,HW1.forecast),TheilU(test1,HW1.forecast))
accu2=list(mae(test1,arima_forecast1),rmse(test1,arima_forecast1),TheilU(test1,arima_forecast1))
accu3=list(mae(test1,hwd1.forecast),rmse(test1,hwd1.forecast),TheilU(test1,hwd1.forecast))


