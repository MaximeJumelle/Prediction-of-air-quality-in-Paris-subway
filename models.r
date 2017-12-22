dataset <- read.csv("C:\\Users\\etudiant\\AppData\\Local\\Temp\\RtmpkJuL9Y\\data143c10d140d0", sep=";")

library(xts)
library(fGarch)
install.packages("devtools")

dataset<-dataset[order(dataset$HEURE),]
dataset<-dataset[order(as.Date(dataset$DATE, format="%d/%m/%Y")),]



dataset$TEMP<-as.numeric(dataset$TEMP)
dataset$HUMI<-as.numeric(dataset$HUMI)
dataset$NO<-as.numeric(dataset$NO)
dataset$NO2<-as.numeric(dataset$NO2)
dataset$PM10<-as.numeric(dataset$PM10)
dataset$CO2<-as.numeric(dataset$CO2)

dates<-as.Date(dataset$DATE[1:216],"%d/%m/%Y")
heure<-dataset$HEURE[1:216]
temp<-dataset$TEMP[1:216]

par(mfrow=c(1,1))

ts.plot(temp)
ts.plot(diff(((temp))))
d<-diff(temp)
# trend

par(mfrow=c(1,1))
temp<-dataset$TEMP[1:216]
var<-c(1:216)

model<-lm(temp~var,na.action=NULL) #regression lineaire temp sur temps
summary(model)  #affichage des coefficients
ts.plot(temp)   
abline(model,col=2)   #display time series + trend

par(mfrow=c(2,1))

ts.plot(resid(model))
ts.plot(diff(temp))


#seasonality

par(mfrow=c(2,1))
S = t(matrix(data = temp, nrow = 24))  #periode de 24 (1 journ?e)
St = rep(colMeans(S, na.rm = T),ceiling(216/24))
ts.plot(St)

ts.plot(temp-St)

#outliers

d<-diff(temp)
outliers<-which(abs(d)>0.85)
d[outliers]<-mean(d[outliers]+c(-1,1))
ts.plot(d)


temp<-dataset$TEMP[1:200]
#AR model
pacf(d)  #ressemble AR(1)
arima(d,order=c(1,0,0))
fitar<-arima(d,order=c(1,0,0))
ts.plot(arima.sim(list(order=c(1,0,0),ar=0.4323),n=200))   #mieux avec +
ts.plot(arima.sim(list(order=c(1,0,0),ar=-0.4323),n=200))
ts.plot(d)
tsdiag(fitar)

#MA model

acf(d)
arima(d,order=c(0,0,1))
fitma<-arima(d,order=c(0,0,1))
ts.plot(arima.sim(list(order=c(0,0,1),ma=0.4119),n=216))     #mieux avec +
ts.plot(arima.sim(list(order=c(0,0,1),ma=-0.4119),n=216))
ts.plot(d)
tsdiag(fitma)

#ARMA model

library(TSA)
d<-d[1:200]
par(mfrow=c(1,1))
BICs<-armasubsets(y=d,nar=5,nma=5)
plot(BICs)
arima(d,order=c(1,0,2))
fitarma<-arima(d,order=c(1,0,2))
par(mfrow=c(1,1))
ts.plot(arima.sim(list(order=c(1,0,2),ar=0.6586, ma=c(-0.2197,-0.1192)), n=200))
ts.plot(d)
tsdiag(fitarma)


BIC(fitar,fitma,fitarma) #Bic min pour ar(1)

par(mfrow=c(1,1))
eps<-fitar$residuals
plot(eps)
acf(eps)
qqnorm(eps)
qqline(eps,col=2)    #courbe assez proche c'est presque gaussien
acf(eps^2)


#GARCH

objf.garch <- function(vartheta, eps,n,sig2init,petit=sqrt(.Machine$double.eps),r0=10){
  omega <- vartheta[1]
  alpha <- vartheta[2]
  beta <- vartheta[3]
  sig2<-rep(0,n)
  sig2[1]<-sig2init
  for(t in 2:n){
    sig2[t]<-omega+alpha*eps[t-1]^2+beta*sig2[t-1]
  }
  qml <- mean(eps[(r0+1):n]^2/sig2[(r0+1):n]+log(sig2[(r0+1):n]))
  qml }

VarAsymp<- function(omega,alpha,beta,eps,sig2init,petit,r0=10){
  n <- length(eps)
  dersigma2<-matrix(0,nrow=3,ncol=n)
  sig2<-rep(0,n)
  sig2[1]<-sig2init
  for(t in 2:n){
    vec<-c(1,eps[t-1]^2,sig2[t-1])
    sig2[t]<-omega+beta*sig2[t-1]+alpha*eps[t-1]^2
    dersigma2[1:3,t]<-vec/sig2[t]+beta*dersigma2[1:3,(t-1)]
  }
  eta <- eps[(r0+1):n]/sqrt(sig2)[(r0+1):n]
  eta <- eta/sd(eta)
  
  J<-dersigma2[1:3,(r0+1):n]%*%t(dersigma2[1:3,(r0+1):n])/(n-r0)
  kappa4<-mean(eta^4)
  
  {if(kappa(J)<1/petit) inv<-solve(J) else inv<-matrix(0,nrow=3,ncol=3)}
  var<-(kappa4-1)*inv
  list(var=var,residus=eta)
}

estimGARCH<- function(omega,alpha,beta,eps,petit=sqrt(.Machine$double.eps),r0=10)
{
  valinit<-c(omega,alpha,beta)
  n <- length(eps)
  sig2init<-var(eps[1:min(n,5)])
  res <- nlminb(valinit,objf.garch,lower=c(petit,0,0),
                upper=c(Inf,Inf,1), eps=eps,n=n,sig2init=sig2init)
  omega <- res$par[1]
  alpha<- res$par[2]
  beta <- res$par[3]
  var<-VarAsymp(omega,alpha,beta,eps,sig2init,petit=sqrt(.Machine$double.eps),r0=10)
  list(coef=c(omega,alpha,beta),residus=var$residus,var=var$var)
}


#Estimation

omega.init<- 0
alpha.init<-0.01
beta.init<- 0
factor<-1
fitgarch<-estimGARCH(omega.init,alpha.init,beta.init,eps)
par<-fitgarch$coef
res<-fitgarch$residus
qqnorm(res)
qqline(eps,col=2)
acf(res^2)

#test beta=0

beta=par[3]
sigmabeta=fitgarch$var[3,3]
se=sqrt(sigmabeta)/sqrt(length(d))
t.value=beta/se
p.value=pnorm(-t.value)
print(p.value)   #0.4997319 > 0.05 on ne rejette pas beta=0


#ARCH(1)

estimARCH<- function(omega,alpha,beta=0,eps,petit=sqrt(.Machine$double.eps),r0=10)
{
  valinit<-c(omega,alpha,beta)
  n <- length(eps)
  sig2init<-var(eps[1:min(n,5)])
  res <- nlminb(valinit,objf.garch,lower=c(petit,0,0),
                upper=c(Inf,Inf,0), eps=eps,n=n,sig2init=sig2init)
  omega <- res$par[1]
  alpha<- res$par[2]
  beta <- res$par[3]
  var<-VarAsymp(omega,alpha,beta,eps,sig2init,petit=sqrt(.Machine$double.eps),r0=10)
  list(coef=c(omega,alpha,beta),residus=var$residus,var=var$var)
}
omega.init<- 0
alpha.init<-0.01
beta.init<- 0
factor<-1
fitgarch<-estimARCH(omega.init,alpha.init,eps=eps)
par<-fitgarch$coef
res<-fitgarch$residus

qqnorm(res)
qqline(eps,col=2)

#mauvais qqnorm pour arch on perefere ar

#test alpha=0

alpha=par[2]
sigmaalpha=fitgarch$var[2,2]
se=sqrt(sigmaalpha)/sqrt(length(d))
t.value=alpha/se
p.value=pnorm(-t.value)
print(p.value) #0.08629463 alpha nul

# ar1 arch1

library(rugarch)

#model<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),
#                  mean.model = list(armaOrder = c(1, 0), include.mean = FALSE),
#                  distribution.model = "norm")

#modelfit<-ugarchfit(spec=model,data=d)
#par(mfrow=c(1,1))
#plot(modelfit)                    we don't use the garch model as we have white noises





predict<-predict(fitar,n.ahead=10)
predint<-cbind(predict$pred-1.96*predict$se,predict$pred+1.96*predict$se)
ts.plot(as.ts(c(d[1:190],rep(NA,10))),c(rep(NA,190),predint[,1]),c(rep(NA,190),predint[,2]),col=c(1,2,3))



