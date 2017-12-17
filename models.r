
# Maxime JUMELLE
# Youva MANSOUR
# Wassil BENTARKA

library(zoo)
source("/home/maxime/Documents/R/TSA/load_data.r") # We include the source file that enables us to manipulate the data

# We load the temperature
temperature<-dataset$TEMP[1:2000]
dates<-dataset$DATE[1:2000]
N<-length(temperature)
temperature<-na.approx(temperature) # We interpolate the NA values

# We find the linear tendancy
var<-ts(1:N)
linearMod <- lm(temperature~var)
linearMod

# We plot the current data with the linear model
df <- data.frame(x = dates,y=temperature)
currentPlot <-ggplot(df,aes(x=x, y=y, group = 1)) + geom_line(aes(y=temperature))
currentPlot<-currentPlot + geom_abline(intercept = linearMod$coefficients[1], slope = linearMod$coefficients[2], color = "blue")
currentPlot

# To study seasonality, we want to remove the linear tendancy
# With that, we only have the values of seasonality component with random and original data
untrended<-temperature - (var*linearMod$coefficients[2] + rep(linearMod$coefficients[1], times=N))

# We plot the seasonality
df <- data.frame(x = dates,y=untrended)
currentPlot <-ggplot(df,aes(x=x, y=y, group = 1)) + geom_line(aes(y=untrended))
currentPlot

# We now want to remove seasonality
# MA model
acf(temperature, lag.max=100)

which.max(abs(pacf(temperature,lag.max=100)$acf))
arima(temperature, order=c(1,0,0))

# AR model
arima(data,order=c(0,0,1))