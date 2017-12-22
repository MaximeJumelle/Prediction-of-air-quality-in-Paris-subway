# Maxime JUMELLE
# Youva MANSOUR
# Wassil BENTARKA

# You must install this package to read the dataset file
#install.packages("rjson")
library(purrr)
library(ggplot2)

dataset<-read.csv("/home/maxime/Documents/R/TSA/data/dataset.csv", sep=";", header= TRUE)

dataset<-dataset[order(as.Date(dataset$DATE, format="%d/%m/%Y")),]
dataset$TEMP<-as.numeric(dataset$TEMP)
dataset$HUMI<-as.numeric(dataset$HUMI)
dataset$NO<-as.numeric(dataset$NO)
dataset$NO2<-as.numeric(dataset$NO2)
dataset$PM10<-as.numeric(dataset$PM10)
dataset$CO2<-as.numeric(dataset$CO2)

dates<-as.Date(dataset$DATE, "%d/%m/%Y")
par(mfrow=c(1,1))

jumpValue<-50
index<-1
datesJump<-c()
tempJump<-c()
while (index < length(dates))
{
  datesJump<-c(datesJump, dates[index])
  tempJump<-c(tempJump, dataset$TEMP[index])
  index<-index + jumpValue
}

df <- data.frame(x = dates[0:35039],y=dataset$PM10[0:35039])
#df <- data.frame(x = datesJump,y=tempJump)
ggplot(df,aes(x=x,y=y)) + geom_line(position = 'jitter')

