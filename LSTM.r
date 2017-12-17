
# Maxime JUMELLE
# Youva MANSOUR
# Wassil BENTARKA

# Keras version for R is required to train a LSTM
# You must uncomment the three following lines in order to install Keras
library(zoo)
#install.packages("keras")
#install_keras()
library(keras)
library(ggplot2)

source("/home/maxime/Documents/R/TSA/load_data.r") # We include the source file that enables us to manipulate the data

# Creation of (X_t, X_{t-1}) matrix
values<-dataset$TEMP[1:2000]
airplane<-read.csv("/home/maxime/Documents/R/TSA/data/airplane.csv")
values<-airplane$Num

values<-na.approx(values)

N<-length(values)
batch_size<-1
epochs<-10

# Make the (X_t) time serie stationary
difference<-c()
for (i in 2:N)
{
  difference<-c(difference, values[i]-values[i-1])
}

# Scale to transform to [-1, 1] in order to feed neural network
values_min<-min(difference)
values_max<-max(difference)
std<-(difference[1] - values_min) / (values_max - values_min)
values_scaled<-2*std - 1
X_train<-c(2 * (-values_min / (values_max - values_min)) - 1, values_scaled)
for (i in 2:(N-1))
{
  std<-(difference[i] - values_min) / (values_max - values_min)
  X_train<-c(X_train, values_scaled, 2*std - 1)
  values_scaled<-2 * std - 1
}
X_train<-t(matrix(X_train, nrow=2))

model <- keras_model_sequential() 
model %>% 
  #layer_lstm(units = 4, input_shape = c(1, 2)) %>% 
  layer_lstm(units = 4, batch_input_shape = c(batch_size, 1, 1), stateful=TRUE) %>% 
  layer_dense(units = 1)
summary(model)

model %>% compile(
  loss = "mean_squared_error",
  optimizer = "adam",
  metrics="accuracy"
)

X_train_feed<-array(data=X_train[,1], dim=c(N, 1, 1))
Y_train_feed<-array(data=X_train[,2], dim=c(N, 1))

loss<-c()
for (i in 1:epochs)
{
  history <- model %>% fit(
    X_train_feed, Y_train_feed, 
    epochs = 1, verbose=1, batch_size=batch_size, shuffle=FALSE
  )
  model %>% reset_states()
  loss <-c(loss, history$metrics$loss)
  print(history$metrics$loss)
}

Y_hat<-model %>% predict(X_train_feed, batch_size=batch_size)
# Now we must map from [-1, 1] to R as we did the previous operation before
Y_hat_invert<-c()
Y_each_step<-c()
Y_cumsum<-c(values[1])
for (i in 1:length(Y_hat))
{
  Y_hat_invert<-c(Y_hat_invert, 0.5 * (values_min + values_max + (values_max - values_min) * Y_hat[i]))
  Y_each_step<-c(Y_each_step, values[i] + Y_hat_invert[i])
  if (i > 1) {
    Y_cumsum<-c(Y_cumsum, Y_cumsum[i-1] + Y_hat_invert[i])
  }
}

linearModel<-lm(values ~ ts(1:length(values)))
linearModel
predicted<-(ts(1:N)*linearModel$coefficients[2] + rep(linearModel$coefficients[1], times=N)) - Y_hat_invert*10

df <- data.frame(x = airplane$Date,y=airplane$Num)
ggplt<-ggplot(df,aes(x=x, y=y, group = 1)) + geom_line(aes(y=airplane$Num)) + geom_line(aes(y = Y_hat_invert), color = "blue")
ggplt<-ggplt + geom_abline(intercept=linearModel$coefficients[1], slope=linearModel$coefficients[2], color="red")
ggplt<-ggplt + geom_line(aes(y=Y_each_step), color="blue")
ggplt

ggplt<-ggplot(df,aes(x=x, y=y, group = 1)) + geom_line(aes(y=airplane$Num)) + geom_line(aes(y = Y_hat_invert), color = "blue")
ggplt<-ggplt + geom_abline(intercept=linearModel$coefficients[1], slope=linearModel$coefficients[2], color="red")
ggplt<-ggplt + geom_line(y=Y_cumsum, color="magenta")
ggplt

# for temperature
df <- data.frame(x = dataset$DATE[1:N],y=dataset$TEMP[1:N])
ggplt<-ggplot(df,aes(x=x, y=y, group = 1)) + geom_line(aes(y=dataset$TEMP[1:N])) + geom_line(aes(y=Y_hat_invert))
ggplt<-ggplt + geom_line(aes(y=Y_each_step), color="blue")
ggplt

ggplt<-ggplot(df,aes(x=x, y=y, group = 1)) + geom_line(aes(y=dataset$TEMP[1:N])) + geom_line(aes(y = Y_hat_invert), color = "blue")
ggplt<-ggplt + geom_abline(intercept=linearModel$coefficients[1], slope=linearModel$coefficients[2], color="red")
ggplt<-ggplt + geom_line(y=Y_cumsum, color="magenta")
ggplt

#df <- data.frame(x = 1:epochs, y=loss)
#ggplot(df,aes(x=x, y=y, colour = 'green', group = 1)) + geom_line(position = 'jitter')
ggplt
