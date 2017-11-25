
# Maxime JUMELLE
# Youva MANSOUR
# Wassil BENTARKA

# Keras version for R is required to train a LSTM
# You must uncomment the three following lines in order to install Keras
#install.packages("keras")
#install_keras()
library(keras)

source("/home/maxime/Documents/R/ISUP/TSA/load_data.r") # We include the source file that enables us to manipulate the data

N<-2000
batch_size<-1

# Creation of (X_t, X_{t-1}) matrix
X_train<-c(0, dataset$TEMP[1])
for (i in 2:N)
{
  X_train<-c(X_train, dataset$TEMP[i-1], dataset$TEMP[i])
}
X_train<-t(matrix(X_train, nrow=2))

# Make the (X_t) time serie stationary
difference<-c()
inverted_difference<-c()
for (i in 1:(N-1))
{
  difference<-c(difference, dataset$TEMP[i]-dataset$TEMP[i+1])
  inverted_difference<-c(inverted_difference, dataset$TEMP[i+1])
}
difference
inverted_difference

# Scale to transform to [-1, 1] in order to feed neural network
diff_min<-min(difference)
diff_max<-max(difference)
difference_scaled<-c(0)
for (i in 1:(N-1))
{
  std<-(difference[i] - diff_min) / (diff_max - diff_min)
  difference_scaled[i+1]<-2 * std - 1
}
difference_scaled

model <- keras_model_sequential() 
model %>% 
  layer_lstm(units = 4, input_shape = c(1, 2)) %>% 
  layer_dense(units = 1)
summary(model)

model %>% compile(
  loss = "mean_squared_error",
  optimizer = 'rmsprop',
  metrics="accuracy"
)

Y_train<-matrix(difference_scaled, ncol=1)

#X_train_feed<-array(X_train, dim=c(1, N, 2))
X_train_feed<-array(data=X_train, dim=c(N, 1, 2))
Y_train_feed<-array(data=Y_train, dim=c(N, 1))
#Y_train_feed<-Y_train
history <- model %>% fit(
  X_train_feed, Y_train_feed, 
  epochs = 10, batch_size=batch_size
)

plot(history)
