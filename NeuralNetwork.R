install_keras()
library(keras)
# To import data
data <-read.csv('C:/Users/solan/OneDrive/Desktop/diabetes (1).csv')
str(data)
#removing rows with Null Values
for (i in 2:6)
data<-data[-which(data[, i] == 0),]
#converting data into matrix
data<- as.matrix(data)
str(data)
dimnames(data)<- NULL
str(data)
#normalizing data
data[,1:8]<-normalize(data[,1:8])
summary(data)
#splitting data into train and test set
sample <- sample.int(n=nrow(data), size = floor(0.2*nrow(data)))
#storing inputs and outputs separately
train <- data[-sample,1:8]
test <- data[sample,1:8]
trainingtarget<-data[-sample,9]
testtarget<-data[sample,9]
#create sequential model
model<-keras_model_sequential()
model%>%
layer_dense(units=8, input_shape = c(8))%>% #pipe function
layer_dense(units=8, activation='relu')%>%
layer_dropout(rate=0.4)%>%
layer_dense(units=8, activation='relu')%>%
layer_dropout(rate=0.2)%>%
layer_dense(units=8, activation='relu')%>%
layer_dense(units = 1, activation = 'sigmoid')
summary(model)
#compile
model %>%
compile(loss='binary_crossentropy',
optimizer = 'adam', metrics = 'accuracy')
#fit model
history <- model %>%
fit(train,trainingtarget, epochs=500, batch_size = '32', validation_split = 0.2)
plot(history)
#evaluate the model
model1<- model %>%
evaluate(test,testtarget)
#confusion matrix
pred<- model%>%
predict_classes(test)
table1<-table(Predicted=pred, Actual=testtarget)
cbind(pred, testtarget)
table1
model1
print(history)
rmse(testtarget,pred)
