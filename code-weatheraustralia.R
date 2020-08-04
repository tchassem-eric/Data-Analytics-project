#1.1) 
qplot(MinTemp, Location, data=WeatherAustralia, color="red") 

#1.2) 
plot(WeatherAustralia$Date, WeatherAustralia$Sunshine)

#1.3)
library(ggmap)
register_google(key = "AIzaSyAF_2zOK7X5TM8lH07jXDgovWFULdJfFKg")
myMap <- get_map(location = "Australia", zoom = 4)

plot(myMap)

ggmap(myMap)+
  geom_point(aes(x =longitude , y = latitude, size=sqrt(Rainfall)),
             data = WeatherAustralia, alpha = .5, color="darkred")

#1.4) 
boxplot(WeatherAustralia$Cloud3pm ~ WeatherAustralia$RainTomorrow, main="rainTomorrow&cloud3pm", xlab="RainTomorrow", ylab="cloud3pm", col=rainbow(7)) 

#1.5) 
plot(WeatherAustralia$MinTemp, WeatherAustralia$MaxTemp)
abline(lm(WeatherAustralia$MaxTemp ~ WeatherAustralia$MinTemp))

plot(WeatherAustralia$Pressure9am, WeatherAustralia$Pressure3pm)
abline(lm(WeatherAustralia$Pressure3pm ~ WeatherAustralia$Pressure9am))

assocplot(table(WeatherAustralia$Cloud9am, WeatherAustralia$Cloud3pm))

#trees
library(rpart)

rpart.Training <- rpart( RainTomorrow~MaxTemp+MinTemp+Pressure9am+Pressure3pm, data=qualityTrain)
plot(rpart.Training , uniform=FALSE)
text(rpart.Training )

#generalized linear model (Splitting the database)
training_begin <- as.Date("2013-01-01")
training_end <- as.Date("2015-01-13")
test_end <- as.Date("2018-07-30")
qualityTrain <- WeatherAustralia[WeatherAustralia$Date >= training_begin & WeatherAustralia$Date < training_end,]
dim(qualityTrain)[1]
#size = 4879
qualityTest <- WeatherAustralia[WeatherAustralia$Date >= training_end & WeatherAustralia$Date <= test_end,]
dim(qualityTest)[1]
#size = 4880
QualityLog = glm(RainTomorrow ~ MaxTemp+MinTemp+Pressure9am+Pressure3pm,data=qualityTrain, family=binomial)
summary(QualityLog)
predictTest = predict(QualityLog, type = "response", newdata = qualityTest)
#confusion matrix we used a threshold value of 0.3
table(qualityTest$RainTomorrow,predictTest >= 0.3)
  
#FALSE TRUE
#No   3225  599
#Yes   446  610
#error test is (446+599)/(3225+599+446+610) = 0.2093

library(ROCR)
ROCRpred = prediction(predictTest, qualityTest$RainTomorrow)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutofs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
predictTest = predict(QualityLog, type = "response", newdata = qualityTest)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Deep Neural Network
library(magrittr)

#install_keras()
library(keras)

NNDatabase <- WeatherAustralia

# Converting the categorical variable into binary variable
NNDatabase$RainTomorrow <- as.numeric(ifelse(WeatherAustralia$RainTomorrow == "Yes",1,0))
NNDatabase$RainToday <- as.numeric(ifelse(WeatherAustralia$RainToday == "Yes",1,0))

#We normalize the data
NNDatabase$MinTemp <- c(normalize(NNDatabase$MinTemp))
NNDatabase$MaxTemp <- c(normalize(NNDatabase$MaxTemp))
NNDatabase$Rainfall <- c(normalize(NNDatabase$Rainfall))
NNDatabase$Evaporation <- c(normalize(NNDatabase$Evaporation))
NNDatabase$Sunshine <- c(normalize(NNDatabase$Sunshine))
NNDatabase$WindGustSpeed <- c(normalize(NNDatabase$WindGustSpeed))
NNDatabase$WindSpeed9am <- c(normalize(NNDatabase$WindSpeed9am))
NNDatabase$WindSpeed3pm <- c(normalize(NNDatabase$WindSpeed3pm))
NNDatabase$Humidity9am <- c(normalize(NNDatabase$Humidity9am))
NNDatabase$Humidity3pm <- c(normalize(NNDatabase$Humidity3pm))
NNDatabase$Pressure9am <- c(normalize(NNDatabase$Pressure9am))
NNDatabase$Pressure3pm <- c(normalize(NNDatabase$Pressure3pm))
NNDatabase$Cloud9am <- c(normalize(NNDatabase$Cloud9am))
NNDatabase$Cloud3pm <- c(normalize(NNDatabase$Cloud3pm))
NNDatabase$Temp9am <- c(normalize(NNDatabase$Temp9am))
NNDatabase$Temp3pm <- c(normalize(NNDatabase$Temp3pm))

# Preparing the dataset
x_train <- NNDatabase[c(1:4000), -c(1,2, 21, 20, 22)] %>% scale()
y_train <- NNDatabase[c(1:4000), c(20)]

x_test <- NNDatabase[c(5001:9001), -c(1,2, 21, 20, 22)] %>% scale()
y_test <- NNDatabase[c(5001:9001), c(20)]

#------------------model with regularizer -----------------------------

l2_model <- keras_model_sequential() %>%
  layer_dense(units = 16, kernel_regularizer = regularizer_l2(0.001),
              activation = "relu", input_shape = c(17)) %>%
  layer_dense(units = 16, kernel_regularizer = regularizer_l2(0.001),
              activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

l2_model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

##-------------------------- Fitting models-------------------------------------

l2_model_hist <- l2_model %>% fit(
  x_train, y_train,
  epochs = 40,
  batch_size = 512,
  validation_data = list(x_test, y_test)
)

##-------------------------- check the Results-------------------------------------
str(l2_model_hist)

