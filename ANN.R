#Artificial Neural Network on concrete.csv
#We are making a multi-layer feedforward neural network
concrete<-read.csv(file.choose(),header=T, stringsAsFactors = F)
View(concrete)
str(concrete)
#plot(concrete)
normalize<-function(x){
  (x-min(x))/(max(x)-min(x))
}
concrete_norm<-as.data.frame(lapply(concrete,normalize))
summary(concrete_norm$superplastic)
summary(concrete_norm$strength)
concrete_train <- concrete_norm[1:773, ]
concrete_test <- concrete_norm[774:1030, ]
install.packages("neuralnet")
library(neuralnet)
model<-neuralnet(strength~ cement+slag+ash+water+superplastic+coarseagg+fineagg+age, data = concrete_train)
plot(model)
p<-compute(model,concrete_test)
predicted_strength <- p$net.result

"Because this is a numeric prediction problem rather than a classification problem, we
cannot use a confusion matrix to examine model accuracy. Instead, we must measure
the correlation between our predicted concrete strength and the true value. This
provides insight into the strength of the linear association between the two variables."

cor(predicted_strength, concrete_test$strength) 

#IMPROVING MODEL PERFORMANCE

model<-neuralnet(strength~ cement+slag+ash+water+superplastic+coarseagg+fineagg+age, data = concrete_train, hidden = 7)
plot(model)
p<-compute(model,concrete_test)
predicted_strength <- p$net.result
cor(predicted_strength, concrete_test$strength) 
