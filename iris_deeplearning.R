install.packages("neuralnet")
install.packages("mltools")
install.packages("data,table")
install.packages("caret", dependencies = T)
install.packages("lubridate")
library(neuralnet)
library(mltools)
library(data.table)
library(caret)

iris2 = scale(iris[,1:4])
iris2 = as.data.frame(iris2)
iris2$Species = iris$Species 
iris2

set.seed(1234)
partition = createDataPartition(1:dim(iris2)[1], p = .7)
iristrain = iris2[partition$Resample1,]
iristest = iris2[- partition$Resample1,]
dim(iristrain)
dim(iristest)

iristrain = cbind(iristrain[,1:4], one_hot(as.data.table(iristrain[,5])))
iristrain

model = neuralnet( V1_setosa  + V1_versicolor  +  V1_virginica  ~ Sepal.Length + Sepal.Width +  Petal.Length + Petal.Width , iristrain, hidden=c(5,4))
print(model)
plot(model)

test = compute(model, iristest[,1:4])
test$net.result
result = as.data.frame(test$net.result)
result 

names(result)[1] <- 'setosa'
names(result)[2] <- 'versicolor'
names(result)[3] <- 'virginica'

result$class = colnames(result[,1:3])[max.col(result[,1:3], ties.method = 'first')]
result

confusion = table(result$class, iristest$Species)
confusion 
sum(diag(confusion) * 100 / sum(confusion))