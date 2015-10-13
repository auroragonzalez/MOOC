data(iris)
library("ggplot2")
names(iris)
table(iris$Species)
inTrain <- createDataPartition(y=iris$Species, p=0.7,list=F)
training <- iris[inTrain,]
testing <- iris[-inTrain,]


qplot(Petal.Width, Sepal.Width, colour= Species, data=training )
modelFit <- train(Species ~.,data=training, method="rpart")
print(modelFit$finalModel)


plot(modelFit$finalModel, uniform=T)
text(modelFit$finalModel, use.n=T, all=T, cex=0.8)
library(rattle)
fancyRpartPlot(modelFit$finalModel)


predict(modelFit,newdata=testing)

