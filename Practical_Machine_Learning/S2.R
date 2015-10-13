##VIDEO 1##
library("caret")
library("kernlab")
data(spam)

inTrain <- createDataPartition(y=spam$type, p=0.75,list=F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)

set.seed(32343)
# Intento predecir el tipo usando todas las variables.
modelFit <- train(type ~.,data=training, method="glm")
modelFit

modelFit$finalModel
predictions <- predict(modelFit, newdata=testing)
confusionMatrix(predictions,testing$type)


##VIDEO 2##

# Antes he separado los datos por tipo 75% en training y 25% en el testing.
# Ahora k-fold
#Devuelve el train set:
set.seed(32323)
folds <- createFolds(y=spam$type, k=10,list=T, returnTrain = T)
sapply(folds,length)
folds[[1]][1:10]
folds[[2]]

#Devuelve el test set:
folds <- createFolds(y=spam$type, k=10,list=T, returnTrain = F)
sapply(folds,length)
folds[[1]][1:10]
folds[[2]][1:10]


# Ahora RESAMPLING
folds <- createResample(y=spam$type, times=10,list=T)
sapply(folds,length)
folds[[1]][1:10]

# Time slicing (si analizas datos que puede que uses para forecasting)
tme <- 1:1000
folds <- createTimeSlices(y=tme, initialWindow = 20, horizon=10)
names(folds)

folds$train[[1]]
folds$test[[1]]


##VIDEO 3## TRAINING OPTIONS
modelFit <- train(type ~.,data=training, method="glm")
modelFit

args(trainControl)

## VIDEO 4##
library("ISLR")
library("ggplot2")
data(Wage)
summary(Wage)

inTrain <- createDataPartition(y=Wage$wage, p=0.7,list=F)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training);dim(testing)

featurePlot(x=training[,c("age","education","jobclass")]
            ,y=training$wage,
            plot="pairs")

qplot(age,wage,data=training)
qplot(age,wage,data=training,color=jobclass)
qq <- qplot(age,wage,data=training,color=education)
qq + geom_smooth(method="lm",formula=y~x)


#Quieres cortar la variable wage en diferentes categorías:
library("Hmisc")
cutWage <- cut2(training$wage,g=3)
table(cutWage)


p1 <- qplot(cutWage,age,data=training,fill=cutWage,geom=c("boxplot"))
p1

p2 <- qplot(cutWage,age,data=training,fill=cutWage,geom=c("boxplot","jitter"))
p2

t1<-table(cutWage,training$job)
t1
t2<-prop.table(t1,1)
t2
qplot(wage,colour=education,data=training,geom="density")

## V5: Preprocessin##


inTrain <- createDataPartition(y=spam$type, p=0.75,list=F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
hist(training$capitalAve)
# La variables es muy skew: muchas observaciones pequeñitas y algunas más grndes
mean(training$capitalAve);sd(training$capitalAve)

# Se estandarizan restando su media y dividiendo por la sd 
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve-mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveS);sd(trainCapAveS)

# Para standarizar el test set hay que hacerlo con la media y sd del training set1
testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve-mean(trainCapAve))/sd(trainCapAve)
mean(testCapAveS);sd(testCapAveS)

# Función depreprocesamiento más general
preObj <- preProcess(training[,-58], method=c("center","scale"))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
mean(trainCapAveS);sd(trainCapAveS)

testCapAveS <- predict(preObj, testing[,-58])$capitalAve
mean(testCapAveS);sd(testCapAveS)


modelFit <- train(type ~.,data=training
                  ,preProcess=c("scale", "center")
                  , method="glm")
modelFit

# La transformación Box-Cox coge datos continuos y los intenta normalizar


preObj <- preProcess(training[,-58], method=c("BoxCox"))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
par(mfrow=c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)


## VIDEO COVARIANTE CREACION

spam$capitalAveSq <- spam$capitalAve^2

#COnvertir una variable en dummy
table(training$jobclass)

dummies <- dummyVars(wage~jobclass,data = training)
head(predict(dummies,newdata=training))

# Para predecir en vez de glm(generalized linear models) líneas curvas:
library("splines")
bsBasis <- bs(training$age,df=3)
bsBasis


lm(wage~bsBasis, data=training)

## Video PCA
##Algunas variables explican lo mismico que otra

library("caret")
library("kernlab")
data(spam)

inTrain <- createDataPartition(y=spam$type, p=0.75,list=F)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

M <- abs(cor(training[,-58]))
diag(M) <-0
which(M>0.8,arr.ind=T)
names(spam)[c(34,32)]
plot(spam[,34],spam[,32])

#Estas dos variables son casi iguales. ¿Cómo las "junto"?
X <- 0.71*training$num415 + 0.71*training$num857
Y <- 0.71*training$num415 - 0.71*training$num857
plot(X,Y)

# Por el plot, cogemos la suma (más lineal) que la resta (Y)

smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)
plot(prComp$x[,1],prComp$x[,2])

#Lo de rotation te devuelve los componentes de la combinación lineal!
prComp$rotation

typeColor <- ((spam$type=="spam")*1+1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2],col=typeColor)
plot(prComp$x[,1],prComp$x[,3],col=typeColor)



preProc <- preProcess(log10(spam[,-58]+1),metho="pca",pcaComp=2)
spamPC <- predict(preProc, log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)


trainPC <- predict(preProc, log10(spam[,-58]+1))
modelFit <- train(training$type~. , method="glm",data=trainPC)


## Predicting with regression
library("caret")
data(faithful)
set.seed(333)
inTrain <- createDataPartition(y=faithful$waiting, p=0.5,list=F)
trainFaith <- faithful[inTrain,]
testFaith <- faithful[-inTrain,]


plot(trainFaith$eruptions,trainFaith$waiting)

lm1<-lm(eruptions~waiting, data=trainFaith)
summary(lm1)

plot(trainFaith$waiting,trainFaith$eruptions)
lines(trainFaith$waiting, lm1$fitted,lwd=3)

coef(lm1)[1] + coef(lm1)[2]*80

newdata <- data.frame(waiting=80)
predict(lm1,newdata)
