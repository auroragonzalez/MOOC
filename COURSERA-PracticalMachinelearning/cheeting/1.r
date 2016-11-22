require(data.table)
setInternet2(TRUE)
url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
D <- fread(url)


url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
DTest <- fread(url)


isAnyMissing <- sapply(DTest, function (x) any(is.na(x) | x == ""))
isPredictor <- !isAnyMissing & grepl("belt|[^(fore)]arm|dumbbell|forearm", names(isAnyMissing))
predCandidates <- names(isAnyMissing)[isPredictor]
predCandidates

varToInclude <- c("classe", predCandidates)
D <- D[, varToInclude, with=FALSE]
dim(D)

D <- D[, classe := factor(D[, classe])]
D[, .N, classe]

require(caret)

seed <- as.numeric(as.Date("2014-10-26"))
set.seed(seed)
inTrain <- createDataPartition(D$classe, p=0.6)
DTrain <- D[inTrain[[1]]]
DProbe <- D[-inTrain[[1]]]


X <- DTrain[, predCandidates, with=FALSE]
preProc <- preProcess(X)
preProc

XCS <- predict(preProc, X)
DTrainCS <- data.table(data.frame(classe = DTrain[, classe], XCS))


X <- DProbe[, predCandidates, with=FALSE]
XCS <- predict(preProc, X)
DProbeCS <- data.table(data.frame(classe = DProbe[, classe], XCS))



nzv <- nearZeroVar(DTrainCS, saveMetrics=TRUE)
if (any(nzv$nzv)) nzv else message("No variables with near zero variance")


# Train a prediction model


require(parallel)

require(doParallel)

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)


ctrl <- trainControl(classProbs=TRUE,
                     savePredictions=TRUE,
                     allowParallel=TRUE)



method <- "rf"
system.time(trainingModel <- train(classe ~ ., data=DTrainCS, method=method ))



stopCluster(cl)

# Evaluate the model on the training dataset
trainingModel

hat <- predict(trainingModel, DTrainCS)
confusionMatrix(hat, DTrain[, classe])

# Evaluate the model on the probing dataset

