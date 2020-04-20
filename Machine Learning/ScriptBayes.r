setwd("C:\\Users\\Paolo\\Desktop\\Progetto machine+DT")
dataset = data.frame(read.csv("mergedAdultCensus.csv", header=TRUE, sep=';'))
library(e1071)
library(ROCR)
library(plotrix)
#Library used for plotting the Pearson correlation's graph
#library(corrplot)

split.data <- function(data, p, s){
  set.seed(s)
  index = sample(1:dim(data)[1])
  train = data[index[1:floor(dim(data)[1] * p)], ]
  test = data[index[((ceiling(dim(data)[1] * p)) + 1):dim(data)[1]], ]
  return(list(train=train, test=test)) 
}

split.data.cross <- function(data){
  len = (dim(data)[1])
  data = data[sample(nrow(data)), ]
  gap = floor(len/10)
  result = list()
  for(i in 0:9){
    result[[i+1]] = data[(i*(gap)+1):((i+1)*gap), ]
  }
  return(result)
}

evaluate.model <- function(table){
  tp = table[1]
  tn = table[4]
  fn = table[2]
  fp = table[3]
  prec = tp / (tp + fp)
  rec = tp / (tp + fn)
  acc = (tp + tn) / (tp + tn + fp + fn)
  f.m = 2*((prec*rec)/(prec+rec))
  return(list(precision = prec, recall = rec, accuracy = acc, f.measure = f.m))
}

# Split Train and Test
random = 1.3
splitted = split.data(dataset, 0.7, random)
trainSet = splitted$train
testSet = splitted$test

# Code used for correlation's plot
# matrix.train = data.matrix(trainSet)
# correlation = cor(matrix.train, method = "pearson")
# corrplot(correlation, method="color", type = "upper")

# Create the first model and test it
classifier = naiveBayes(trainSet, trainSet$income_50k)
bayes.pred = predict(classifier, testSet)
# Confusion Matrix
bayes.table = table(bayes.pred, testSet$income_50k)
#AUC
pred.rocr = prediction(as.numeric(bayes.pred), testSet$income_50k)
perf.rocr = performance(pred.rocr, measure = "auc", x.measure = "cutoff")
no.cross.auc = perf.rocr@y.values[[1]]
perf.tpr.rocr = performance(pred.rocr, "tpr", "fpr")
plot(perf.tpr.rocr, colorize=T, main=paste("AUC:",(perf.rocr@y.values)))
#Other Indicators
evaluation = evaluate.model(bayes.table)
no.cross.prec = evaluation$precision
no.cross.rec = evaluation$recall
no.cross.acc = evaluation$accuracy
no.cross.f.mea = evaluation$f.measure

# Cross Preparation
trainSet = NULL
testSet = NULL
fold = split.data.cross(dataset)
cross.results = list()
train.index = c(2:10)
test.index = c(1)
# Cross Validation
for(i in 1:10){
  trainSet = c()
  for(t in train.index)
    trainSet = rbind(trainSet, fold[[t]])
  testSet = fold[[test.index]]
  #Train and Test
  classifier = naiveBayes(trainSet, trainSet$income_50k)
  bayes.pred = predict(classifier, testSet)
  #AUC
  pred.rocr = prediction(as.numeric(bayes.pred), testSet$income_50k)
  perf.rocr = performance(pred.rocr, measure = "auc", x.measure = "cutoff")
  auc.value = perf.rocr@y.values[[1]]
  # Confusion Matrix
  bayes.table = table(bayes.pred, testSet$income_50k)
  evaluation = evaluate.model(bayes.table)
  cross.results[[i]] = c(evaluation$precision, evaluation$recall, evaluation$accuracy, evaluation$f.measure, auc.value)
  #Indexes and Variable Update
  train.index = c(1:10)
  test.index = test.index + 1
  train.index = setdiff(train.index, test.index)
  trainSet = NULL
  testSet = NULL
}
#Merge the cross validation's result
final.precision = 0 
final.recall = 0
final.accuracy = 0
final.f.measure = 0
final.auc = 0
for(i in 1:10){
  final.precision = final.precision + cross.results[[i]][1]
  final.recall = final.recall + cross.results[[i]][2]
  final.accuracy = final.accuracy + cross.results[[i]][3]
  final.f.measure = final.f.measure + cross.results[[i]][4]
  final.auc = final.auc + cross.results[[i]][5]
}

final.precision = final.precision / 10
final.recall = final.recall / 10
final.accuracy = final.accuracy / 10
final.f.measure = final.f.measure / 10
final.auc = final.auc / 10



# Final Table
total = matrix(c(no.cross.prec, no.cross.rec, no.cross.acc, no.cross.f.mea, no.cross.auc,
                 final.precision, final.recall, final.accuracy, final.f.measure, final.auc),
               ncol = 5, byrow = TRUE)
colnames(total) = c("Precision", "Recall", "Accuracy", "F-Measure", "AUC")
rownames(total) = c("No Cross Validation", "Cross Validation")
total = as.table(total)
total

#Histogram
testdf = data.frame(Precision = c(no.cross.prec, final.precision),
                    Recall = c(no.cross.rec, final.recall),
                    Accuracy = c(no.cross.acc, final.accuracy),
                    FMeasure = c(no.cross.f.mea, final.f.measure),
                    AUC = c(no.cross.auc, final.auc)
)
rownames(testdf) = c("Red","Green")
barp(testdf,main = "Bayes Result", ylab = "Value", names.arg = colnames(testdf), col = 2:3)
legend("bottomright",legend=c("Cross V","Not Cross V"), fill = c("Green", "Red"), bty="h", ncol = 1, cex=0.7, pt.cex=0.7)