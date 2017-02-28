library(BBmisc)
  
#OML tasks
load("omlTaskIds.RData")

# Define tuning parameters for different learners
# SVM
svm.ps = makeParamSet(
  makeDiscreteParam("kernel", values = c("linear", "polynomial", "radial")),
  makeNumericParam("cost", lower = -10, upper = 10, trafo = function(x) 2^x),
  makeNumericParam("gamma", lower = -10, upper = 10, trafo = function(x) 2^x, requires = quote(kernel == "radial")),
  makeIntegerParam("degree", lower = 2, upper = 5, requires = quote(kernel == "polynomial"))
)

svm.des = generateRandomDesign(100, svm.ps)
svm.des$mlr.lrn.name = "classif.svm"

#KNN
knn.ps = makeParamSet(
  makeIntegerParam("k", lower = 1L, upper = 30L)
)

knn.des = generateRandomDesign(n = 20L, knn.ps)
knn.des = unique(knn.des)
knn.des$mlr.lrn.name = "classif.kknn"

#rpart
rpart.ps = makeParamSet(
  makeNumericParam("cp", lower = 10^-4, upper = 10^-1, trafo = function(x) 2^x),
  makeIntegerParam("minsplit", lower = 2L^0, upper = 2L^7, trafo = function(x) 2^x),
  makeIntegerParam("minbucket", lower = 2L^0, upper = 2L^6, trafo = function(x) 2^x)
)

rpart.des = generateRandomDesign(100, rpart.ps)
rpart.des$mlr.lrn.name = "classif.rpart"

#Create final design
learner.list = list(knn.des, svm.des, rpart.des)
des = rbindlist(learner.list, use.names = TRUE, fill = TRUE, idcol = FALSE)
des = convertDataFrameCols(des, factors.as.char = TRUE)


