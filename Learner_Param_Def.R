library(BBmisc)
  
#OML tasks
load("omlTaskIds.RData")

# Define tuning parameters for different learners
#DF.LEARNER.PARAM.SPECS 

#knn.params = makeParamSet(makeIntegerParam("k", lower = 1L, upper = 30L))
#knn.params = generateRandomDesign(n = 20L, knn.params)
#knn.params = unique(knn.params)
#knn.params$mlr.lrn.name = "classif.kknn"

ps = makeParamSet(
  makeDiscreteParam("kernel", values = c("linear", "polynomial", "radial")),
  makeNumericParam("cost", lower = -10, upper = 10, trafo = function(x) 2^x),
  makeNumericParam("gamma", lower = -10, upper = 10, trafo = function(x) 2^x, requires = quote(kernel == "radial")),
  makeIntegerParam("degree", lower = 2, upper = 5, requires = quote(kernel == "polynomial")),
  makeNumericParam("predict.threshold", lower = 0, upper = 1)
)

des = generateRandomDesign(100, ps)


des$mlr.lrn.name = "classif.svm"
des = convertDataFrameCols(des, factors.as.char = TRUE)

#svm.params = makeParamSet(makeIntegerParam("degree", lower = 2L, upper = 4L),
#  makeNumericParam("gamma", lower = 2^-10, upper = 2^10))

