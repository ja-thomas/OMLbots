# Define tuning parameters for different learners
DF.LEARNER.PARAM.SPECS 

knn.params = makeParamSet(makeIntegerParam("k", lower = 1L, upper = 30L))
knn.params = generateRandomDesign(n = 20L, knn.params)
knn.params = unique(knn.params)
knn.params$mlr.lrn.name = "classif.kknn"

getParamSet(makeLearner("classif.svm"))
svm.params = makeParamSet(makeIntegerParam("degree", lower = 2L, upper = 4L),
  makeNumericParam("gamma", lower = 2^-10, upper = 2^10))

