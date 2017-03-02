# Simple learner param set
simple.lrn.par.set = makeLrnPsSets(learner = makeLearner("classif.rpart", predict.type = "prob"), 
  param.set = makeParamSet(
    makeNumericParam("cp", lower = 0, upper = 1),
    makeIntegerParam("maxdepth", lower = 1, upper = 30),
    makeIntegerParam("minbucket", lower = 1, upper = 60),
    makeIntegerParam("minsplit", lower = 1, upper = 60)))

simple.lrn.par.set = makeLrnPsSets(learner = makeLearner("classif.glmnet", predict.type = "prob"),
  param.set = makeParamSet(
    makeNumericParam("alpha", lower = 0, upper = 1),
    makeNumericVectorParam("lambda", len = 1L, lower = -10, upper = 10, trafo = function(x) 2^x)), 
  lrn.ps.sets = simple.lrn.par.set)  

# increase to a general param set
lrn.par.set = makeLrnPsSets(learner = makeLearner("classif.svm", predict.type = "prob"), 
  param.set = makeParamSet(
    makeDiscreteParam("kernel", values = c("linear", "polynomial", "radial")),
    makeNumericParam("cost", lower = -10, upper = 10, trafo = function(x) 2^x),
    makeNumericParam("gamma", lower = -10, upper = 10, trafo = function(x) 2^x, requires = quote(kernel == "radial")),
    makeIntegerParam("degree", lower = 2, upper = 5, requires = quote(kernel == "polynomial"))),
  lrn.ps.sets = simple.lrn.par.set)

lrn.par.set = makeLrnPsSets(learner = makeLearner("classif.kknn", predict.type = "prob"), 
  param.set = makeParamSet(
    makeIntegerParam("k", lower = 1, upper = 30)),
  lrn.ps.sets = lrn.par.set)
