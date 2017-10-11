# How to write a bot
library(OpenML)
library(mlr)
# Provide dataset(s)
task = getOMLTask(3)
# Provide learner(s)
lrn = makeLearner("classif.glmnet")
# Provide parameter setting(s) with ranges (with help of ParamHelpers)
lrn.par.set = makeParamSet(
    makeNumericParam("alpha", lower = 0, upper = 1, default = 1),
    makeNumericVectorParam("lambda", len = 1L, lower = -10, upper = 10, 
      default = 0 ,trafo = function(x) 2^x))
# Specify the number of runs
nr.runs = 10
# Draw (random) hyperparameters
par.sets = generateRandomDesign(nr.runs, lrn.par.set, trafo = TRUE)
par.sets = BBmisc::convertDataFrameCols(par.sets, factors.as.char = TRUE)

# If you have several learners or tasks, you have to write more for-loops here
for (i in 1:nr.runs) {
  print(i)
  par.set = as.list(par.sets[i,])
  mlr.lrn = setHyperPars(lrn, par.vals = par.set)
  res = runTaskMlr(task, mlr.lrn)
  print(res)
  tags = c("mySimpleBot", "v1")
  uploadOMLRun(res, confirm.upload = FALSE, tags = tags, verbosity = 1)
}