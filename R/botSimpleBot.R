# How to write a bot
library(OpenML)
library(mlr)
# Provide dataset(s)
task = getOMLTask(task.id = 3)
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

# Put the results into a table
# get performances
#my_runs = listOMLRuns(tag = "mySimpleBot")
my_runs = listOMLRunEvaluations(tag = "mysimpleBot")
# subset results on some information(s) and measure(s)
my_runs = my_runs[, c("run.id", "task.id", "area.under.roc.curve" )]
# get hyperparameters
parameters = list()
for(i in 1:nrow(my_runs)) {
  run_i = getOMLRun(my_runs$run.id[i])
  parameter_i = data.frame(getOMLRunParList(run_i))
  parameters[[i]] = c(my_runs$run.id[i], as.numeric(parameter_i$value))
}
parameters = do.call(rbind, parameters)
colnames(parameters) = c("run.id", parameter_i$name)
# Put things together
results = merge(my_runs, parameters, by = "run.id")
# Put it in a nice order
results = results[, c(setdiff(names(results), "area.under.roc.curve"), "area.under.roc.curve")]
# Now you can compare the performances of your different hyperparameters
print(results)


# Annex
# Get some informations about the dataset
data.id = task$input$data.set$desc$id
qualities = getOMLDataSetQualities(data.id = data.id)
qualities[qualities$name %in% c("NumberOfClasses", "NumberOfInstances"), ]
# Possible hyperparameter settings for other learners
# rpart
param.set.rpart = makeParamSet(
  makeNumericParam("cp", lower = 0, upper = 1, default = 0.01),
  makeIntegerParam("maxdepth", lower = 1, upper = 30, default = 30),
  makeIntegerParam("minbucket", lower = 1, upper = 60, default = 1),
  makeIntegerParam("minsplit", lower = 1, upper = 60, default = 20))
# kknn
param.set.kknn = makeParamSet(
  makeIntegerParam("k", lower = 1, upper = 30))
# svm
param.set.svm = makeParamSet(
  makeDiscreteParam("kernel", values = c("linear", "polynomial", "radial")),
  makeNumericParam("cost", lower = -10, upper = 10, trafo = function(x) 2^x),
  makeNumericParam("gamma", lower = -10, upper = 10, trafo = function(x) 2^x, requires = quote(kernel == "radial")),
  makeIntegerParam("degree", lower = 2, upper = 5, requires = quote(kernel == "polynomial")))
# ranger
param.set.ranger = makeParamSet(
  makeIntegerParam("num.trees", lower = 1, upper = 2000),
  makeLogicalParam("replace"),
  makeNumericParam("sample.fraction", lower = 0.1, upper = 1),
  makeNumericParam("mtry", lower = 0, upper = 1),
  makeLogicalParam(id = "respect.unordered.factors"),
  makeNumericParam("min.node.size", lower = 0, upper = 1))
# xgboost
param.set.xgboost = makeParamSet(
  makeIntegerParam("nrounds", lower = 1, upper = 5000), 
  makeNumericParam("eta", lower = -10, upper = 0, trafo = function(x) 2^x),
  makeNumericParam("subsample",lower = 0.1, upper = 1),
  makeDiscreteParam("booster", values = c("gbtree", "gblinear")),
  makeIntegerParam("max_depth", lower = 1, upper = 15, requires = quote(booster == "gbtree")),
  makeNumericParam("min_child_weight", lower = 0, upper = 7, requires = quote(booster == "gbtree"), trafo = function(x) 2^x),
  makeNumericParam("colsample_bytree", lower = 0, upper = 1, requires = quote(booster == "gbtree")),
  makeNumericParam("colsample_bylevel", lower = 0, upper = 1, requires = quote(booster == "gbtree")),
  makeNumericParam("lambda", lower = -10, upper = 10, trafo = function(x) 2^x),
  makeNumericParam("alpha", lower = -10, upper = 10, trafo = function(x) 2^x))
