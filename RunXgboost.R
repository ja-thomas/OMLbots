library(devtools)
load_all()

setOMLConfig(apikey = "34ebc7dff3057d8e224e5beac53aea0e")

lrn.par.set = makeLrnPsSets(learner = makeLearner("classif.xgboost", predict.type = "prob"), 
  param.set = makeParamSet(
    makeIntegerParam("nrounds", lower = 1, upper = 5000, default = 100L), 
    makeNumericParam("eta", lower = -10, upper = 0, trafo = function(x) 2^x, default = log(0.3)/log(2)), #(log(0.3)/log(2))
    makeNumericParam("subsample",lower = 0.1, upper = 1, default = 1),
    makeDiscreteParam("booster", values = c("gbtree", "gblinear"), default = "gbtree"),
    makeIntegerParam("max_depth", lower = 1, upper = 15, requires = quote(booster == "gbtree"), default = 6L),
    makeNumericParam("min_child_weight", lower = 0, upper = 7, requires = quote(booster == "gbtree"), trafo = function(x) 2^x, default = 0),
    makeNumericParam("colsample_bytree", lower = 0, upper = 1, requires = quote(booster == "gbtree"), default = 1),
    makeNumericParam("colsample_bylevel", lower = 0, upper = 1, requires = quote(booster == "gbtree"), default = 1),
    makeNumericParam("lambda", lower = -10, upper = 10, trafo = function(x) 2^x, default = 0),
    makeNumericParam("alpha", lower = -10, upper = 10, trafo = function(x) 2^x, default = 0)))

sampleTask = function(){
  tasks = listOMLTasks(number.of.classes = 2L, number.of.missing.values = 0, 
    data.tag = "study_14", estimation.procedure = "10-fold Crossvalidation")
    task = tasks %>%
    filter(format == "ARFF", status == "active", task.id == 219) %>% 
    sample_n(1) %>% 
    select(task.id, name)
  
  return(list(id = task$task.id, name = task$name))
}

getXGboostDefault = function(size, par.set) { # run.id 7937366
  if (size > 1)
    warning("For the default Configuration only one configuration is generated")
  des = generateDesignOfDefaults(par.set, trafo = TRUE)
  des = BBmisc::convertDataFrameCols(des, factors.as.char = TRUE)
  attr(des, "additional.tags") = "defaultRun"
  return(des)
}

runBot(1, path = paste0("xgboost_runs_def"), 
    sample.learner.fun = sampleRandomLearner, sample.task.fun = sampleTask, 
    sample.configuration.fun = sampleRandomConfiguration, 
    lrn.ps.sets = lrn.par.set, upload = TRUE, extra.tag = "botV1")



