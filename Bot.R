if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, ggplot2, batchtools, mlr, ParamHelpers)

source("Learner_Param_Def.R")

# Create registry -------------------------------------------------------------------------------------------------
unlink("./registry", recursive = TRUE)
reg = makeExperimentRegistry(seed = 100L,
                             packages = c("ParamHelpers", "cluster", "mlr"))
#reg$cluster.functions = makeClusterFunctionsSocket(1)

addProblem("PimaDiabetes", data = pid.task, reg = reg)

runBot <- function(job, data, instance, mlr.lrn.name, predict.threshold, ...) {
  mlr.task = data
  mlr.res = makeResampleDesc("CV", iters = 10)
  mlr.lrn = makeLearner(mlr.lrn.name,
    predict.type = 'prob',
    predict.threshold = predict.threshold)
  
  #Set hyperpars for learner
  mlr.par.set = list(...)
  lrn.pars = filterParams(getParamSet(mlr.lrn), tunable = TRUE)
  mlr.par.set = mlr.par.set[names(mlr.par.set) %in% names(lrn.pars$pars)]
  mlr.lrn = setHyperPars(mlr.lrn, par.vals = mlr.par.set)
  
  mlr.mod = resample(mlr.lrn,
                      mlr.task, 
                      resampling = mlr.res, 
                      measures = getDefaultMeasure(mlr.task))
  
  return(list(mlr.mod = mlr.mod))
}

addAlgorithm("runBot", fun = runBot, reg = reg)
algo.designs = list()
algo.designs$runBot = DF.LEARNER.PARAM.SPECS

addExperiments(algo.designs = algo.designs, reg = reg)
summarizeExperiments(reg=reg)
# submitJobs(ids = findNotDone()$job.id, reg=reg)
# getJobStatus(1)
# reduceResultsList(1)






