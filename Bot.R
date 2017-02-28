if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, ggplot2, batchtools, mlr, ParamHelpers)

source("Learner_Param_Def.R")

# Create registry -------------------------------------------------------------------------------------------------
unlink("registry", recursive = TRUE)
reg = makeExperimentRegistry(seed = 100L,
  packages = c("ParamHelpers", "mlr", "OpenML"))
#reg$cluster.functions = makeClusterFunctionsSocket(1)

addProblem("TASK", data = ids[1], reg = reg)

runBot = function(job, data, instance, mlr.lrn.name, ...) {
  
  mlr.lrn = makeLearner(mlr.lrn.name, predict.type = 'prob')
  data = getOMLTask(task.id = data)
  #Set hyperpars for learner
  mlr.par.set = list(...)
  mlr.par.set = mlr.par.set[!vapply(mlr.par.set, is.na, logical(1))]
  mlr.lrn = setHyperPars(mlr.lrn, par.vals = mlr.par.set)
  
  measures = listMeasures(convertOMLTaskToMlr(data)$mlr.task, create = TRUE)

  res = runTaskMlr(data, mlr.lrn, measures = measures)
  uploadOMLRun(res, confirm.upload = FALSE)
  res
}

addAlgorithm("runBot", fun = runBot, reg = reg)
algo.designs = list()
algo.designs$runBot = des

addExperiments(algo.designs = algo.designs, reg = reg)
summarizeExperiments(reg=reg)
testJob(1)
# submitJobs(ids = findNotDone()$job.id, reg=reg)
# getJobStatus(1)
# reduceResultsList(1)






