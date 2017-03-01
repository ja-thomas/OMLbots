evalConfigurations = function(lrn, task, par, min.resources, max.resources) {
  
  reg = makeExperimentRegistry(file.dir = NA, packages = c("mlr"))
  addProblem(name = as.character(task), data = task)
  
  addAlgorithm("lrnwrapper", fun = function(job, data, instance, mlr.lrn = lrn, ...) {
    
    data = getOMLTask(data)
    mlr.par.set = list(...)
    mlr.par.set = mlr.par.set[!vlapply(mlr.par.set, is.na)]
    mlr.lrn = setHyperPars(mlr.lrn, par.vals = mlr.par.set)
    res = runTaskMlr(data, mlr.lrn)
    uploadOMLRun(res, confirm.upload = FALSE)
    return(TRUE)
  })
  
  addExperiments(algo.designs = list(lrnwrapper = par), reg = reg)
  if (!is.null(max.resources))
    exponentialBackOff(jobs = 1:nrow(par), registry = reg, start.resources = min.resources, max.resources = max.resources)
  else
    submitJobs()
}
