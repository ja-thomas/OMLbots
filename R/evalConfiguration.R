# This evaluates all configurations of a single Learner + Task
# @param lrn Learner
# @param task OMLTask
# @param par data.frame of configurations to evaluate
# @param min.resources minimal used resources
# @param max.resources maximum allowed resources for a single evaluation
# @param upload should the run be uploaded
evalConfigurations = function(lrn, task, par, min.resources, max.resources, upload) {
  
  reg = makeExperimentRegistry(file.dir = NA, packages = c("mlr"))
  addProblem(name = as.character(task), data = task)
  
  addAlgorithm("lrnwrapper", fun = function(job, data, instance, mlr.lrn = lrn, should.upload = upload, ...) {
    data = getOMLTask(data)
    mlr.par.set = list(...)
    mlr.par.set = mlr.par.set[!vlapply(mlr.par.set, is.na)]
    mlr.lrn = setHyperPars(mlr.lrn, par.vals = mlr.par.set)
    res = runTaskMlr(data, mlr.lrn)
    if (should.upload)
      uploadOMLRun(res, confirm.upload = FALSE, tags = "mlrRandomBotV1", verbosity = 1)
    print(res)
    
    return(TRUE)
  })
  
  addExperiments(algo.designs = list(lrnwrapper = par), reg = reg)
  
  if (!is.null(max.resources))
    exponentialBackOff(jobs = 1:nrow(par), registry = reg, start.resources = min.resources, max.resources = max.resources)
  else
    submitJobs()
}
