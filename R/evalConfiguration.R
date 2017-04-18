# This evaluates all configurations of a single learner and a matching task.
# Configurations in par should be valid for task. 
# @param lrn Learner
# @param task OMLTask
# @param par data.frame of configurations to evaluate
# @param min.resources minimal used resources
# @param max.resources maximum allowed resources for a single evaluation
# @param upload should the run be uploaded
evalConfigurations = function(lrn, task, par, min.resources, max.resources, 
  upload, path) {
  
  if(!dir.exists(path)){
    reg = makeExperimentRegistry(file.dir = path, 
      packages = c("mlr", "OpenML", "BBmisc"),
      namespaces = "rscimark")
      #conf.file = ".batchtools.conf.R")
  } else {
    reg = loadRegistry(file.dir = path)
  }
  
  addProblem(name = task$name, data = task$task)
  
  addAlgorithm(lrn$short.name, fun = function(job, data, instance, mlr.lrn = lrn, 
    should.upload = upload, add.tags = attr(par, "additional.tags"), ...) {

    # Run mlr
    mlr.par.set = list(...)
    mlr.par.set = mlr.par.set[!vlapply(mlr.par.set, is.na)]
    mlr.lrn = setHyperPars(mlr.lrn, par.vals = mlr.par.set)
    sci.bench = rscimark::rscimark() #FIXME: only execute this once. source in makeExperiment doesn't work...
    res = runTaskMlr(data, mlr.lrn, scimark.vector = sci.bench)
    print(res)
    if (should.upload) {
      tags = c("mlrRandomBot", add.tags)
      uploadOMLRun(res, confirm.upload = FALSE, tags = tags, verbosity = 1)
    }
      
    return(TRUE)
  })
  
  design = list(par)
  names(design) = lrn$short.name
  addExperiments(algo.designs = design, reg = reg)
  
  if (!is.null(max.resources)){
    reg$cluster.functions = makeClusterFunctionsSlurm("slurm_lmulrz.tmpl", clusters = "serial")
    # exponentialBackOff(jobs = 1:nrow(par), registry = reg, start.resources = min.resources, max.resources = max.resources)    
    submitJobs(resources = max.resources)
  } else {
    reg$cluster.functions = makeClusterFunctionsSocket(10)
    submitJobs()    
  }
  waitForJobs(reg = reg)
  unlink(path, recursive = TRUE)
}


