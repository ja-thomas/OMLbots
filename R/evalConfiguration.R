# This evaluates all configurations of a single Learner + Task
# @param lrn Learner
# @param task OMLTask
# @param par data.frame of configurations to evaluate
# @param min.resources minimal used resources
# @param max.resources maximum allowed resources for a single evaluation
# @param upload should the run be uploaded
evalConfigurations = function(lrn, task, par, min.resources, max.resources, 
  upload, path) {
  
  attr(par, "additional.tags") = c(attr(par, "additional.tags"), paste0("sciBenchV", packageDescription("rscimark")$Version))
  task$task = getOMLTask(task$id)
  if(!dir.exists(path)){
    reg = makeExperimentRegistry(file.dir = path, 
      packages = c("mlr", "OpenML", "BBmisc"),
      namespaces = "rscimark")
  } else {
    reg = loadRegistry(file.dir = path)
  }
  
  addProblem(name = task$name, data = task$task)
  
  addAlgorithm(lrn$short.name, fun = function(job, data, instance, mlr.lrn = lrn, 
    should.upload = upload, add.tags = attr(par, "additional.tags"), ...) {
    
    # FIXME: Check if there is any value in running this every time.
    #  If not, then run when creating registry.
    sci.bench = rscimark::rscimark()
    print(sci.bench)
    
    # Run mlr
    mlr.par.set = list(...)
    mlr.par.set = mlr.par.set[!vlapply(mlr.par.set, is.na)]
    mlr.lrn = setHyperPars(mlr.lrn, par.vals = mlr.par.set)
    res = runTaskMlr(data, mlr.lrn, scimark.vector = sci.bench)
    print(res)
    if (should.upload) {
      tags = c("mlrRandomBot", "botV1", add.tags)
      uploadOMLRun(res, confirm.upload = FALSE, tags = tags, verbosity = 1)
    }
      
    return(TRUE)
  })
  
  design = list(par)
  names(design) = lrn$short.name
  addExperiments(algo.designs = design, reg = reg)
  
  if (!is.null(max.resources))
    exponentialBackOff(jobs = 1:nrow(par), registry = reg, start.resources = min.resources, max.resources = max.resources)
  else
    reg$cluster.functions = makeClusterFunctionsSocket(3)
    submitJobs()
}
