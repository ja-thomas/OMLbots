# This evaluates all configurations of a single Learner + Task
# @param lrn Learner
# @param task OMLTask
# @param par data.frame of configurations to evaluate
# @param min.resources minimal used resources
# @param max.resources maximum allowed resources for a single evaluation
# @param upload should the run be uploaded
evalConfigurations = function(lrn, task, par, min.resources, max.resources, 
  upload, path, extra.tag) {
  
  attr(par, "additional.tags") = c(attr(par, "additional.tags"), paste0("sciBenchV", packageDescription("rscimark")$Version))
  task$task = getOMLTask(task$id)
  reg = makeExperimentRegistry(file.dir = path, 
    packages = c("mlr", "OpenML", "BBmisc"),
    namespaces = "rscimark")
  
  addProblem(name = task$name, data = task$task)
  
  addAlgorithm(lrn$short.name, fun = function(job, data, instance, mlr.lrn = lrn, 
    should.upload = upload, add.tags = attr(par, "additional.tags"), extra.tag = extra.tag, ...) {
    
    # FIXME: Check if there is any value in running this every time.
    #  If not, then run when creating registry.
    sci.bench = rscimark::rscimark()
    print(sci.bench)
    
    # Run mlr
    mlr.par.set = list(...)
    mlr.par.set = mlr.par.set[!vlapply(mlr.par.set, is.na)]
    if (getLearnerPackages(lrn) == "ranger") {
      p = ncol(data$input$data.set$data) - 1
      mlr.par.set$mtry = ceiling(p * mlr.par.set$mtry)
    }
    if (getLearnerPackages(lrn) == "xgboost") {
      target.column = which(colnames(data$input$data.set$data) == data$input$data.set$target.features)
      data$input$data.set$data = data.frame(convToNum(data$input$data.set$data[, -target.column]), data$input$data.set$data[, target.column])
      colnames(data$input$data.set$data)[ncol(data$input$data.set$data)] = data$input$data.set$target.features
      mlr.par.set$nthread = 1
    }
    mlr.lrn = setHyperPars(mlr.lrn, par.vals = mlr.par.set)
    res = runTaskMlr(data, mlr.lrn, scimark.vector = sci.bench)
    print(res)
    if (should.upload) {
      tags = c("mlrRandomBot", extra.tag, add.tags)
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
    submitJobs()
}

# Conversion from factor to numeric for xgboost
convToNum = function(data) {
  char_i = names(Filter(function(x) x=="factor", sapply(data, class)))
  for (i in char_i) {
    data[, i] = as.numeric(ordered(data[, i]))
  }
  return(data)
}
