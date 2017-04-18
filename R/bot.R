# Main function to run the bot. This evaluates batch.size configurations. 
# Task, learner and the actual configuration are defined by the corresponding functions. 
# @param batch.size number of configurations to evaluate in parallel
# @param sample.learner.fun function to sample a learner from tunePair
# @param sample.task.fun sample a OML task
# @param sample.configuration given a lrn sample a configuration
# @param min.resources minimal resources to start benchmark (list with elements walltime and memory)
# @param max.resources maximum resources allowed for each evaluation (list with elements walltime and memory)
# @param lrn.ps.sets of available learners with matching parameter sets
# @param upload should the run be uploaded to OpenML
# @param path where should the registry be created, defaults to tempdir()
runBot = function(batch.size, sample.learner.fun = sampleRandomLearner, 
  sample.task.fun = sampleSimpleTask, sample.configuration.fun = sampleRandomConfiguration, 
  min.resources = NULL, max.resources = NULL, lrn.ps.sets = simple.lrn.par.set, upload = FALSE,
  path = NA, extra.tag = "botV1") {
  
  task = sample.task.fun()
  task$task = getOMLTask(task$id)
  print(sprintf("Selected OML task: %s (id %s)", task$name, task$id))
  
  lrn = sample.learner.fun(lrn.ps.sets)
  print(sprintf("Selected learner: %s", lrn$learner$short.name))
  
  par = sample.configuration.fun(batch.size, lrn$param.set)
  attr(par, "additional.tags") = c(attr(par, "additional.tags"), paste0("sciBenchV", packageDescription("rscimark")$Version))
  attr(par, "additional.tags") = c(attr(par, "additional.tags"), extra.tag)
  print("Selected configurations:")
  print(par)
  
  if (getLearnerPackages(lrn$learner) == "ranger") {
    p = ncol(task$task$input$data.set$data) - 1
    par$mtry = ceiling(p * par$mtry)
  }
  
  if (getLearnerPackages(lrn$learner) == "xgboost") {
    #task$task$input$data.set$data[cols] = data.frame(convToNum(task$task$input$data.set$data[, cols]))
    
    target = task$task$input$data.set$target.features
    cols = which(colnames(task$task$input$data.set$data) != target)
    task$task$input$data.set$data = cbind(sapply(dummy.data.frame(task$task$input$data.set$data[cols]), as.numeric), task$task$input$data.set$data[,target])
  }
  
  evalConfigurations(lrn$learner, task = task, par, min.resources, max.resources, 
    upload = upload, path = path)
}

# Conversion from factor to numeric for xgboost
convToNum = function(data) {
  char_i = names(Filter(function(x) x=="factor", sapply(data, class)))
  for (i in char_i) {
    data[,i] = as.numeric(ordered(data[, i]))
  }
  return(data)
}
