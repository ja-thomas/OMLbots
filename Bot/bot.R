#' @param batch.size number of configurations to evaluate in parallel
#' @param sample.learner.fun function to sample a learner from tunePair
#' @param sample.task.fun sample a OML task
#' @param sample.configuration given a lrn sample a configuration
#' @param min.resources minimal resources to start benchmark (list with elements walltime and memory)
#' @param max.resources maximum resources allowed for each evaluation (list with elements walltime and memory)
#' @param lrn.ps.sets of available learners with matching parameter sets
runBot = function(batch.size, sample.learner.fun = sampleRandomLearner, 
  sample.task.fun = sampleRandomTask, sample.configuration.fun = sampleRandomConfiguration, 
  min.resources = NULL, max.resources = NULL, lrn.ps.sets) {
  
  lrn = sample.learner.fun(lrn.ps.sets)
  task = sample.task.fun()
  par = sample.configuration.fun(batch.size, lrn$ps)
  evalConfigurations(lrn$lrn, task, par, min.resources, max.resources)
}

#' @param lrn.ps.sets of available learners with matching parameter sets
#' @return list of one learner with matching parameter set
sampleRandomLearner = function(lrn.ps.sets) {
  sample(lrn.ps.sets, size = 1)[[1]]
}

#' @param size number of configurations to generate
#' @param par.set parameter set
#' @return data.frame where each row is one valid configuration
sampleRandomConfiguration = function(size, par.set) {
  des = generateRandomDesign(size, par.set, trafo = TRUE)
  des = BBmisc::convertDataFrameCols(des, factors.as.char = TRUE)
}
