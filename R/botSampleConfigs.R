#' Samples size random Configuration for a given parameter set
#' @param size number of configurations to generate
#' @param par.set parameter set
#' @return data.frame where each row is one valid configuration
#' 
#' @export
sampleRandomConfiguration = function(size, par.set) {
  des = generateRandomDesign(size, par.set, trafo = TRUE)
  des = BBmisc::convertDataFrameCols(des, factors.as.char = TRUE)
  return(des)
}


#' sampleDefaultConfiguration
#'
#' @param size number of configurations to generate
#' @param par.set parameter set
#' @return data.frame where each row is one valid configuration
#' 
#' @export
sampleDefaultConfiguration = function(size, par.set) {
  if (size > 1)
    warning("For the default Configuration only one configuration is generated")
  des = generateDesignOfDefaults(par.set, trafo = TRUE)
  des = BBmisc::convertDataFrameCols(des, factors.as.char = TRUE)
  attr(des, "additional.tags") = "defaultRun"
  return(des)
}


#' Sample a random learner with matching parameter set from the lrn.ps.sets list
#' @param lrn.ps.sets of available learners with matching parameter sets
#' @return list of one learner with matching parameter set
#' @export
sampleRandomLearner = function(lrn.ps.sets) {
  sample(lrn.ps.sets, size = 1)[[1]]
}


#' sampleRandomTask
#' 
#' this draws a random binary classif OMLTask from study 14 with 10 fold CV and without missing values
#' @return OML task
#' @export
sampleRandomTask = function() {
  
  tasks = listOMLTasks(number.of.classes = 2L, number.of.missing.values = 0, 
    data.tag = "OpenML100", estimation.procedure = "10-fold Crossvalidation")
  messagef("Found %i available OML tasks", nrow(tasks))
  task = tasks %>% 
    filter(format == "ARFF", status == "active") %>% 
    sample_n(1) %>% 
    select(task.id, name)
  
  return(list(id = task$task.id, name = task$name))
}


#' sampleSimpleTask
#' 
#' sample really simple and small datasets
#' @export
sampleSimpleTask = function() {
  
  tasks = listOMLTasks(number.of.classes = 2L, number.of.instances = c(100L, 500L), 
    number.of.features = c(3L, 20L), number.of.missing.values = 0, estimation.procedure = "33% Holdout set")
  messagef("Found %i available OML tasks", nrow(tasks))
  task = tasks %>%
    filter(format == "ARFF", status == "active") %>% 
    sample_n(1) %>% 
    select(task.id, name)
  
  return(list(id = task$task.id, name = task$name))
}

