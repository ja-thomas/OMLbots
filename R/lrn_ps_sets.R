# Create a learner-parameter set. 
# @param learner learner object
# @param param.set parameter set, that matches the learner
# @param lrn.ps.sets a previously created lrn.ps.set
# @param id name of the lrn.ps set to create
# @param overwrite overwrite existing id, if id collision occurs
makeLrnPsSets = function(learner, param.set, lrn.ps.sets = NULL, 
  id = paste0(learner$id, ".set"), overwrite = FALSE) {
  
  assertClass(learner, "Learner")
  assertClass(param.set, "ParamSet")
  par.match = names(param.set$pars) %in% names(learner$par.set$pars)
  if(all(par.match)){
    ls = list(learner = learner, param.set = param.set)
  } else {
    stop(paste("The following parameters in param.set are not included in learner:", 
      paste(names(param.set$pars[par.match == FALSE]), collapse = ", ")))
  }
  
  if(is.null(lrn.ps.sets)){
    lrn.ps.sets = list()
    lrn.ps.sets[[id]] = ls
    attr(lrn.ps.sets, "class") <- "LrnPsSet"
  } else {
    assertClass(lrn.ps.sets, "LrnPsSet")
    
    if(id %in% names(lrn.ps.sets) & overwrite == FALSE){
      stop("tune.pair already contains id: \"", id, "\". Please specify a new id or set overwrite = TRUE.")
    } else {
      lrn.ps.sets[[id]] = ls
    }
  }
  
  return(lrn.ps.sets)
}


# Define some learner-param sets
learner = makeLearner("classif.svm", predict.type = "prob")
param.set = makeParamSet(
  makeDiscreteParam("kernel", values = c("linear", "polynomial", "radial")),
  makeNumericParam("cost", lower = -10, upper = 10, trafo = function(x) 2^x),
  makeNumericParam("gamma", lower = -10, upper = 10, trafo = function(x) 2^x, requires = quote(kernel == "radial")),
  makeIntegerParam("degree", lower = 2, upper = 5, requires = quote(kernel == "polynomial")))
lrn.ps.sets = makeLrnPsSets(learner = learner, param.set = param.set)

learner = makeLearner("classif.kknn", predict.type = "prob")
param.set = makeParamSet(
  makeIntegerParam("k", lower = 1, upper = 30))
lrn.ps.sets = makeLrnPsSets(learner = learner, param.set = param.set, lrn.ps.sets)  

