#' @title makeLrnPsSets
#' @description  Create a learner-parameter set. 
#' @param learner learner object
#' @param param.set parameter set, that matches the learner
#' @param lrn.ps.sets a previously created lrn.ps.set
#' @param id name of the lrn.ps set to create
#' @param overwrite overwrite existing id, if id collision occurs
#' @export
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
    attr(lrn.ps.sets, "class") = "LrnPsSet"
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

#' print.LrnPsSet
#' @export
print.LrnPsSet = function(x, ...) {
  lrns = vcapply(x, function(l) l$learner$short.name)
  lrns = paste(lrns, collapse = ", ")
  print(sprintf("%i learner/parameter combinations containing: %s", length(x), lrns))
}
