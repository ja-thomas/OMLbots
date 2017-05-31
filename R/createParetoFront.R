
# Create a pareto front for a given surrogate models for a measure and time of a learner
#' @param surrogate_measure Name of the measure to optimize
#' @param surrogate_time Name of learner
createParetoFront = function(learner.name, lrn.par.set, surrogates.measures, surrogates.time, meta.features, n.points = 100) {
  
  param.set = lrn.par.set[[which(names(lrn.par.set) == paste0(substr(learner.name, 5, 100), ".set"))]]$param.set
  surrogate.measures = surrogates.measures[[learner.name]]$surrogate
  surrogate.time = surrogates.time[[learner.name]]$surrogate
  
  rnd.points = generateRandomDesign(n.points, param.set, trafo = TRUE)
  rnd.points = deleteNA(rnd.points)
  hyp.pars = rnd.points
  rnd.points = cbind(rnd.points, meta.features)
  
  preds.measures = predict(surrogate.measures, newdata = rnd.points)
  preds.time = predict(surrogate.time, newdata = rnd.points)
  
  return(list(measure = preds.measures$data$response, time = preds.time$data$response, hyp.pars = hyp.pars))
}

# Get a proposal for a hyperparameter setting for a given time
#' @param surrogate_measure Name of the measure to optimize
#' @param surrogate_time Name of learner
getTimeDependentHyperpar = function(surrogate_measures, surrogate_time, meta_features) {
  # Not ready yet
  return(hyperpar)
}