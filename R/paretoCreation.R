# Create a pareto front for a given surrogate models for a measure and time of a learner
createParetoFront = function(learner.name, lrn.par.set, surrogates.measures, surrogates.time, meta.features, n.points = 100) {
  
  param.set = lrn.par.set[[which(names(lrn.par.set) == paste0(substr(learner.name, 5, 100), ".set"))]]$param.set
  surrogate.measures = surrogates.measures[[learner.name]]$surrogate
  surrogate.time = surrogates.time[[learner.name]]$surrogate
  
  rnd.points = generateRandomDesign(n.points, param.set, trafo = TRUE)
  rnd.points = deleteNA(rnd.points)
  hyp.pars = rnd.points
  rnd.points = cbind(rnd.points, meta.features)
  
  measures = predict(surrogate.measures, newdata = rnd.points)$data$response
  times = predict(surrogate.time, newdata = rnd.points)$data$response
  
  mat = matrix(NA, 2, length(measures))
  mat[1, ] = times
  mat[2, ] = - measures
  dominated = is_dominated(mat)
  mat[2,] = - mat[2,]
  
  mat = t(mat)
  mat = data.frame(mat)
  colnames(mat) = c("time", "measure")
  non.dominated.points = mat[!dominated, ]
  non.dominated.hyp.pars = hyp.pars[!dominated,]
  dominated.points = mat[dominated, ]
  dominated.hyp.pars = hyp.pars[dominated,]
  
  return(list(non.dominated = list(preds = non.dominated.points, hyp.pars = non.dominated.hyp.pars), 
   dominated = list(preds = dominated.points, hyp.pars = dominated.hyp.pars)))
}

# Plot the pareto front that was created with createParetoFront
#' @param par.front pareto front object created with createParetoFront
plotParetoFront = function(par.front, plotly = FALSE) {
  if (!plotly) {
    plot(par.front$dominated$preds$measure, par.front$dominated$preds$time, cex = 0.1, 
      main = learner.names[i], xlab = "Performance", ylab = "Time in seconds",
      xlim = range(c(par.front$non.dominated$preds$measure, par.front$dominated$preds$measure)),
      ylim = range(c(par.front$non.dominated$preds$time, par.front$dominated$preds$time)))
    points(par.front$non.dominated$preds$measure, par.front$non.dominated$preds$time, col = "red", cex = 0.7, pch = 16)
  } else {
    d = data.frame(
      rbind(par.front$non.dominated$preds, par.front$dominated$preds),
      rbind(par.front$non.dominated$hyp.pars, par.front$dominated$hyp.pars)
    )
    d$non.dominated = c(rep(0, nrow(par.front$dominated$preds)),
      rep(1, nrow(par.front$non.dominated$preds)))
    
    hyp.pars.names = colnames(d)[-c(1,2, ncol(d))]
    text.func = function (x) {
      paste(c("Parameters:", paste0(hyp.pars.names, "=", round(x, 3))), collapse = " ")
    }
    text = d[, hyp.pars.names]
    text = unlist(apply(text, 1, function(x) text.func(x)))
    
    p = plot_ly(
      d, x = ~measure, y = ~time,
      type = "scatter",
      mode = "markers",
      text = text,
      color = ~non.dominated,
      sizes = c(1, 7),
      size = rep(10, nrow(d))
    )
    p
  }
}

# Get a proposal for a hyperparameter setting for a given time
#' @param surrogate_measure Name of the measure to optimize
#' @param surrogate_time Name of learner
getTimeDependentHyperpar = function(surrogate_measures, surrogate_time, meta_features) {
  # Not ready yet
  return(hyperpar)
}