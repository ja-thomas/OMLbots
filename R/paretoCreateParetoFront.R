
#' @title createParetoFront
#' @description Create a pareto front for a given surrogate model for a measure and time of a learner
#' @param learner.name 
#' @param lrn.par.set 
#' @param surrogates.measures 
#' @param surrogates.time 
#' @param meta.features 
#' @param n.points 
#' @param extra.points 
#'
#' @export
createParetoFront = function(learner.name, lrn.par.set, surrogates.measures, surrogates.time, meta.features, n.points = 100, extra.points = NULL) {
  
  param.set = lrn.par.set[[which(names(lrn.par.set) == paste0(substr(learner.name, 5, 100), ".set"))]]$param.set
  surrogate.measures = surrogates.measures[[learner.name]]$surrogate
  surrogate.time = surrogates.time[[learner.name]]$surrogate
  
  rnd.points = generateRandomDesign(n.points, param.set, trafo = TRUE)
  hyp.pars = rnd.points
  rnd.points = deleteNA(rnd.points)
  rnd.points = cbind(rnd.points, meta.features)
  
  measures = predict(surrogate.measures, newdata = rnd.points)$data$response
  times = predict(surrogate.time, newdata = rnd.points)$data$response
   
  mat = matrix(NA, 2, length(measures))
  mat[1, ] = times
  mat[2, ] = - measures
  dominated = is_dominated(mat)
  mat[2,] = - mat[2,]
  
  mat = data.frame(t(mat))
  colnames(mat) = c("time", "measure")
  non.dominated.points = mat[!dominated, ]
  non.dominated.hyp.pars = hyp.pars[!dominated, , drop = FALSE]
  dominated.points = mat[dominated, ]
  dominated.hyp.pars = hyp.pars[dominated, , drop = FALSE]
  
  # Calculation of extra points, that should be evaluated
  mat.extra = NULL
  if(!is.null(extra.points)) {
    extra.points = deleteNA(extra.points)
    extra.points.data = cbind(extra.points, meta.features)
    mat.extra = data.frame(matrix(NA, nrow(extra.points), 2))
    colnames(mat.extra) = c("time", "measure")
    mat.extra$measure = predict(surrogate.measures, newdata = extra.points.data)$data$response
    mat.extra$time = predict(surrogate.time, newdata = extra.points.data)$data$response
  }
  
  return(list(non.dominated = list(preds = non.dominated.points, hyp.pars = non.dominated.hyp.pars), 
   dominated = list(preds = dominated.points, hyp.pars = dominated.hyp.pars), extra.points = list(preds = mat.extra, hyp.pars = extra.points)))
}

#' @title plotParetoFront
#' Plot the pareto front that was created with createParetoFront
#' @param par.front pareto front object created with createParetoFront
#' @param plotly should plotly be used? Default is FALSE.
#' @param log should the time axis be scaled on a log scale? Default is FALSE.
#' @param col vector of colors for extra hyperparameters if they are available in par.front. Default is NULL.
#' @param learner.name 
#' @param cex vector of point size for extra hyperparameters if they are available in par.front. Default is NULL.
#' 
#' @export
plotParetoFront = function(learner.name, par.front, plotly = FALSE, log = FALSE, col = NULL, cex = NULL) {
  
  if (!plotly) {
    plot(par.front$dominated$preds$measure, par.front$dominated$preds$time, cex = 0.1, 
      log = ifelse(log, "y", ""),
      main = learner.name, xlab = "Performance", ylab = "Time in seconds",
      xlim = range(c(par.front$non.dominated$preds$measure, par.front$dominated$preds$measure)),
      ylim = range(c(par.front$non.dominated$preds$time, par.front$dominated$preds$time)))
    points(par.front$non.dominated$preds$measure, par.front$non.dominated$preds$time, col = "red", cex = 0.7, pch = 16)
    if(!is.null(par.front$extra.points$preds)) {
      points(par.front$extra.points$preds$measure, par.front$extra.points$preds$time, col = col, cex = cex, pch = 16)
    }
  } else {
    d = data.frame(
      rbind(par.front$dominated$preds, par.front$non.dominated$preds),
      rbind(par.front$dominated$hyp.pars, par.front$non.dominated$hyp.pars)
    )
    d$non.dominated = c(rep(0, nrow(par.front$dominated$preds)),
      rep(1, nrow(par.front$non.dominated$preds)))
    if(!is.null(par.front$extra.points$preds)) {
      d.extra = data.frame(par.front$extra.points$preds, par.front$extra.points$hyp.pars)
      d.extra$non.dominated = as.numeric(as.factor(col)) + 1
      d = rbind(d, d.extra)
    }
    
    d$non.dominated = as.factor(d$non.dominated)
    
    hyp.pars.names = colnames(d)[-c(1,2, ncol(d))]

    text = d[, hyp.pars.names, drop = FALSE]
    nums <- vapply(text, is.numeric, FUN.VALUE = logical(1))
    text[,nums] <- round(text[,nums], digits = 3)
    text = sapply(text, format, trim = TRUE)
    text = trimws(text)
    text.func = function (x) {
      paste(c("Parameters:", paste0(hyp.pars.names, "=", x)), collapse = " ")
    }
    text = unlist(apply(text, 1, function(x) text.func(x)))
    
    d$z = rep(1, nrow(d))
    # d$z = 1/c(rep(5, 982),  rep(6.5, 18), 15, rep(6.5, 25))
    
    p = plot_ly(
      d, x = ~measure, y = ~time,
      type = "scatter",
      mode = "markers",
      text = text,
      color = ~non.dominated,
      colors = c("black", "red", unique(col)),
      sizes = c(10, 1), # c(20,2)
      size = ~z 
    )
    if(log)
      p = layout(p, yaxis = list(type = "log"))
    p
  }
}

#' @title getTimeDependentHyperpar
#' Get a proposal for a hyperparameter setting for a given time
#' @param surrogate_measures 
#' @param meta_features 
#' @param surrogate_time Name of learner
#' 
#' @export
getTimeDependentHyperpar = function(surrogate_measures, surrogate_time, meta_features) {
  # Not ready yet
  return(hyperpar)
}



