# @param tag Name of the tag of the benchmark study
# @return [\code{data.frame}] Table with number of experiments for each learner and each task 
getMlrRandomBotOverview = function(tag = "mlrRandomBotV1") {
  df <- listOMLRunEvaluations(tag = tag)
  return(table(df$flow.name, df$data.name))
}

# @param tag Name of the tag of the benchmark study
getMlrRandomBotResults = function(tag) {
  runs = listOMLRuns(tag = tag)
  perf = t(sapply(runs$run.id, getOMLRunResult))
  perf = cbind(runs$run.id, perf)
  colnames(perf) = c("id", getOMLRunResult(runs$run.id[1], run.names = TRUE))
  perf
}

# @param tag Name of the tag of the benchmark study
# @return [\code{data.frame}] Table with runid, hyperparameter name & value.
getMlrRandomBotHyperpars = function(tag) {
  runs = listOMLRuns(tag = tag)
  
  df = data.frame(RunId = integer(),
    Hyp.par.name = character(),
    Hyp.par.value = character())
  
  for (i in runs$run.id){
    hyp.pars = getOMLRunParList(getOMLRun(i))
    
    temp.df = data.frame(RunId = i,
      Hyp.par.name = vcapply(hyp.pars, function(x) x$name),
      Hyp.par.value = vcapply(hyp.pars, function(x) x$value))
    
    df = rbind(df, temp.df)
  }
  
  return(df)
}

# Helpers
getOMLRunResult = function(run.id, run.names = FALSE) {
  results = getOMLRun(run.id)$output.data$evaluations
  results[is.na(results$fold), ifelse(run.names, "name", "value")]
}

getOMLRunHypPars = function(run.id) {
  run = getOMLRun(run.id)
  list(learner = run$flow.name, params = getOMLRunParList(run)[])
}





