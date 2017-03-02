# @param tag Name of the tag of the benchmark study.
# @return [\code{data.frame}] Table with number of experiments for each learner and each task. 
getMlrRandomBotOverview = function(tag = "mlrRandomBotV1") {
  df = listOMLRunEvaluations(tag = tag)
  return(as.data.frame.matrix(table(df$flow.name, df$data.name)))
}

# @param tag Name of the tag of the benchmark study.
# @return [\code{data.frame}] Table with run.id, task.id, flow.id, flow.name, measure values.
getMlrRandomBotResults = function(tag) {
  df = listOMLRunEvaluations(tag = tag)
  drops = c("setup.id", "data.name", "upload.time")
  df[, !(names(df) %in% drops)]
}

# @param tag Name of the tag of the benchmark study.
# @return [\code{data.frame}] Table with run.id, hyperparameter name & value.
getMlrRandomBotHyperpars = function(tag) {
  runs = listOMLRuns(tag = tag)
  df = data.frame(run.id = integer(), Hyp.par.name = character(), Hyp.par.value = character())
  for (i in runs$run.id){
    hyp.pars = getOMLRunParList(getOMLRun(i))
    temp.df = data.frame(run.id = i,
      Hyp.par.name = vcapply(hyp.pars, function(x) x$name),
      Hyp.par.value = vcapply(hyp.pars, function(x) x$value))
    df = rbind(df, temp.df)
  }
  return(df)
}

# @param tag Name of the tag of the benchmark datasets.
# @return [\code{data.frame}] Table with task.id, data.id, name, target.feature and metafeatures.
getMetaFeatures = function(tag = "study_14") {
  df = listOMLTasks(tag = tag)
  drops = c("task.type", "status", "format", "estimation.procedure", "evaluation.measures", 
    "target.feature", "tags")
  df[, !(names(df) %in% drops)]
}