# @param tag Name of the tag of the benchmark study.
# @return [\code{data.frame}] Table with number of experiments for each learner and each task. 
getMlrRandomBotOverview = function(tag = "mlrRandomBotV1") {
  df = listOMLRunEvaluations(tag = tag)
  return(as.data.frame.matrix(table(df$flow.name, df$data.name)))
}

# @param tag Name of the tag of the benchmark study.
# @return [\code{data.frame}] Table with run.id, task.id, flow.id, flow.name, measure values.
getMlrRandomBotResults = function(tag = "mlrRandomBotV1") {
  df = listOMLRunEvaluations(tag = tag)
  drops = c("setup.id", "data.name", "upload.time")
  df = df[, !(colnames(df) %in% drops)]
  df = df[, !grepl("array", colnames(df))]
  tidyr::gather(df, key = "measure.name", value = "measure.value", -run.id, -task.id, -flow.id, -flow.name)
}

# @param tag Name of the tag of the benchmark study.
# @return [\code{data.frame}] Table with run.id, hyperparameter name & value.
getMlrRandomBotHyperpars = function(tag = "mlrRandomBotV1") {
  runs = listOMLRuns(tag = tag)
  res = lapply(runs$run.id, function(x){
    pars = getOMLRunParList(getOMLRun(x))
    #FIXME: Just kill me now...
    pars = data.frame(do.call(rbind, lapply(pars, function(p) do.call(cbind, p))))
    pars$run.id = x
    pars
  })
  res = do.call(rbind, res)
  res %>% 
    mutate(hyperpar.name = name, hyperpar.value = value) %>% 
    select(run.id, hyperpar.name, hyperpar.value)
}

# @param tag Name of the tag of the benchmark datasets.
# @return [\code{data.frame}] Table with task.id, data.id, name, target.feature and metafeatures.
getMetaFeatures = function(tag = "study_14") {
  df = listOMLTasks(tag = tag)
  drops = c("task.type", "status", "format", "estimation.procedure", "evaluation.measures", 
    "target.feature", "tags")
  df[, !(names(df) %in% drops)]
}