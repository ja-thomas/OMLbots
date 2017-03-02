# @param tag Name of the tag of the benchmark study
getMlrRandomBotOverview = function(tag = "mlrRandomBotV1") {
  runs = listOMLRuns(tag = tag)
  runs$task.id
  overview = table(runs$task.id, sapply(runs$run.id, getOMLRunLearner))
}

# @param tag Name of the tag of the benchmark study
getMlrRandomBotResults = function(tag) {
  runs = listOMLRuns(tag = tag)
  perf = t(sapply(runs$run.id, getOMLRunResult))
  perf = cbind(runs$run.id, perf)
  colnames(perf) = c("id", getOMLRunResult(runs$run.id[1], names = TRUE))
  perf
}

# @param tag Name of the tag of the benchmark study
getMlrRandomBotHyperpars = function(tag) {
  runs = listOMLRuns(tag = tag)
  hypPars = sapply(runs$run.id, getOMLRunHypPars)
  colnames(hypPars) = runs$run.id
  learners = unlist(hypPars[1,])
  learnersUnique = unique(learners)
  hypPars_learners = list()
  for (i in learnersUnique) {
    hypPars_list = list()
    for (j in which(hypPars[1,] == i)) {
      hypPars_list[[j]] = lapply(hypPars[2, ][[j]], `[[`, 2)
    }
    dfs = lapply(hypPars_list, data.frame, stringsAsFactors = FALSE)
  hypPars_learners[[i]] = rbind.fill(dfs)
  hypPars_learners[[i]] = cbind(id = runs$run.id[hypPars[1,] == i], hypPars_learners[[i]])
  }
  hypPars_learners
}

# Helpers
getOMLRunResult = function(run.id, names = FALSE) {
  results = getOMLRun(run.id)$output.data$evaluations
  if (names == TRUE) {
    results = results[is.na(results$fold), c("name")]
  } else {
    results = results[is.na(results$fold), c("value")]
  }
}

getOMLRunLearner = function(run.id) {
  getOMLRun(run.id)$flow.name
}

getOMLRunHypPars = function(run.id) {
  run = getOMLRun(run.id)
  list(learner = run$flow.name, params = getOMLRunParList(run)[])
}





