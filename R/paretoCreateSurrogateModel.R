#' @title makeSurrogateModel
#' Create surrogate model for different tasks
#'
#' @param measure.name Name of the measure to optimize
#' @param learner.name Name of learner
#' @param task.ids [\code{numeric}] ids of the tasks
#' @param lrn.par.set learner-parameter set which should include relevant bounds for flow
#' @param tbl.results df with getMlrRandomBotResults()
#' @param tbl.hypPars df with getMlrRandomBotHyperpars()
#' @param tbl.metaFeatures df with getMlrRandomBotHyperpars()
#' @param surrogate.mlr.lrn model(s) that should be used as surrogate model
#' @param min.experiments minimum number of experiments that should be available for a dataset, otherwise the dataset is excluded
#' @param benchmark [\code{logical}] Should a benchmark experiment with different surrogate models be executed to 
#' @param tbl.runTime 
#' @param tbl.resultsReference 
#' @param time 
#' 
#' evaluate the performance of the surrogate models? Default is FALSE.
#' 
#' @return surrogate model
#' @export
makeSurrogateModel = function(measure.name, learner.name, task.ids, lrn.par.set, tbl.results, tbl.hypPars, 
  tbl.metaFeatures, tbl.runTime, tbl.resultsReference, surrogate.mlr.lrn, min.experiments = 100, benchmark = FALSE, time = FALSE) {
  
  param.set = lrn.par.set[[which(names(lrn.par.set) == paste0(substr(learner.name, 5, 100), ".set"))]]$param.set
  
  #train mlr model on full table for measure
  mlr.mod.measure = list()
  task.data = makeBotTable(measure.name, learner.name, tbl.results, tbl.hypPars, tbl.metaFeatures, tbl.runTime, tbl.resultsReference)
  # delete or Transform Missing values
  task.data[, names(param.set$pars)] = deleteNA(task.data[, names(param.set$pars), drop = FALSE])
  
  bigger = names(table(task.data$task.id))[which(table(task.data$task.id) > min.experiments)]
  task.data = task.data[task.data$task.id %in% bigger,]
  
  # get specific task ids
  if(!is.null(task.ids)) {
    uni = unique(task.data$task.id)
    task.ids = uni[uni %in% task.ids]
  } else {
    task.ids = unique(task.data$task.id)
  }
  
  if (time) {
    mlr.task.measure = makeRegrTask(id = as.character(learner.name), subset(task.data, task.id %in% task.ids, select =  c("run.time", names(param.set$pars), 
      "majority.class.size", "minority.class.size", "number.of.classes", "number.of.features", "number.of.instances",
      "number.of.numeric.features", "number.of.symbolic.features")), target = "run.time")
  } else {
    mlr.task.measure = makeRegrTask(id = as.character(learner.name), subset(task.data, task.id %in% task.ids, select =  c("measure.value", names(param.set$pars), 
      "majority.class.size", "minority.class.size", "number.of.classes", "number.of.features", "number.of.instances",
      "number.of.numeric.features", "number.of.symbolic.features")), target = "measure.value")
  }
  mlr.lrn = surrogate.mlr.lrn
  
  if(benchmark) {
    rdesc = makeResampleDesc("RepCV", reps = 10, folds = 10)
    res = benchmark(mlr.lrn, mlr.task.measure, resamplings = rdesc, measures = list(mse, rsq, kendalltau, spearmanrho), 
      models = FALSE, keep.pred = FALSE)
    return(list(result = res))
  }
  else {
    mlr.mod.measure = train(mlr.lrn, mlr.task.measure)
    return(list(surrogate = mlr.mod.measure))
  }
}

#' @title makeBotTable
#' Merge results, hyperpars and features tables and prepare for mlr.task input
#'
#' @param learner.name What learner to analyse
#' @param tbl.results df with getMlrRandomBotResults()
#' @param tbl.hypPars df with getMlrRandomBotHyperpars()
#' @param measure.name What measure to analyse
#' @param tbl.runTime 
#' @param tbl.resultsReference 
#' @param tbl.metaFeatures df with getMlrRandomBotHyperpars()
#'
#' @return [\code{data.frame}] Complete table used for creating the surrogate model 
#' @export
makeBotTable = function(measure.name, learner.name, tbl.results, tbl.hypPars, tbl.metaFeatures, tbl.runTime, tbl.resultsReference){

  measure.name.filter = measure.name
  learner.name.fiter = learner.name
  
  tbl.runTime = scaleRunTime(tbl.runTime)
  
  bot.table = tbl.results %>% 
    filter(., measure.name == measure.name.filter & learner.name == learner.name.fiter) %>%
    inner_join(., tbl.metaFeatures, by = "task.id") %>%
    inner_join(., tbl.hypPars, by = "run.id") %>%
    select(., -measure.name, -flow.name, -flow.id, -data.id, -flow.source,
      -setup.id, -data.name, -upload.time, -flow.version, -learner.name, -name, -number.of.instances.with.missing.values, 
      -number.of.missing.values) %>%
    spread(., key = hyperpar.name, value = hyperpar.value, convert = TRUE) %>%
    inner_join(., tbl.runTime, by = "run.id") %>%
    select(., -run.id)
  bot.table$measure.value = as.numeric(bot.table$measure.value)
  
  # scale by reference learner
  tbl.resultsReference = tbl.resultsReference[tbl.resultsReference$measure.name == measure.name.filter,]
  tbl.resultsReference = tbl.resultsReference %>% group_by(task.id, learner.name) %>% summarize(mean(measure.value))
  tbl.resultsReference = tbl.resultsReference[tbl.resultsReference$learner.name == "mlr.classif.ranger",]
  tbl.resultsReference$learner.name = NULL
  colnames(tbl.resultsReference)[2] = "avg"
  
  bot.table = bot.table %>%
    left_join(., tbl.resultsReference, by = "task.id")
  # this can be changed!
  bot.table$measure.value = bot.table$measure.value - bot.table$avg + 0.5
  
  bot.table = convertDataFrameCols(bot.table, chars.as.factor = TRUE)
  return(bot.table)
}

#' Scale the run time results with the sci.mark results
#' @param tbl.runTime The runtime table
#' @export
scaleRunTime = function(tbl.runTime) {
  tbl.runTime$sci.mark = tbl.runTime$sci.mark / median(tbl.runTime$sci.mark, na.rm = T)
  tbl.runTime$run.time = tbl.runTime$run.time / tbl.runTime$sci.mark
  return(tbl.runTime)
}

#' Replace NA values by -11 (numeric variables) or NA levels (factor variables)
#' @param task.data task data
#' @export
deleteNA = function(task.data) {
  for(i in 1:ncol(task.data)) {
    if(is.numeric(task.data[, i]))
      task.data[is.na(task.data[, i]), i] = -10 - 1
    if(is.factor(task.data[, i])) {
      task.data[, i] = addNA(task.data[, i])
      task.data[, i] = droplevels(task.data[, i])
    }
    if(is.logical(task.data[, i]))
      task.data[, i] = as.factor(task.data[, i])
  }
  task.data
}