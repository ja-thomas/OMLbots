#' Create surrogate models for different tasks
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
#' evaluate the performance of the surrogate models? Default is FALSE.
#' @return surrogate model
makeSurrogateModels = function(measure.name, learner.name, task.ids, lrn.par.set, tbl.results, tbl.hypPars, 
  tbl.metaFeatures, surrogate.mlr.lrn, min.experiments = 100, benchmark = FALSE, time = FALSE) {
  param.set = lrn.par.set[[which(names(lrn.par.set) == paste0(substr(learner.name, 5, 100), ".set"))]]$param.set
  
  #train mlr model on full table for measure
  mlr.mod.measure = list()
  task.data = makeBotTable2(measure.name, learner.name, tbl.results, tbl.hypPars, tbl.metaFeatures)
  # delete or Transform Missing values
  task.data[, names(param.set$pars)] = deleteNA(task.data[, names(param.set$pars)])
  
  bigger = names(table(task.data$task.id))[which(table(task.data$task.id) > min.experiments)]
  task.data = task.data[task.data$task.id %in% bigger,]
  
  # get specific task ids
  if(!is.null(task.ids)) {
    uni = unique(task.data$task.id)
    task.ids = uni[uni %in% task.ids]
  } else {
    task.ids = unique(task.data$task.id)
  }
  
  mlr.task.measure = makeRegrTask(id = as.character(learner.name), subset(task.data, task.id %in% task.ids, select =  c("measure.value", names(param.set$pars), 
    "majority.class.size", "minority.class.size", "number.of.classes", "number.of.features", "number.of.instances",
    "number.of.numeric.features", "number.of.symbolic.features")), target = "measure.value")
  mlr.lrn = surrogate.mlr.lrn
  
  if(benchmark) {
    rdesc = makeResampleDesc("RepCV", reps = 2, folds = 10)
    res = benchmark(mlr.lrn, mlr.task.measure, resamplings = rdesc)
    return(list(result = res))
  }
  else {
    mlr.mod.measure = train(mlr.lrn, mlr.task.measure)
    return(list(surrogate = mlr.mod.measure))
  }
}

#' Merge results, hyperpars and features tables and prepare for mlr.task input
#' @param measure.name.filter What measure to analyse
#' @param learner.name What learner to analyse
#' @param tbl.results df with getMlrRandomBotResults()
#' @param tbl.hypPars df with getMlrRandomBotHyperpars()
#' @param tbl.metaFeatures df with getMlrRandomBotHyperpars()
#' @return [\code{data.frame}] Complete table used for creating the surrogate model 
makeBotTable2 = function(measure.name, learner.name, tbl.results, tbl.hypPars, tbl.metaFeatures){
  
  measure.name.filter = measure.name
  learner.name.fiter = learner.name
  bot.table = tbl.results %>% 
    filter(., measure.name == measure.name.filter & learner.name == learner.name.fiter) %>%
    inner_join(., tbl.metaFeatures, by = "task.id") %>%
    inner_join(., tbl.hypPars, by = "run.id") %>%
    select(., -measure.name, -flow.name, -flow.id, -data.id, -flow.source,
      -setup.id, -data.name, -upload.time, -flow.version, -learner.name, -name, -number.of.instances.with.missing.values, 
      -number.of.missing.values) %>%
    spread(., key = hyperpar.name, value = hyperpar.value, convert = TRUE) %>%
    select(., -run_NA, -run.id)
  #bot.table$user.time = tbl.results[tbl.results$measure.name == "usercpu.time.millis" & tbl.results$learner.name == learner.name.fiter, "measure.value"]
  bot.table$measure.value = as.numeric(bot.table$measure.value)
  #bot.table$user.time = as.numeric(bot.table$user.time)
  bot.table = convertDataFrameCols(bot.table, chars.as.factor = TRUE)  
  return(bot.table)
}

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