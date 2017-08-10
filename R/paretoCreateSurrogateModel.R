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
  task.data = makeBotTable(measure.name, learner.name, tbl.results, tbl.metaFeatures, tbl.hypPars, tbl.runTime, tbl.resultsReference, param.set)
  # delete or Transform Missing values
  task.data[, names(param.set$pars)] = deleteNA(task.data[, names(param.set$pars), drop = FALSE])
  
  bigger = names(table(task.data$task_id))[which(table(task.data$task_id) > min.experiments)]
  task.data = task.data[task.data$task_id %in% bigger,]
  
  # get specific task ids
  if(!is.null(task.ids)) {
    uni = unique(task.data$task_id)
    task.ids = uni[uni %in% task.ids]
  } else {
    task.ids = unique(task.data$task_id)
  }
  
  if (time) {
    mlr.task.measure = makeRegrTask(id = as.character(learner.name), subset(task.data, task_id %in% task.ids, select =  c(-task_id, -measure.value)), target = "runtime")
  } else {
    mlr.task.measure = makeRegrTask(id = as.character(learner.name), subset(task.data, task_id %in% task.ids, select =  c(-task_id, -runtime)), target = "measure.value")
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
makeBotTable = function(measure.name, learner.name, tbl.results, tbl.metaFeatures, tbl.hypPars, tbl.runTime, tbl.resultsReference, param.set) {

  # This is not used at the moment  
  measure.name.filter = measure.name
  
  tbl.runTime = scaleRunTime(tbl.runTime)
  
  # Readjust tbl.hypPars.learner
  tbl.hypPars.learner = tbl.hypPars[tbl.hypPars$fullName == learner.name, ]
  tbl.hypPars.learner = spread(tbl.hypPars.learner, name, value)
  tbl.hypPars.learner = data.frame(tbl.hypPars.learner)
  # Convert the columns to the specific classes
  params = getParamIds(param.set)
  param_types = getParamTypes(param.set)
  for(i in seq_along(params))
    tbl.hypPars.learner[, params[i]] = conversion_function(tbl.hypPars.learner[, params[i]], param_types[i])
  
  # Readjust tbl.metaFeatures
  tbl.metaFeatures.adj = spread(tbl.metaFeatures, quality, value)
  tbl.metaFeatures.adj = data.frame(tbl.metaFeatures.adj)
  for (i in 1:ncol(tbl.metaFeatures.adj))
    tbl.metaFeatures.adj[,i] = as.numeric(tbl.metaFeatures.adj[,i])
  
  bot.table = inner_join(tbl.results, tbl.hypPars.learner, by = "setup") %>%
    inner_join(., tbl.metaFeatures.adj, by = "data_id") %>%
    inner_join(., tbl.runTime, by = "run_id") %>%
    select(., -run_id, -setup, -fullName, -scimark)
  
  # Scale mtry in random forest
  if(learner.name == "mlr.classif.ranger"){
    n_feats = filter(tbl.metaFeatures, quality == "NumberOfFeatures") %>%
      select(., -quality)
    n_feats$value = as.numeric(n_feats$value)
    
    bot.table = inner_join(bot.table, n_feats, by = "data_id")
    bot.table$mtry = bot.table$mtry/bot.table$value
    bot.table = bot.table %>% select(., -value)
  }
  
  bot.table = bot.table %>%  select(., -data_id)
  colnames(bot.table)[2] = "measure.value"
  # Make the measure numeric
  bot.table$measure.value = as.numeric(bot.table$measure.value)
  
  # scale by reference learner
  colnames(tbl.resultsReference)[4] = "measure.value"
  tbl.resultsReference = tbl.resultsReference %>% group_by(task_id) %>% summarize(mean(measure.value))
  colnames(tbl.resultsReference)[2] = "avg"
  bot.table = bot.table %>%
    inner_join(., tbl.resultsReference, by = "task_id")
  
  # there are missing some reference learners for some task_ids...
  # this can be changed!
  bot.table$measure.value = bot.table$measure.value - bot.table$avg + 0.5
  bot.table = bot.table %>%  select(., -avg)
  
  return(bot.table)
}

#' Conversion function to get right class for the hyperparameters
#' @param x 
#' @param param_type 
#' @export
conversion_function = function(x, param_type) {
  if(param_type %in% c("integer", "numeric", "numericvector")) 
    x = as.numeric(x)
  if(param_type %in% c("character", "logical", "factor"))
    x = as.factor(x)
  return(x)
}

#' Scale the run time results with the sci.mark results
#' @param tbl.runTime The runtime table
#' @export
scaleRunTime = function(tbl.runTime) {
  tbl.runTime$scimark = tbl.runTime$scimark / median(tbl.runTime$scimark, na.rm = T)
  tbl.runTime$runtime = tbl.runTime$runtime / tbl.runTime$scimark
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