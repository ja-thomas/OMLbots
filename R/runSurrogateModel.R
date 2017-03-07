#' Merge results, hyperpars and features tables and prepare for mlr.task input
#' @param measure.name.filter What measure to analyse
#' @param learner.name What learner to analyse
#' @param tbl.results df with getMlrRandomBotResults()
#' @param tbl.hypPars df with getMlrRandomBotHyperpars()
#' @param tbl.metaFeatures df with getMlrRandomBotHyperpars()
#' @return [\code{data.frame}] Complete table used for creating the surrogate model 
makeBotTable = function(measure.name, learner.name, tbl.results, tbl.hypPars, tbl.metaFeatures){
  
  measure.name.filter = measure.name
  learner.name.fiter = learner.name
  bot.table = tbl.results %>% 
    filter(., measure.name == measure.name.filter & learner.name == learner.name.fiter) %>%
    inner_join(., tbl.metaFeatures, by = "task.id") %>%
    inner_join(., tbl.hypPars, by = "run.id") %>%
    select(., -measure.name, -flow.name, -flow.id, -run.id, -data.id, -name) %>%
    spread(., key = hyperpar.name, value = hyperpar.value, convert = TRUE)
  bot.table$user.time = tbl.results[tbl.results$measure.name == "usercpu.time.millis" & tbl.results$flow.id == flow.id.filter, "measure.value"]
  
  return(bot.table)
}

#' Create table for a specific task with random hyperpars
#' @param task.id.filter What task to use
#' @param learner.name What learner to use
#' @param lrn.ps.sets What learner to use
#' @param n Number of rows to create
#' @param tbl.hypPars df with getMlrRandomBotHyperpars()
#' @return [\code{data.frame}] Complete table used for exploring surrogate model 
makeRandomTable = function(task.id.filter, learner.name, lrn.ps.sets, n, tbl.metaFeatures){
  
  for(i in lrn.ps.sets){
    if(paste0("mlr.", i$learner$id) == learner.name){
      rnd.ps = generateRandomDesign(n, i$param.set)
    }
  }
  if(!exists("rnd.ps")){
    stop(paste("No learner in lrn.ps.sets with id:", sub("mlr.", "", learner.name)))
  }
  
  rnd.table = tbl.metaFeatures[tbl.metaFeatures$task.id == task.id.filter,][rep(1,n), ]
  rnd.table = cbind(rnd.table, rnd.ps) 
  rnd.table = rnd.table %>% select(., -data.id, -name) 

  return(rnd.table)
}

#' Create random hyperpars for a flow and predict the measure and time for a given task.
#' @param measure.name Name of the measure to optimize
#' @param learner.name Name of learner
#' @param task.id ID of the task
#' @param lrn.par.set learner-parameter set which should include relevant bounds for flow
#' @param n Number of points to create
#' @param tbl.results df with getMlrRandomBotResults()
#' @param tbl.hypPars df with getMlrRandomBotHyperpars()
#' @param tbl.metaFeatures df with getMlrRandomBotHyperpars()
#' @return [\code{data.frame}] df with hyperpars of flow and matching predictions for measure value and time
makeMeasureTimePrediction = function(measure.name, learner.name, task.id, lrn.par.set, n, 
  tbl.results, tbl.hypPars, tbl.metaFeatures){
    
  #train mlr model on full table for measure and runtime
  task.data = makeBotTable(measure.name, learner.name, tbl.results, tbl.hypPars, tbl.metaFeatures)
  mlr.task.measure = makeRegrTask("measure.value", subset(task.data, select = -user.time), target = "measure.value")
  mlr.task.time = makeRegrTask("user.time", subset(task.data, select = -measure.value), target = "user.time")
  mlr.lrn = makeLearner("regr.randomForest")
  mlr.mod.measure = train(mlr.lrn, mlr.task.measure)
  mlr.mod.time = train(mlr.lrn, mlr.task.time)
  
  #predict new table for measure & runtime
  pred.data = makeRandomTable(task.id, learner.name, lrn.par.set, n, tbl.metaFeatures)
  mlr.pred.measure = predict(mlr.mod.measure, newdata = predqay.data)
  mlr.pred.time = predict(mlr.mod.time, newdata = pred.data)
  pred.data$pred.measure.value = mlr.pred.measure$data$response
  pred.data$pred.time = mlr.pred.time$data$response
  rownames(pred.data) = NULL
  
  return(pred.data[,12:ncol(pred.data)])
}


