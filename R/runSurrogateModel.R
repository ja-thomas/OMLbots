# Merge results, hyperpars and features tables and prepare for mlr.task input
# @param measure.name.filter What measure to analyse
# @param flow.id.filter What flow to analyse
# @return [\code{data.frame}] Complete table used for creating the surrogate model 
makeBotTable = function(measure.name.filter, flow.id.filter){
  rslt.table = getMlrRandomBotResults()
  hyp.table = getMlrRandomBotHyperpars()
  meta.table = getMetaFeatures(tag = "study_14")
  
  bot.table = rslt.table %>% 
    filter(., measure.name == measure.name.filter & flow.id == flow.id.filter) %>%
    inner_join(., meta.table, by = "task.id") %>%
    inner_join(., hyp.table, by = "run.id") %>%
    select(., -measure.name, -flow.name, -flow.id, -run.id, -data.id, -name) 
  
  #tbd: cast hyperpars and set data.type
  
  return(bot.table)
}

#create table for a specific task with random hyperpars
# @param task.id.filter What task to use
# @param flow.id What flow to analyse
# @param lrn.ps.sets What learner to use
# @param n Number of rows to create
# @return [\code{data.frame}] Complete table used for exploring surrogate model 
makeRandomTable = function(task.id.filter, flow.id, lrn.ps.sets, n){
  learner.name = getOMLFlow(flow.id = flow.id)$name
  meta.table = getMetaFeatures(tag = "study_14")
  
  for(i in lrn.ps.sets){
    if(paste0("mlr.", i$learner$id) == learner.name){
      rnd.ps = generateRandomDesign(n, i$param.set)
    }
  }

  if(!exists("rnd.ps")){
    stop(paste("No learner in lrn.ps.sets with id:", sub("mlr.", "", learner.name)))
  }
  
  rnd.table = meta.table[meta.table$task.id == task.id.filter,][rep(1,n), ]
  rnd.table = cbind(rnd.table, rnd.ps) 
  rnd.table = rnd.table %>% select(., -data.id, -name) 

  return(rnd.table)
}

#train mlr model on full table for measure and runtime
task.data <- makeBotTable("area.under.roc.curve", 5526)

#should be done for the general case within the function:
task.data$k <- as.integer(task.data$hyperpar.value)
task.data$hyperpar.name <- NULL
task.data$hyperpar.value <- NULL

mlr.task <- makeRegrTask("measure.model", task.data, target = "measure.value")
mlr.lrn <- makeLearner("regr.randomForest")
mlr.mod <- train(mlr.lrn, mlr.task)

#predict new table for measure & runtime
pred.data <- makeRandomTable(3950, 5526, lrn.par.set, 20)
mlr.pred <- predict(mlr.mod, newdata = pred.data)
pred.data$pred.measure.value <- mlr.pred$data$response

#create pareto-front 
#pick random points from pareto-front for validation runs to check results
