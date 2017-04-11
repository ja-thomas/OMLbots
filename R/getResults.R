# @param tag Name of the tag of the benchmark study.
# @return [\code{data.frame}] Table with number of experiments for each learner and each task. 
getMlrRandomBotOverview = function(tag = "mlrRandomBot") {
  df = listOMLRunEvaluations(tag = tag) %>% 
    mutate(flow.version = c(stri_match_last(flow.name, regex = "[[:digit:]]+\\.*[[:digit:]]*")),
      learner.name = stri_replace_last(flow.name, replacement = "", regex = "[([:digit:]]+\\.*[[:digit:]*)]"))
  
  return(as.data.frame.matrix(table(df$learner.name, df$data.name)))
}

# @param tag Name of the tag of the benchmark study.
# @return [\code{data.frame}] Table with run.id, task.id, flow.id, flow.name, measure values.
getRunTable = function(run.tag = "mlrRandomBot", excl.run.ids = NULL, local.db = NULL) {
  if(is.null(local.db)){
    numRuns = 20000 #FIXME: fix this once OpenML offers new solution
    results = do.call("rbind", 
      lapply(0:floor(numRuns/10000), function(i) {
        return(listOMLRuns(tag = run.tag, limit = 10000, offset = (10000 * i) + 1))
      })
    )
    
    results = results[results$run.id %in% setdiff(results$run.id, excl.run.ids),]
    
    if(nrow(results) > 0){
      res = do.call("rbind", 
        lapply(0:floor(nrow(results)/100), function(i) {
          return(listOMLRunEvaluations(run.id = results$run.id[((100*i)+1):(100*(i+1))]))
        })
      )
      
      df = res %>%
        gather(., key = "measure.name", value = "measure.value", -(run.id:upload.time), na.rm = TRUE) %>%
        mutate(flow.version = c(stri_match_last(flow.name, regex = "[[:digit:]]+\\.*[[:digit:]]*")),
          learner.name = stri_replace_last(flow.name, replacement = "", regex = "[([:digit:]]+\\.*[[:digit:]*)]"))
    } else {
      df <- NULL
    }

  } else {
    df = collect(tbl(local.db, sql("SELECT * FROM [run.table]")))
  }

  return(df)
}

# @param tag Name of the tag of the benchmark study.
# @return [\code{data.frame}] Table with run.id, hyperparameter name & value.
getHyperparTable = function(run.tag = "mlrRandomBot", excl.run.ids = NULL, local.db = NULL) {
  if(is.null(local.db)){
    numRuns = 20000 #FIXME: Fix this once OpenML offers different solution
    runs = do.call("rbind", 
                   lapply(0:floor(numRuns/10000), function(i) {
                     return(listOMLRuns(tag = run.tag, limit = 10000, offset = (10000 * i) + 1))
                   })
    )
    
    runs = runs[runs$run.id %in% setdiff(runs$run.id, excl.run.ids),]
    
    if(nrow(runs) > 0){
      # FIXME: HORRIBLE performance
      res = lapply(runs$run.id, function(x){ #FIXME: Increase performance once OpenML offers solution
        pars = tryCatch(getOMLRunParList(getOMLRun(x)), error = function(cond){return(NA)}) 
        if(length(pars) > 0){
          pars = ifelse(is.na(pars),
                        data.frame(name = "run_NA", value = NA, component = NA, stringsAsFactors = FALSE),
                        data.frame(do.call(rbind, lapply(pars, function(p) do.call(cbind, p))), stringsAsFactors = FALSE))
        } else {
          pars = data.frame(name = "no_pars", value = NA, component = NA, stringsAsFactors = FALSE)
        }
        
        pars$run.id = x
        return(pars)
      })
      res = do.call(rbind, res)
      res = res %>% 
        mutate(hyperpar.name = name, hyperpar.value = value) %>% 
        select(run.id, hyperpar.name, hyperpar.value)
    } else {
      res = NULL
    }
    
  } else {
    res = collect(tbl(local.db, sql("SELECT * FROM [hyperpar.table]")))
  }
  
  return(res)
}

# @param tag Name of the tag of the benchmark datasets.
# @return [\code{data.frame}] Table with task.id, data.id, name, target.feature and metafeatures.
getMetaFeaturesTable = function(task.tag = "study_14") {
  df = listOMLTasks(tag = task.tag)
  drops = c("task.type", "status", "format", "estimation.procedure", "evaluation.measures", 
    "target.feature", "tags")
  df = df[, !(names(df) %in% drops)]
  
  return(df)
}