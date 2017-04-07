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
getMlrRandomBotResults = function(tag = "mlrRandomBot") {
  df = listOMLRunEvaluations(tag = tag) %>%
    gather(., key = "measure.name", value = "measure.value", -(run.id:upload.time), na.rm = TRUE) %>%
    mutate(flow.version = c(stri_match_last(flow.name, regex = "[[:digit:]]+\\.*[[:digit:]]*")),
      learner.name = stri_replace_last(flow.name, replacement = "", regex = "[([:digit:]]+\\.*[[:digit:]*)]"))

  return(df)
}

# @param tag Name of the tag of the benchmark study.
# @return [\code{data.frame}] Table with run.id, hyperparameter name & value.
getMlrRandomBotHyperpars = function(tag = "mlrRandomBot") {
  runs = listOMLRuns(tag = tag)
  res = lapply(runs$run.id, function(x){
    pars = getOMLRunParList(getOMLRun(x))
    #FIXME: Just kill me now...
    pars = data.frame(do.call(rbind, lapply(pars, function(p) do.call(cbind, p))))
    pars$run.id = x
    pars
  })
  res = do.call(rbind, res)
  res = res %>% 
    mutate(hyperpar.name = name, hyperpar.value = value) %>% 
    select(run.id, hyperpar.name, hyperpar.value)
  
  return(res)
}

addDefaultValues = function(res, flow.id) {
  if(flow.id %in% c(5624, 5859)) { # rpart
    levels(res$hyperpar.value) = c(levels(res$hyperpar.value), 30, 20)
    data_wide <- spread(res, hyperpar.name, hyperpar.value)
    data_wide$maxdepth[is.na(data_wide$maxdepth)] = 30
    data_wide$minsplit[is.na(data_wide$minsplit)] = 20
    res = gather(data_wide, hyperpar.name, hyperpar.value, -run.id, -xval)
  }
  
  if(flow.id %in% c(5890, 5972)) { # kknn
    res$hyperpar.name = "k"
    levels(res$hyperpar.value) = c(levels(res$hyperpar.value), 7)
    res$hyperpar.value[is.na(res$hyperpar.value)] = 7
  }
  
  if(flow.id %in% c(5891,5969)) { # svm
    levels(res$hyperpar.value) = c(levels(res$hyperpar.value), "radial", 3)
    data_wide <- spread(res, hyperpar.name, hyperpar.value)
    data_wide$kernel[is.na(data_wide$kernel)] = "radial"
    nas = is.na(data_wide[data_wide$kernel == "polynomial",]$degree)
    data_wide[data_wide$kernel == "polynomial",]$degree[nas] = 3
    res = gather(data_wide, hyperpar.name, hyperpar.value, -run.id)
  }
  
  if(flow.id %in% c(5889, 5964, 5965, 5968)) { # ranger
    levels(res$hyperpar.value) = c(levels(res$hyperpar.value), TRUE, 500, FALSE)
    data_wide <- spread(res, hyperpar.name, hyperpar.value)
    data_wide$num.trees[is.na(data_wide$num.trees)] = 500
    data_wide$replace[is.na(data_wide$replace)] = TRUE
    data_wide$respect.unordered.factors[is.na(data_wide$respect.unordered.factors)] = FALSE
    res = gather(data_wide, hyperpar.name, hyperpar.value, -run.id)
  }
    
    if(flow.id %in% c(5906, 5963, 5971, 6003)) { # xgboost
      levels(res$hyperpar.value) = c(levels(res$hyperpar.value), "gbtree", 1, 6)
      data_wide <- spread(res, hyperpar.name, hyperpar.value)
      data_wide$nrounds[is.na(data_wide$nrounds)] = 1
      data_wide$booster[is.na(data_wide$booster)] = "gbtree"
      nas = is.na(data_wide[data_wide$booster == "gbtree",]$max_depth)
      data_wide[data_wide$booster == "gbtree",]$max_depth[nas] = 6
      res = gather(data_wide, hyperpar.name, hyperpar.value, -run.id)
    }
      res
}

# @param tag Name of the tag of the benchmark datasets.
# @return [\code{data.frame}] Table with task.id, data.id, name, target.feature and metafeatures.
getMetaFeatures = function(tag = "study_14") {
  df = listOMLTasks(tag = tag)
  drops = c("task.type", "status", "format", "estimation.procedure", "evaluation.measures", 
    "target.feature", "tags")
  df = df[, !(names(df) %in% drops)]
  
  return(df)
}