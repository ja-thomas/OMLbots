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
# @param excl.run.ids ids that should not be downloaded
# @param numRuns maximum Number of runs that should be downloaded in the first step; 
# should be set to a value so that it downloads all available runs
# @param n maximum number of runs that should be downloaded
# @return [\code{data.frame}] Table with run.id, hyperparameter name & value.
getHyperparTable = function(run.tag = "mlrRandomBot", excl.run.ids = NULL, numRuns = 300000, n = 1000) {
  #FIXME: Fix this once OpenML offers different solution
  runs = do.call("rbind", 
    lapply(0:floor(numRuns/10000), function(i) {
      return(listOMLRuns(tag = run.tag, limit = 10000, offset = (10000 * i) + 1, uploader.id = 2702)) # OpenML_bot
    })
  )
  if(!is.null(excl.run.ids))
    runs = runs[!(runs$run.id %in% excl.run.ids), ]
  
  runs = runs[1:n,]
  
  # FIXME: HORRIBLE performance
  res_total = data.frame()
  flow.ids = unique(runs$flow.id)
  
  for(i in seq_along(flow.ids)) {
    run.ids = runs$run.id[runs$flow.id == flow.ids[i]]
    res = lapply(run.ids, function(x){ #FIXME: Increase performance once OpenML offers solution
      pars = tryCatch(getOMLRunParList(getOMLRun(x)), error = function(cond){return(NA)}) 
      if(length(pars) > 0){
        pars = data.frame(do.call(rbind, lapply(pars, function(p) do.call(cbind, p))))
        pars$run.id = x
        pars
      } else {
        pars = data.frame(name = "no_pars", value = NA, component = NA, stringsAsFactors = FALSE)
      }
      
      pars$run.id = x
      pars
    })
    res = do.call(rbind, res)
    
    res = res %>% 
      mutate(hyperpar.name = name, hyperpar.value = value) %>% 
      select(run.id, hyperpar.name, hyperpar.value)
    res = try(addDefaultValues(res))
    if(ncol(res) == 3 && !is.character(res))
      res_total = rbind(res_total, res)
  }
  return(res_total)
}

# @param res Long table with hyperparameters generated in getHyperparTable
# @return [\code{data.frame}] Long Table with added values for the defaults.
addDefaultValues = function(res) {
  learner.name = try(listOMLRunEvaluations(run.id = res$run.id[1])$learner.name)
  
  if(learner.name == "classif.rpart") { # rpart
    levels(res$hyperpar.value) = c(levels(res$hyperpar.value), 30, 20)
    data_wide <- spread(res, hyperpar.name, hyperpar.value)
    data_wide$xval = NULL
    data_wide$maxdepth[is.na(data_wide$maxdepth)] = 30
    data_wide$minsplit[is.na(data_wide$minsplit)] = 20
    res = gather(data_wide, hyperpar.name, hyperpar.value, -run.id)
  }
  if(learner.name == "classif.kknn") { # kknn
    res$hyperpar.name = "k"
    levels(res$hyperpar.value) = c(levels(res$hyperpar.value), 7)
    res$hyperpar.value[is.na(res$hyperpar.value)] = 7
    res
  }
  if(learner.name == "classif.svm") { # svm
    levels(res$hyperpar.value) = c(levels(res$hyperpar.value), "radial", 3)
    data_wide <- spread(res, hyperpar.name, hyperpar.value)
    data_wide$kernel[is.na(data_wide$kernel)] = "radial"
    nas = is.na(data_wide[data_wide$kernel == "polynomial",]$degree)
    data_wide[data_wide$kernel == "polynomial",]$degree[nas] = 3
    res = gather(data_wide, hyperpar.name, hyperpar.value, -run.id)
  }
  if(learner.name == "classif.ranger") { # ranger
    levels(res$hyperpar.value) = c(levels(res$hyperpar.value), TRUE, 500, FALSE)
    data_wide <- spread(res, hyperpar.name, hyperpar.value)
    data_wide$verbose = NULL
    data_wide$num.trees[is.na(data_wide$num.trees)] = 500
    data_wide$replace[is.na(data_wide$replace)] = TRUE
    data_wide$respect.unordered.factors[is.na(data_wide$respect.unordered.factors)] = FALSE
    res = gather(data_wide, hyperpar.name, hyperpar.value, -run.id)
  }
  if(learner.name == "classif.xgboost") { # xgboost
    levels(res$hyperpar.value) = c(levels(res$hyperpar.value), "gbtree", 1, 6)
    data_wide <- spread(res, hyperpar.name, hyperpar.value)
    data_wide$verbose = NULL
    data_wide$nrounds[is.na(data_wide$nrounds)] = 1
    data_wide$booster[is.na(data_wide$booster)] = "gbtree"
    nas = is.na(data_wide[data_wide$booster == "gbtree",]$max_depth)
    data_wide[data_wide$booster == "gbtree",]$max_depth[nas] = 6
    res = gather(data_wide, hyperpar.name, hyperpar.value, -run.id)
  }
  return(res)
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