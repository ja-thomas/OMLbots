#' @param tag Name of the tag of the benchmark study.
#' @return [\code{data.frame}] Table with number of experiments for each learner and each task. 
getMlrRandomBotOverview = function(tag = "mlrRandomBot") {
  df = listOMLRunEvaluations(tag = tag) %>% 
    mutate(flow.version = c(stri_match_last(flow.name, regex = "[[:digit:]]+\\.*[[:digit:]]*")),
      learner.name = stri_replace_last(flow.name, replacement = "", regex = "[([:digit:]]+\\.*[[:digit:]*)]"))
  
  return(as.data.frame.matrix(table(df$learner.name, df$data.name)))
}

#' Small helper function to get a clean df from listOMLRuns
getRunDf = function(run.tag, numRuns, excl.run.ids){
  max.limit = 10000 #FIXME: fix this once OpenML offers new solution
  results = do.call("rbind", 
    lapply(seq(0, numRuns/max.limit), function(i) {
      run.df = tryCatch(listOMLRuns(tag = run.tag, 
        limit = max.limit, #min(max.limit, numRuns - max.limit * i), 
        offset = (max.limit * i) + 1,
        uploader.id = 2702), # only OpenML_bot results
        error = function(cond){return(NULL)})
      return(run.df)
    })
  )
  
  results = results[results$run.id %in% setdiff(results$run.id, excl.run.ids),]
  return(results)
}

#' @param tag Name of the tag of the benchmark study.
#' @return [\code{data.frame}] Table with run.id, task.id, flow.id, flow.name, measure values.
getRunTable = function(run.tag = "mlrRandomBot", numRuns = 200000, excl.run.ids = NULL, local.db = NULL) {
  if(is.null(local.db)){
    results = getRunDf(run.tag = run.tag, numRuns = numRuns, excl.run.ids = excl.run.ids)
    
    if(nrow(results) > 0){
      res.ids = results$run.id
      #res.chunks = split(seq_along(res.ids), ceiling(seq_along(res.ids)/475))
      res = do.call("rbind", 
        lapply(seq_along(res.ids), function(i) {
          #return(listOMLRunEvaluations(run.id = res.ids[res.chunks[[i]]]))
          return(listOMLRunEvaluations(run.id = res.ids[i]))
        })
      )
      
      df = res %>%
        gather(., key = "measure.name", value = "measure.value", -(run.id:upload.time), na.rm = TRUE) %>%
        mutate(flow.version = c(stri_match_last(flow.name, regex = "[[:digit:]]+\\.*[[:digit:]]*")),
          learner.name = stri_replace_last(flow.name, replacement = "", regex = "[([:digit:]]+\\.*[[:digit:]*)]")) %>%
        filter(substr(measure.name,1,5) != "array")
    } else {
      df = NULL
    }

  } else {
    df = collect(tbl(local.db, sql("SELECT * FROM [run.table]")), n = numRuns)
  }

  return(df)
}

#' @param tag Name of the tag of the benchmark study.
#' @param excl.run.ids ids that should not be downloaded
#' @param numRuns maximum Number of runs that should be downloaded in the first step; 
#' should be set to a value so that it downloads all available runs
#' @param n maximum number of runs that should be downloaded
#' @return [\code{data.frame}] Table with run.id, hyperparameter name & value.
getHyperparTable = function(run.tag = "mlrRandomBot", numRuns = 200000, excl.run.ids = NULL, local.db = NULL, n = 200000) {
  if(is.null(local.db)){
    runs = getRunDf(run.tag = run.tag, numRuns = numRuns, excl.run.ids = excl.run.ids)
    runs = runs[sample(1:nrow(runs), size = min(n,nrow(runs))),]
    
    if(nrow(runs) > 0){
      
      res_total = data.frame()
      flow.ids = unique(runs$flow.id)
      
      for(i in seq_along(flow.ids)) {
        run.ids = runs$run.id[runs$flow.id == flow.ids[i]]
        # FIXME: HORRIBLE performance
        res = lapply(run.ids, function(x){ #FIXME: Increase performance once OpenML offers solution
          pars = tryCatch(getOMLRunParList(getOMLRun(x)), error = function(cond){return(NA)}) 
          if(length(pars) > 0 && is.na(pars)){
            pars = data.frame(name = "run_NA", value = NA, component = NA, stringsAsFactors = FALSE)
          } else if (length(pars) > 0 && !is.na(pars)){
            pars = data.frame(do.call(rbind, lapply(pars, function(p) do.call(cbind, p))), stringsAsFactors = FALSE)
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
        res = try(addDefaultValues(res))
        if(ncol(res) == 3 && !is.character(res))
          res_total = rbind(res_total, res)
      }
    } else {
      res_total = NULL
    }
    
  } else {
    res_total = collect(tbl(local.db, sql("SELECT * FROM [hyperpar.table]")), n = numRuns)
  }

  return(res_total)
}

#' @param res Long table with hyperparameters generated in getHyperparTable
#' @return [\code{data.frame}] Long Table with added values for the defaults.
addDefaultValues = function(res) {
  learner.name = try(listOMLRunEvaluations(run.id = res$run.id[1])$learner.name)
  
  if(learner.name == "classif.glmnet") { # glmnet
    data_wide = spread(res, hyperpar.name, hyperpar.value)
    data_wide$s = NULL
    res = gather(data_wide, hyperpar.name, hyperpar.value, -run.id)
  }
  
  if(learner.name == "classif.rpart") { # rpart
    levels(res$hyperpar.value) = c(levels(res$hyperpar.value), 30, 20)
    data_wide = spread(res, hyperpar.name, hyperpar.value)
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
    data_wide = spread(res, hyperpar.name, hyperpar.value)
    data_wide$kernel[is.na(data_wide$kernel)] = "radial"
    nas = is.na(data_wide[data_wide$kernel == "polynomial",]$degree)
    data_wide[data_wide$kernel == "polynomial",]$degree[nas] = 3
    res = gather(data_wide, hyperpar.name, hyperpar.value, -run.id)
  }
  if(learner.name == "classif.ranger") { # ranger
    levels(res$hyperpar.value) = c(levels(res$hyperpar.value), TRUE, 500, FALSE)
    data_wide = spread(res, hyperpar.name, hyperpar.value)
    data_wide$verbose = NULL
    data_wide$num.threads = NULL
    data_wide$num.trees[is.na(data_wide$num.trees)] = 500
    data_wide$replace[is.na(data_wide$replace)] = TRUE
    data_wide$respect.unordered.factors[is.na(data_wide$respect.unordered.factors)] = FALSE
    res = gather(data_wide, hyperpar.name, hyperpar.value, -run.id)
  }
  if(learner.name == "classif.xgboost") { # xgboost
    levels(res$hyperpar.value) = c(levels(res$hyperpar.value), "gbtree", 1, 6)
    data_wide = spread(res, hyperpar.name, hyperpar.value)
    data_wide$verbose = NULL
    data_wide$nrounds[is.na(data_wide$nrounds)] = 1
    data_wide$booster[is.na(data_wide$booster)] = "gbtree"
    nas = is.na(data_wide[data_wide$booster == "gbtree",]$max_depth)
    data_wide[data_wide$booster == "gbtree",]$max_depth[nas] = 6
    res = gather(data_wide, hyperpar.name, hyperpar.value, -run.id)
  }
  return(res)
}


#' @param tag Name of the tag of the benchmark datasets.
#' @return [\code{data.frame}] Table with task.id, data.id, name, target.feature and metafeatures.
getMetaFeaturesTable = function(task.tag = "study_14", local.db = NULL) {
  if(is.null(local.db)){
    df = listOMLTasks(tag = task.tag)
    drops = c("task.type", "status", "format", "estimation.procedure", "evaluation.measures", 
      "target.feature", "tags")
    df = df[, !(names(df) %in% drops)]
  } else {
    df = collect(tbl(local.db, sql("SELECT * FROM [meta.table]")))
  }

  return(df)
}

#' Scrape user time and scibench for a run. Only works for runs by OMLBot
scrapeRunTime = function(run.id) {
  oml.url = "https://www.openml.org/r/"
  
  xml.scraper = function(run.id){
    html_body = getURL(paste0(oml.url, run.id))
    parsed_body = htmlParse(html_body, asText =  TRUE, encoding = "utf-8")
    
    xml.link = xpathSApply(parsed_body, "//*/a[@class = \"btn btn-fab btn-raised btn-material-red resultfile\"]/@href")[1]
    xml.file = getURL(xml.link)
    xml.parsed = xmlParse(xml.file)
    return(xmlToList(xml.parsed))
  }
  
  xml.list = try(xml.scraper(run.id))
  if(class(xml.list) != "try-error"){
    df = data.frame(run.id = run.id, 
      run.time = ifelse(xml.list$output_data[3]$evaluation$name == "usercpu_time_millis", 
        as.numeric(xml.list$output_data[3]$evaluation$value), NA),
      sci.mark = ifelse(xml.list$output_data[4]$evaluation$name == "scimark_benchmark",
        as.numeric(xml.list$output_data[4]$evaluation$value), NA))
  } else {
    df = data.frame(run.id = run.id,
      run.time = NA,
      sci.mark = NA)
  }
  
  Sys.sleep(0.1) #Just being nice to the server :)
  return(df)
}

#' Get user times for a vector of run ids
getRunTime = function(run.ids) {
  res = lapply(run.ids, function(x) scrapeRunTime(x))
  df = do.call(rbind, res)
  return(df)
}

#' Get runtime table from the database
getRunTimeTable = function(local.db = NULL, numRuns = 200000) {
  if(!is.null(local.db))
    collect(tbl(local.db, sql("SELECT * FROM [runtime.table]")), n = numRuns)
}

