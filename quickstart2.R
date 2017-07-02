.libPaths("./Rlib")
devtools::load_all()

# On LRZ
# Serial cluster
setOMLConfig(apikey = "34ebc7dff3057d8e224e5beac53aea0e")
lrn.par.set = getMultipleLearners ()
i = sample(1:1e7, 1)
bot.nr = 1

for(i in 1:20){
  runBot(10000, path = paste0("test", i), 
         sample.learner.fun = sampleRandomLearner, sample.task.fun = sampleRandomAzureTask(bot.nr), 
         sample.configuration.fun = sampleRandomConfiguration,   
         lrn.ps.sets = lrn.par.set, upload = TRUE, extra.tag = c("botV1", paste0("AzureBot", bot.nr)))
}


listFullOMLRuns = function(tag = NULL){
  callOMLRuns = function(i) {
    run.df = listOMLRuns(tag = tag, 
      limit = max.limit,
      offset = (max.limit * i) + 1)
    return(run.df)
  }
  
  df = tryCatch({listOMLRuns(tag = tag)},
    error = function(err){
      max.limit = 10000 #FIXME: fix this once OpenML offers new solution
      nr.results = stringr::str_extract(err, "[0-9]{5,}")
      message(paste0("Adjusting function to get ", nr.results, " results."))
      
      ls = lapply(seq(0, as.numeric(nr.results)/max.limit), function(i) callOMLRuns(i))
      results = do.call("rbind", ls)
      return(results)
    })
  
  return(df)
}


data = lapply(1:25, function(x) listFullOMLRuns(tag = paste0("AzureBot", x)))
dt = do.call(rbind, data)
setDT(dt)
dt[,.N,by=tags]
