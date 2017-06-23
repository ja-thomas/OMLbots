.libPaths("./Rlib")
devtools::load_all()

# On LRZ
# Serial cluster
setOMLConfig(apikey = "34ebc7dff3057d8e224e5beac53aea0e")
lrn.par.set = getMultipleLearners()
i = sample(1:1e7, 1)
bot.nr = 1

for(i in 1:20){
  runBot(10000, path = paste0("test", i), 
         sample.learner.fun = sampleRandomLearner, sample.task.fun = sampleRandomAzureTask(bot.nr), 
         sample.configuration.fun = sampleRandomConfiguration,   
         lrn.ps.sets = lrn.par.set, upload = TRUE, extra.tag = c("botV1", paste0("AzureBot", bot.nr)))
}

data = lapply(1:5, function(x) listOMLRuns(tag = paste0("AzureBot", x)))
dt = do.call(rbind, data)
setDT(dt)
dt[,.N,by=tags] #FIXME: xgboost seems to crash the bot on Azure. Might be some parallelization issue.
