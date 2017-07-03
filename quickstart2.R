.libPaths("./Rlib")
devtools::load_all()

# On LRZ
# Serial cluster
setOMLConfig(apikey = "34ebc7dff3057d8e224e5beac53aea0e")
lrn.par.set = getMultipleLearners()
bot.nr = 1

for(i in 50:500){
  try(runBot(1000, path = paste0("test", i), 
         sample.learner.fun = sampleRandomLearner, sample.task.fun = sampleRandomAzureTask(bot.nr), 
         sample.configuration.fun = sampleRandomConfiguration,   
         lrn.ps.sets = lrn.par.set, upload = TRUE, extra.tag = c("botV1", paste0("AzureBot", bot.nr))))
}

