.libPaths("./Rlib")
devtools::load_all()

# On LRZ
# Serial cluster
setOMLConfig(apikey = "34ebc7dff3057d8e224e5beac53aea0e")
lrn.par.set = getMultipleLearners()
bot.nr = 1
dt = fread("Azure_Overview.csv")
dt[,GetDate := as.Date(GetDate, format = "%d/%m/%Y")]
max.date = max(dt$GetDate)
task.ignore = dt[GetDate == max.date,.(NrRuns = sum(NrRuns)), by = task.id][NrRuns > 50000, task.id]

for(i in 1000:2500){
  try(runBot(1000, path = paste0("bot", i), 
         sample.learner.fun = sampleRandomLearner, sample.task.fun = sampleRandomAzureTask(bot.nr, task.ignore), 
         sample.configuration.fun = sampleRandomConfiguration,   
         lrn.ps.sets = lrn.par.set, upload = TRUE, extra.tag = c("botV1", paste0("AzureBot", bot.nr))))
}
