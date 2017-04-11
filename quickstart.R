library(devtools)
load_all()

# On LRZ
# Serial cluster
setOMLConfig(apikey = "34ebc7dff3057d8e224e5beac53aea0e")
max.resources = list(walltime = 10000, memory = 2000)

for(i in 1:1000) {
  try(runBot(100, path = paste0("/naslx/projects/ua341/di49ruw/test", i), 
    sample.learner.fun = sampleRandomLearner, sample.task.fun = sampleRandomTask, 
    sample.configuration.fun = sampleRandomConfiguration, max.resources = max.resources,  
    lrn.ps.sets = lrn.par.set, upload = TRUE, extra.tag = "botV1"))
}
# Locally
for(i in 1:3){
  runBot(10, path = "test", upload = TRUE)
}

#overview = getMlrRandomBotOverview("botV1")
#print(overview)

updateLocalDatabase()
local.db = initializeLocalDatabase()

tbl.results = getRunTable(local.db = local.db)
print(tbl.results)

tbl.hypPars = getHyperparTable(local.db = local.db)
print(tbl.hypPars)

tbl.metaFeatures = getMetaFeaturesTable(local.db = local.db)
print(head(tbl.metaFeatures))

# surrogate function stuff
x <- makeMeasureTimePrediction(measure.name = "area.under.roc.curve",
  learner.name = "mlr.classif.kknn",
  task.id = 3950, 
  lrn.par.set = lrn.par.set,
  n = 2000,
  tbl.results = tbl.results,
  tbl.hypPars = tbl.hypPars,
  tbl.metaFeatures = tbl.metaFeatures)

plot(x$k, x$pred.measure.value)
plot(x$k, x$pred.time)
plot(x$pred.measure.value, x$pred.time)  

#create pareto-front 
#pick random points from pareto-front for validation runs to check results
