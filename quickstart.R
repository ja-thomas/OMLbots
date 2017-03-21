library(devtools)
load_all()

# On LRZ
# Interactiv
runBot(2, path = "/naslx/projects/ua341/di49ruw/test", upload = TRUE)

# In parallel mode
max.resources = list(walltime = 1000, memory = 1000, ncpus = 2, ntasks = 2)
runBot(20, path = "/naslx/projects/ua341/di49ruw/test", upload = TRUE, max.resources = max.resources)

for(i in 1:3){
  runBot(10, path = "test", upload = TRUE)
}

runBot(1, sample.configuration.fun = sampleDefaultConfiguration, upload = TRUE)

overview = getMlrRandomBotOverview("botV1")
print(overview)

tbl.results = getMlrRandomBotResults("botV1")
print(tbl.results)

tbl.hypPars = getMlrRandomBotHyperpars("botV1")
print(tbl.hypPars)

tbl.metaFeatures = getMetaFeatures(tag = "study_14")
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
