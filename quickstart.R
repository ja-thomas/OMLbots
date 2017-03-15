library(devtools)
load_all()

for(i in 1:10){
  runBot(100, path = "test", upload = TRUE)
}

runBot(1, sample.configuration.fun = sampleDefaultConfiguration, upload = TRUE)

overview = getMlrRandomBotOverview()
print(overview)

tbl.results = getMlrRandomBotResults()
print(tbl.results)

tbl.hypPars = getMlrRandomBotHyperpars()
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
