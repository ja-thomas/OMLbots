library(devtools)
load_all()

runBot(1)

runBot(1, sample.configuration.fun = sampleDefaultConfiguration, upload = TRUE)

overview = getMlrRandomBotOverview()
print(overview)

results = getMlrRandomBotResults()
print(results)

hypPars = getMlrRandomBotHyperpars()
print(hypPars)

metaFeatures = getMetaFeatures(tag = "study_14")
print(head(metaFeatures))

# surrogate function stuff
x <- makeMeasureTimePrediction(measure.name = "area.under.roc.curve",
  flow.id = 5526,
  task.id = 3950, 
  lrn.par.set = lrn.par.set,
  n = 2000,
  tbl.results = results,
  tbl.hypPars = hypPars,
  tbl.metaFeatures = metaFeatures)

plot(x$k, x$pred.measure.value)
plot(x$k, x$pred.time)
plot(x$pred.measure.value, x$pred.time)  

plot(jitter(pred.data$pred.measure.value), jitter(pred.data$pred.time))

#create pareto-front 
#pick random points from pareto-front for validation runs to check results
