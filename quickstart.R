library(devtools)
load_all()

# On LRZ
# Serial cluster
setOMLConfig(apikey = "34ebc7dff3057d8e224e5beac53aea0e")
max.resources = list(walltime = 3600*6, memory = 2000)

for(i in 2:1000) {
  try(runBot(500, path = paste0("/naslx/projects/ua341/di49ruw/test", i), 
    sample.learner.fun = sampleRandomLearner, sample.task.fun = sampleRandomTask, 
    sample.configuration.fun = sampleRandomConfiguration, max.resources = max.resources,  
    lrn.ps.sets = lrn.par.set, upload = TRUE, extra.tag = "botV1"))
}

# Locally
for(i in 1:1000){
  try(runBot(100, path = paste0("test", i), 
    sample.learner.fun = sampleRandomLearner, sample.task.fun = sampleRandomTask, 
    sample.configuration.fun = sampleRandomConfiguration,   
    lrn.ps.sets = lrn.par.set, upload = TRUE, extra.tag = "botV1"))
}

overview = getMlrRandomBotOverview("botV1")
print(overview)
# Geht nicht bei zu großen Ergebnissen; stattdessen mit limit und listOMLRuns 
tag = "mlrRandomBot"
numRuns = 160000
results = do.call("rbind", 
  lapply(0:floor(numRuns/10000), function(i) {
    return(listOMLRuns(tag = tag, limit = 10000, offset = (10000 * i) + 1))
  })
)
table(results$flow.id, results$task.id)
table(results$uploader)

updateLocalDatabase()
local.db = initializeLocalDatabase()

for(i in 2:11){
  deleteOMLObject(results$run.id[i], object = "run")
}

tbl.results = getRunTable(local.db = NULL, numRuns = 170000)

print(tbl.results)

tbl.hypPars = getHyperparTable(local.db = local.db)
print(tbl.hypPars)

tbl.metaFeatures = getMetaFeaturesTable(local.db = NULL)
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

# --------------------------------------------------------------------------------------------------------------------------------------
# Create surrogate models
library(repmis)
source_data("https://github.com/PhilippPro/tunability/blob/master/hypPars.RData?raw=True")
tbl.results = getRunTable(run.tag = "botV1", numRuns = 200000)
tbl.metaFeatures = getMetaFeaturesTable(local.db = NULL)

tbl.results = getRunTable(run.tag = "referenceV1", numRuns = 200000)
tbl.results = data.table(tbl.results)
tbl.results[tbl.results$measure.name == "area.under.roc.curve", list(AUC=mean(measure.value)), by = "data.name,learner.name"]

is.data.table(tbl.results)
table(tbl.results$measure.name)


# get learner names
library(stringi)
learner.names = paste0("mlr.", names(lrn.par.set))
learner.names = stri_sub(learner.names, 1, -5)
# get task.ids
task.ids = unique(tbl.results$task.id)
# set surrogate model
surrogate.mlr.lrn = makeLearner("regr.ranger", par.vals = list(num.trees = 2000))
# user time fehlt noch im Modell!! -> Daniel
# +Skalierung mit Reference learner
#tbl.results.ref = getRunTable("reference")

surrogates_measures = surrogates_time = list()

for (i in seq_along(learner.names)) {
surrogates_measures[[i]] = makeSurrogateModels(measure.name = "area.under.roc.curve", 
  learner.name = learner.names[i], task.ids,   lrn.par.set, tbl.results, tbl.hypPars, 
  tbl.metaFeatures, surrogate.mlr.lrn, min.experiments = 100)

surrogates_time[[i]] = makeSurrogateModels(measure.name = "area.under.roc.curve", learner.name = learner.names[i], task.ids, tbl.results, tbl.hypPars, 
  tbl.metaFeatures, lrn.par.set, surrogate.mlr.lrn, min.experiments = 100, time = TRUE)
}
names(surrogates_measures) = learner.names
names(surrogates_time) = learner.names
save(surrogates_measures, surrogates_time, file = "surrogates.RData")

# Compare different surrogate models 
surrogate.mlr.lrns = list(
  makeLearner("regr.ranger", par.vals = list(num.trees = 2000)), 
  makeLearner("regr.cubist")
)
res = makeSurrogateModels(measure.name = "area.under.roc.curve", learner.name = learner.names[1], task.ids, lrn.par.set, tbl.results, tbl.hypPars, 
  tbl.metaFeatures, surrogate.mlr.lrns, min.experiments = 100, benchmark = TRUE)



