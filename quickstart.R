library(devtools)
load_all()

# On LRZ
# Serial cluster
setOMLConfig(apikey = "34ebc7dff3057d8e224e5beac53aea0e")
max.resources = list(walltime = 3600*5, memory = 2000)

for(i in 1:1000) {
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

# Database extraction

local.db = initializeLocalDatabase(overwrite = FALSE)
run.tag = "botV1"
updateLocalDatabase()
updateRunTable(run.tag = run.tag, local.db) # geht das gescheit?
updateHyperparTable(run.tag = run.tag, local.db)
updateMetaTable(task.tag = "study_14", local.db)
updateRunTimeTable(local.db)

# --------------------------------------------------------------------------------------------------------------------------------------
# Create surrogate models
tbl.results = getRunTable(local.db = local.db, numRuns = 5000000)
tbl.metaFeatures = getMetaFeaturesTable(local.db = local.db)
tbl.hypPars = getHyperparTable(local.db = local.db, numRuns = 1000000)
tbl.runTime = getRunTimeTable(local.db = local.db, numRuns = 250000)
tbl.resultsReference = getRunTable(run.tag = "referenceV1", numRuns = 20000, local.db = NULL)

# get learner names
library(stringi)
learner.names = paste0("mlr.", names(lrn.par.set))
learner.names = stri_sub(learner.names, 1, -5)
# get task.ids
task.ids = unique(tbl.results$task.id)
# set surrogate model
surrogate.mlr.lrn = makeLearner("regr.ranger", par.vals = list(num.trees = 2000))
# user time fehlt noch im Modell
# add sci.mark scale -> Daniel

surrogates_measures = surrogates_time = list()

for (i in seq_along(learner.names)) {
  print(i)
  surrogates_measures[[i]] = makeSurrogateModel(measure.name = "area.under.roc.curve", 
    learner.name = learner.names[i], task.ids, lrn.par.set, tbl.results, tbl.hypPars, 
    tbl.metaFeatures, tbl.runTime, tbl.resultsReference, surrogate.mlr.lrn, min.experiments = 100)
  
  surrogates_time[[i]] = makeSurrogateModel(measure.name = "area.under.roc.curve", 
    learner.name = learner.names[i], task.ids, lrn.par.set, tbl.results, tbl.hypPars, 
    tbl.metaFeatures, tbl.runTime, tbl.resultsReference, surrogate.mlr.lrn, min.experiments = 100, 
    time = TRUE)
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
  tbl.metaFeatures, tbl.runTime, tbl.resultsReference, surrogate.mlr.lrns, min.experiments = 100, benchmark = TRUE)

###################### 

#create pareto-front 
#pick random points from pareto-front for validation runs to check results


