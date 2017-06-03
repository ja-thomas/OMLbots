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
updateReferenceTable(run.tag = "referenceV1", local.db)

# --------------------------------------------------------------------------------------------------------------------------------------
# Create surrogate models
local.db = initializeLocalDatabase(overwrite = FALSE)
tbl.results = getRunTable(local.db = local.db, numRuns = 5000000)
tbl.metaFeatures = getMetaFeaturesTable(local.db = local.db)
tbl.hypPars = getHyperparTable(local.db = local.db, numRuns = 1000000)
tbl.runTime = getRunTimeTable(local.db = local.db, numRuns = 250000)
tbl.resultsReference = getReferenceTable(local.db = local.db)

# get learner names
library(stringi)
learner.names = paste0("mlr.", names(lrn.par.set))
learner.names = stri_sub(learner.names, 1, -5)
# get task.ids
task.ids = unique(tbl.results$task.id)
# set surrogate model
surrogate.mlr.lrn = makeLearner("regr.ranger", par.vals = list(num.trees = 2000, num.threads = 10))

surrogate.measures = surrogate.time = list()

set.seed(123)
for (i in seq_along(learner.names)) {
  print(i)
  surrogate.measures[[i]] = makeSurrogateModel(measure.name = "area.under.roc.curve", 
    learner.name = learner.names[i], task.ids, lrn.par.set, tbl.results, tbl.hypPars, 
    tbl.metaFeatures, tbl.runTime, tbl.resultsReference, surrogate.mlr.lrn, min.experiments = 100)
  
  surrogate.time[[i]] = makeSurrogateModel(measure.name = "area.under.roc.curve", 
    learner.name = learner.names[i], task.ids, lrn.par.set, tbl.results, tbl.hypPars, 
    tbl.metaFeatures, tbl.runTime, tbl.resultsReference, surrogate.mlr.lrn, min.experiments = 100, 
    time = TRUE)
}
names(surrogate.measures) = learner.names
names(surrogate.time) = learner.names
save(surrogate.measures, surrogate.time, file = "surrogates.RData")

# Compare different surrogate models 
surrogate.mlr.lrns = list(
  makeLearner("regr.rpart"),
  makeLearner("regr.ranger", par.vals = list(num.trees = 2000, respect.unordered.factors = TRUE, num.threads = 6)),
  makeLearner("regr.cubist"),
  makeLearner("regr.kknn"),
  makeLearner("regr.lm"),
  makeLearner("regr.featureless")
)

surrogate.measures.benchmark = surrogate.time.benchmark = list()
set.seed(123)
for (i in seq_along(learner.names)) {
  print(i)
  surrogate.measures.benchmark[[i]] = makeSurrogateModel(measure.name = "area.under.roc.curve", 
    learner.name = learner.names[i], task.ids, lrn.par.set, tbl.results, tbl.hypPars, 
    tbl.metaFeatures, tbl.runTime, tbl.resultsReference, surrogate.mlr.lrns, min.experiments = 100, benchmark = TRUE)
 
  surrogate.time.benchmark[[i]] = makeSurrogateModel(measure.name = "area.under.roc.curve", 
    learner.name = learner.names[i], task.ids, lrn.par.set, tbl.results, tbl.hypPars, 
    tbl.metaFeatures, tbl.runTime, tbl.resultsReference, surrogate.mlr.lrns, min.experiments = 100, benchmark = TRUE, time = TRUE)
}

names(surrogate.measures.benchmark) = learner.names
names(surrogate.time.benchmark) = learner.names
save(surrogate.measures.benchmark, surrogate.time.benchmark, file = "surrogates_benchmark.RData")

for(i in seq_along(learner.names)) {
  print(paste(learner.names[i], "---------------------------------------------------------------------------------------"))
  print(surrogate.measures.benchmark[[i]]$result)
}

for(i in seq_along(learner.names)) {
  print(paste(learner.names[i], "---------------------------------------------------------------------------------------"))
  print(surrogate.time.benchmark[[i]]$result)
}
# cubist and ranger are the best surrogate models for the measures
# Outliers destroy mse result for surrogate models for the time. RSQ: ranger and cubist in general the best model
###################### 

#create pareto-front 
#pick random points from pareto-front for validation runs to check results
library(emoa)
load("surrogates.RData")
meta.features = tbl.metaFeatures[2,] %>% select(., majority.class.size, minority.class.size, number.of.classes,
  number.of.features, number.of.instances, number.of.numeric.features, number.of.symbolic.features)

pdf("paretofronts_example.pdf", width = 10, height = 8)
for(i in seq_along(learner.names)) {
  print(i)
  print(learner.names[i])
  par.front = createParetoFront(learner.name = learner.names[i], lrn.par.set, surrogate.measures, surrogate.time, meta.features, n.points = 10000) 
  plotParetoFront(par.front)
}
dev.off()

plotParetoFront(par.front, plotly = TRUE)
plotParetoFront(par.front, plotly = FALSE)
