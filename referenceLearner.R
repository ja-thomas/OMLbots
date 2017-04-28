reference1.lrn.par.set = makeLrnPsSets(learner = makeLearner("classif.ranger", predict.type = "prob"), 
  param.set = makeParamSet(
    makeIntegerParam("num.trees", lower = 2000, upper = 2000, default = 2000)
    )
)

reference2.lrn.par.set = makeLrnPsSets(learner = makeLearner("classif.featureless", predict.type = "prob"), 
  param.set = makeParamSet(
    makeDiscreteParam("method", values = "majority", default = "majority")
  )
)

tasks = listOMLTasks(number.of.classes = 2L, number.of.missing.values = 0, 
  data.tag = "study_14", estimation.procedure = "10-fold Crossvalidation")

for (i in 1:nrow(tasks)) {
  fixed.task = function() list(id = tasks$task.id[i], name = tasks$name[i])
  
  runBot(10, sample.learner.fun = sampleRandomLearner, 
    sample.task.fun = fixed.task, sample.configuration.fun = sampleRandomConfiguration, 
    lrn.ps.sets = reference1.lrn.par.set, upload = TRUE,
    path = "reference", extra.tag = "referenceV1")
  unlink("reference", recursive = TRUE)
  
  runBot(10, sample.learner.fun = sampleRandomLearner, 
    sample.task.fun = fixed.task, sample.configuration.fun = sampleRandomConfiguration, 
    lrn.ps.sets = reference2.lrn.par.set, upload = TRUE,
    path = "reference", extra.tag = "referenceV1")
  unlink("reference", recursive = TRUE)
}

overview = getMlrRandomBotOverview("referenceV1")
print(overview)

a = listOMLRuns(tag = "referenceV1")
getRunDf("referenceV1")
tbl.results = getRunTable("referenceV1")
print(tbl.results)

for(i in 1:nrow(a)){
  deleteOMLObject(a$run.id[i], object = "run")
}

