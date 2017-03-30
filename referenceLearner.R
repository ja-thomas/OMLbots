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

for (i in seq_along(tasks)) {
  fixed.task = function() list(id = tasks$task.id[i], name = tasks$name[i])
  
  runBot(10 , sample.learner.fun = sampleRandomLearner, 
    sample.task.fun = fixed.task, sample.configuration.fun = sampleRandomConfiguration, 
    lrn.ps.sets = reference1.lrn.par.set, upload = TRUE,
    path = "reference", extra.tag = "reference")
  unlink("reference", recursive = TRUE)
  
  runBot(10 , sample.learner.fun = sampleRandomLearner, 
    sample.task.fun = fixed.task, sample.configuration.fun = sampleRandomConfiguration, 
    lrn.ps.sets = reference2.lrn.par.set, upload = TRUE,
    path = "reference", extra.tag = "reference")
  unlink("reference", recursive = TRUE)
}

overview = getMlrRandomBotOverview("reference")
print(overview)

tbl.results = getMlrRandomBotResults("reference")
print(tbl.results)

