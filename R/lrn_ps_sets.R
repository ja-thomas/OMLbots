# List of mlr learner and parameter set pairs
lrn.ps.sets = list(
  list(
    lrn = makeLearner("classif.svm", predict.type = "prob"),
    ps = makeParamSet(
      makeDiscreteParam("kernel", values = c("linear", "polynomial", "radial")),
      makeNumericParam("cost", lower = -10, upper = 10, trafo = function(x) 2^x),
      makeNumericParam("gamma", lower = -10, upper = 10, trafo = function(x) 2^x, requires = quote(kernel == "radial")),
      makeIntegerParam("degree", lower = 2, upper = 5, requires = quote(kernel == "polynomial"))
    )
  ),
  list(
    lrn = makeLearner("classif.kknn", predict.type = "prob"),
    ps = makeParamSet(
      makeIntegerParam("k", lower = 1, upper = 30)
    )
  )
)
