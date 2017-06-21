## ---- message = FALSE----------------------------------------------------
library(batchtools)
reg = makeExperimentRegistry(file.dir = NA, seed = 1)

## ----include=FALSE-------------------------------------------------------
if (dir.exists("/tmp"))
  reg$work.dir = "/tmp"

## ------------------------------------------------------------------------
subsample = function(data, job, ratio, ...) {
  n = nrow(data)
  train = sample(n, floor(n * ratio))
  test = setdiff(seq_len(n), train)
  list(test = test, train = train)
}

## ------------------------------------------------------------------------
data("iris", package = "datasets")
addProblem(name = "iris", data = iris, fun = subsample, seed = 42)

## ------------------------------------------------------------------------
svm.wrapper = function(data, job, instance, ...) {
  library("e1071")
  mod = svm(Species ~ ., data = data[instance$train, ], ...)
  pred = predict(mod, newdata = data[instance$test, ], type = "class")
  table(data$Species[instance$test], pred)
}
addAlgorithm(name = "svm", fun = svm.wrapper)

## ------------------------------------------------------------------------
forest.wrapper = function(data, job, instance, ...) {
  library("ranger")
  mod = ranger(Species ~ ., data = data[instance$train, ], write.forest = TRUE)
  pred = predict(mod, data = data[instance$test, ])
  table(data$Species[instance$test], pred$predictions)
}
addAlgorithm(name = "forest", fun = forest.wrapper)

## ------------------------------------------------------------------------
getProblemIds()
getAlgorithmIds()

## ---- echo=FALSE---------------------------------------------------------
knitr::include_graphics("tikz_prob_algo_simple.png")

## ------------------------------------------------------------------------
# problem design: try two values for the ratio parameter
pdes = list(iris = data.frame(ratio = c(0.67, 0.9)))

# algorithm design: try combinations of kernel and epsilon exhaustively,
# try different number of trees for the forest
ades = list(
  svm = expand.grid(kernel = c("linear", "polynomial", "radial"), epsilon = c(0.01, 0.1)),
  forest = data.frame(ntree = c(100, 500, 1000))
)

addExperiments(pdes, ades, repls = 5)

## ------------------------------------------------------------------------
summarizeExperiments()
summarizeExperiments(by = c("problem", "algorithm", "ratio"))

## ------------------------------------------------------------------------
id1 = head(findExperiments(algo.name = "svm"), 1)
print(id1)
id2 = head(findExperiments(algo.name = "forest", algo.pars = (ntree == 1000)), 1)
print(id2)
testJob(id = id1)
testJob(id = id2)

## ------------------------------------------------------------------------
submitJobs()
waitForJobs()

## ------------------------------------------------------------------------
results = reduceResultsDataTable(fun = function(res) (list(mce = (sum(res) - sum(diag(res))) / sum(res))))
head(results)

## ------------------------------------------------------------------------
tab = ijoin(getJobPars(), results)
head(tab)

## ------------------------------------------------------------------------
tab[ratio == 0.67, list(mmce = mean(mce)), by = c("algorithm", "kernel", "epsilon", "ntree")]

