## ---- message = FALSE----------------------------------------------------
library(batchtools)
reg = makeRegistry(file.dir = NA, seed = 1)

## ----include=FALSE-------------------------------------------------------
if (dir.exists("/tmp"))
  reg$work.dir = "/tmp"

## ------------------------------------------------------------------------
piApprox = function(n) {
  nums = matrix(runif(2 * n), ncol = 2)
  d = sqrt(nums[, 1]^2 + nums[, 2]^2)
  4 * mean(d <= 1)
}
piApprox(1000)

## ------------------------------------------------------------------------
batchMap(fun = piApprox, n = rep(1e5, 10))

## ------------------------------------------------------------------------
names(getJobTable())

## ------------------------------------------------------------------------
submitJobs(resources = list(walltime = 3600, memory = 1024))

## ------------------------------------------------------------------------
getStatus()

## ------------------------------------------------------------------------
waitForJobs()
mean(sapply(1:10, loadResult))
reduceResults(function(x, y) x + y) / 10

## ------------------------------------------------------------------------
res = btlapply(rep(1e5, 10), piApprox)
mean(unlist(res))

