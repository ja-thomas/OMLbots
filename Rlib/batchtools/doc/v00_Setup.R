## ----include = FALSE-----------------------------------------------------
library(batchtools)

## ------------------------------------------------------------------------
reg = makeRegistry(NA)
reg$cluster.functions = makeClusterFunctionsSocket(2)

## ----eval=FALSE----------------------------------------------------------
#  Sys.setenv(DEBUGME = "batchtools")
#  library(batchtools)

## ----eval = FALSE--------------------------------------------------------
#  cluster.functions = makeClusterFunctionsInteractive()

## ---- eval = FALSE-------------------------------------------------------
#  work.dir = "~"
#  packages = union(packages, "checkmate")

