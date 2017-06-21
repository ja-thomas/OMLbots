## ---- message = FALSE----------------------------------------------------
library(batchtools)
reg = makeRegistry(file.dir = NA, seed = 1)

## ----include=FALSE-------------------------------------------------------
if (dir.exists("/tmp"))
  reg$work.dir = "/tmp"

## ------------------------------------------------------------------------
flakeyFunction <- function(value) {
  if (value %in% c(2, 9)) stop("Ooops.")
  value^2
}
batchMap(flakeyFunction, 1:10)

## ------------------------------------------------------------------------
testJob(id = 1)

## ------------------------------------------------------------------------
as.character(try(testJob(id = 2)))

## ------------------------------------------------------------------------
submitJobs()
waitForJobs()
getStatus()

## ------------------------------------------------------------------------
findErrors()
getErrorMessages()

## ------------------------------------------------------------------------
writeLines(getLog(id = 9))

## ------------------------------------------------------------------------
ids = grepLogs(pattern = "ooops", ignore.case = TRUE)
print(ids)

