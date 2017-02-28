library(batchtools)
source("exponentialBackOff.R")

unlink("registry/", recursive = TRUE)

reg = makeRegistry()

f = function(i, j) {
    Sys.sleep(i * 5)
    x = rnorm(j * 1000000)
}

x = expand.grid(i = 1:10, j = 1:10)

batchMap(i = x$i, j = x$j, f)

