library(batchtools)
source("exponentialBackOff.R")

unlink("registry/", recursive = TRUE)

reg = makeRegistry()
reg$default.resources$memory = 100L
reg$default.resources$walltime = 1L


f = function(i, j) {
    
    g = 0
    for (k in 1:(10000000*i)) {
      g = g + 1
    }
    
    x = rnorm(j * 1000000)
    return(TRUE)
}

x = expand.grid(i = 1:10, j = 1:10)

batchMap(i = x$i, j = x$j, f)
mr = list(memory = 512, walltime = 300)
