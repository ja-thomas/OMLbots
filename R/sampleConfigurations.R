# Samples size random Configuration for a given parameter set
# @param size number of configurations to generate
# @param par.set parameter set
# @return data.frame where each row is one valid configuration
sampleRandomConfiguration = function(size, par.set) {
  des = generateRandomDesign(size, par.set, trafo = TRUE)
  des = BBmisc::convertDataFrameCols(des, factors.as.char = TRUE)
  return(des)
}


sampleDefaultConfiguration = function(size, par.set) {
  if (size > 1)
    warning("For the default Configuration only one configuration is generated")
  des = generateDesignOfDefaults(par.set, trafo = TRUE)
  des = BBmisc::convertDataFrameCols(des, factors.as.char = TRUE)
  return(des)
}