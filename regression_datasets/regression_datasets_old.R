######################################################### Regression #########################################################

options(java.parameters = "- Xmx4096m") # Should avoid java gc overhead
library(OpenML)

saveOMLConfig(apikey = "6df93acdb13899f48fd6bd689b0dd7cf", arff.reader = "RWeka", overwrite=TRUE)
datasets = listOMLDataSets() 
length(datasets$name)
reg = listOMLTasks(task.type = "Supervised Regression", estimation.procedure = "10-fold Crossvalidation", limit = 50000, number.of.missing.values = 0)

dim(reg) # 374 Regr.-Tasks

# Every dataset only once
reg = reg[order(reg$data.id),]
logic = logical(nrow(reg))
logic = rep(TRUE, nrow(reg))
for(i in 2:nrow(reg))
  if(reg$data.id[i] == reg$data.id[i-1]) logic[i] = FALSE
reg = reg[logic,]

# nans = character(nrow(reg))
# # Only datasets with integer/numeric target
# for(j in 1:length(reg$task.id)){
#   print(j)
#   task = try(getOMLTask(task.id = reg$task.id[j], verbosity=0))
#   nans[j] = try(class(task$input$data.set$data[, task$input$data.set$target.features]))
#   save(nans, file = "/home/probst/Paper/Exploration_of_Hyperparameters/OMLbots/regression_datasets/nans_reg.RData")
#   gc()
# }
# 
# load("nans_reg.RData")
# reg = reg[which(nans == "numeric" | nans == "integer"), ]

# delete doubles
doppelt = names(sort(table(reg$name)[table(reg$name) > 1]))
doppelt = reg[reg$name %in% doppelt, ]
doppelt = doppelt[order(doppelt$name), ]

raus = c(4892, 4883, 4876, 5025, 12715, 4874)

reg = reg[!(reg$task.id %in% raus),]

reg = reg[order(reg$name), ]

# Delete Friedman-, volcanoes- and trX-datasets
reg = reg[substr(reg$name,1,3) != "fri" & substr(reg$name,1,4) != "a3a" & substr(reg$name,1,3) != "a4a" & 
    substr(reg$name,1,3) != "a5a" & substr(reg$name,1,3) != "a6a" & substr(reg$name,1,3) != "a7a" & 
    substr(reg$name,1,3) != "a8a" & substr(reg$name,1,3) != "a9a" & substr(reg$name,1,9) != "autoPrice" & 
    substr(reg$name,1,9) != "chscase_c" & substr(reg$name,1,4) != "QSAR" , ]

# Delete artificial drug-datasets (with 1143 numeric features)
reg = reg[reg$number.of.numeric.features != 1143, ]

nrow(reg) #  223 datasets
sum(reg$number.of.features) # 296227 Features
hist(reg$number.of.features)
sum(reg[reg$number.of.features < 100,]$number.of.features) # 2546

# Order them by size (n*p)
reg = reg[order(as.numeric(reg$number.of.features) * as.numeric(reg$number.of.instances)), ]

# Only 1 Feature = Target is senseless
reg = reg[which(reg$name != "lmpavw"), ]

# Is this useful???
# Delete datasets with more than 50 categories in one variable
# fail to download some of the biggest (~20) datasets
more2 = !logical(nrow(reg))
for(j in 1:nrow(reg)){
  print(j)
  task = getOMLTask(task.id = reg$task.id[j], verbosity=0)
  classen = sapply(task$input$data.set$data, class)
  indiz = which(classen == "character" | classen == "factor")
  if(!any(apply(as.data.frame(task$input$data.set$data[, indiz]), 2, function(x) length(unique(x))) > 50))
    more2[j] = FALSE
}
reg = reg[more2 == FALSE ,]

reg[order(reg$number.of.instances), c("number.of.instances", "number.of.features", "task.id")]

# Delete equal datasets
raus = c(12723:12729, 4993, 5009, 5013, 5023, 2280, 2313, 12714)
reg = reg[!(reg$task.id %in% raus),]

# Split into small and big datasets
reg_small = reg[which(reg$number.of.instances < 1000 & reg$number.of.features < 1000),]
reg_big = reg[which(!(reg$number.of.instances < 1000 & reg$number.of.features < 1000)),]

save(reg, file = "./regression_datasets/regression_datasets.RData")
load("./regression_datasets/regression_datasets.RData")

# Manual selection
for(i in 1:nrow(reg)) {
  print(paste(i, "#######################################################################################################"))
  print(paste("#######################################################################################################"))
  task = getOMLTask(task.id = reg$task.id[i], verbosity=0)
  print(head(task$input$data.set$data))
  print(head(task$input$data.set$desc$description))
  print(task$input$data.set$target.features)
  print(unique(task$input$data.set$data[,task$input$data.set$target.features]))
  print(dim(task$input$data.set$data))
}

# Criteria: Only Regression (classification or survival); repeated datasets (e.g. ozone); 
# unclear variable names and/or description

ok = c(1, 3, 4, 7, 8, 9, 11, 12, 14, 16, 18, 19, 22, 25, 27, 28, 29, 33, 34, 35, 38, 39, 40,
  41, 42, 43, 46, 47, 48, 50, 51, 52, 53, 54, 56, 58, 59, 60, 62, 63, 64, 65, 67, 68, 71, 
  72, 76, 77, 78, 79, 81, 82, 84, 85, 86, 87, 89, 91, 92, 97, 98, 100, 101, 103, 104, 105, 106, 107, 
  108, 109, 111, 112, 113, 114, 117, 118, 119, 121, 122, 126, 127, 128, 130, 132, 133, 134, 135, 136, 
  138, 139, 141, 142, 145, 146, 147, 148, 149, 150, 151, 154, 156, 159)

reg = reg[ok, ]

save(reg, file = "./regression_datasets/regression_datasets_manual.RData")
load("./regression_datasets/regression_datasets_manual.RData")

# Estimation of time for training a RF on each dataset
time = numeric(nrow(reg))
for(i in 1:nrow(reg)) {
  print(i)
  task = getOMLTask(task.id = reg$task.id[i], verbosity=0)

  task = convertOMLTaskToMlr(task)$mlr.task
  lrn = makeLearner("regr.ranger", num.threads = 6, num.trees = 60)
  time[i] = system.time(train(lrn, task))[3]
}
# Longest one takes 4 seconds

reg = reg[reg$number.of.instances >= 500, ]
