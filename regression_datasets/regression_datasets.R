######################################################### Regression #########################################################

options(java.parameters = "- Xmx1024m") # Should avoid java gc overhead
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
doppelt[, c(1,3,5,7,9,10,11,14,15)]

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


# Delete datasets with more than 50 categories in one variable
more2 = logical(nrow(reg))
for(j in 1:nrow(reg)){
  print(j)
  task = try(getOMLTask(task.id = reg$task.id[j], verbosity=0))
  classen = sapply(task$input$data.set$data, class)
  indiz = which(classen == "character" | classen == "factor")
  if(any(apply(as.data.frame(task$input$data.set$data[, indiz]), 2, function(x) length(unique(x))) > 50))
    more2[j] = TRUE
}
reg = reg[more2 == FALSE ,]

# Delete equal datasets
raus = c(4993, 5013, 5023, 2280, 2313)
reg = reg[!(reg$task.id %in% raus),]

# Split into small and big datasets
reg_small = reg[which(reg$number.of.instances < 1000 & reg$number.of.features < 1000),]
reg_big = reg[which(!(reg$number.of.instances < 1000 & reg$number.of.features < 1000)),]

save(reg, reg_small, reg_big, file = "/home/probst/Paper/Exploration_of_Hyperparameters/OMLbots/regression_datasets/regression_datasets.RData")

