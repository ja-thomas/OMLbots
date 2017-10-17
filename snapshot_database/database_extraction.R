############################################################################
library(RMySQL)
library(dplyr)
library(tidyr)

mydb = dbConnect(MySQL(), user = "root", dbname='openml', password = "")
dbListTables(mydb)

############################# evaluation_results ###########################

# evaluation table
evaluation =  dbSendQuery(mydb, "select * from evaluation where function_id in (4, 59, 63)") # AUC, scimark, runtime
evaluation = fetch(evaluation, n=-1)
head(evaluation)
table(evaluation$function_id)
# only AUC, scimark and runtime
# id of math_function identifies the measure in evaluation (function_id)
# evaluation = evaluation[evaluation$function_id %in% c(4, 59, 63),] #  4, 59 und 63 werden benötigt
colnames(evaluation)[1] = "rid"
evaluation = evaluation[, c("rid", "function_id", "value")]

# math_function table
math_function =  dbSendQuery(mydb, "select * from math_function")
math_function = fetch(math_function, n=-1)
head(math_function)
math_function$name

# run table
run =  dbSendQuery(mydb, "select * from run")
run = fetch(run, n=-1)
head(run)
table(unique(evaluation$source) %in% unique(run$rid))
# ok!
run = run[, c("rid", "uploader", "setup", "task_id")]

# Join tables to evaluation_results
evaluation_results = inner_join(run, evaluation, by = "rid")
head(evaluation_results)
# only runs of the open_ml bot
evaluation_results = evaluation_results[evaluation_results$uploader == 2702,]
head(evaluation_results)
table(evaluation_results$function_id) 
# Around 2886153 results

############################# meta_features ################################

# data quality table
data_quality =  dbSendQuery(mydb, "select * from data_quality")
data_quality = fetch(data_quality, n=-1)
head(data_quality)
data_quality = data_quality[, c("data", "quality", "value")]
table(data_quality$data)

# input data table
input_data =  dbSendQuery(mydb, "select * from input_data")
input_data = fetch(input_data, n=-1)
head(input_data)
input_data = input_data[, c("run", "data")]
colnames(input_data)[1] = "rid"

# Add the data id to the evaluation results as identifier
evaluation_results = inner_join(evaluation_results, input_data, "rid")
head(evaluation_results)

# Only keep relevant informations of relevant datasets
meta_features_names = c("MajorityClassSize", "MajorityClassPercentage", "NumberOfClasses", 
  "NumberOfInstances", "NumberOfFeatures", "NumberOfNumericFeatures", "NumberOfSymbolicFeatures")
meta_features = data_quality[data_quality$quality %in% meta_features_names, ]
meta_features = meta_features[meta_features$data %in% unique(evaluation_results$data), ]
head(meta_features)

############################# hyperparameters ################################

input_setting = dbSendQuery(mydb, "select * from input_setting")
input_setting = fetch(input_setting, n=-1)
head(input_setting)
input_setting = input_setting[input_setting$setup %in% unique(evaluation_results$setup),]

input =  dbSendQuery(mydb, "select * from input")
input = fetch(input, n=-1)
head(input)
colnames(input)[1] = "input_id"
input = input[, c("input_id", "fullName", "name")]

hyperparameters = inner_join(input_setting, input, by = "input_id")
head(hyperparameters)
hyperparameters = hyperparameters[!(hyperparameters$name %in% c("nthread", "num.threads", "openml.kind", "openml.normal.kind", "openml.seed", "s", "verbose", "xval")), ]
hyperparameters$fullName = gsub("\\(.*","",hyperparameters$fullName)
table(hyperparameters$name)
hyperparameters$input = NULL
hyperparameters$input_id = NULL

############################# Add default values ################################
# rpart
new_hypers_rpart = (hyperparameters[hyperparameters$fullName == "mlr.classif.rpart",])
data_wide = spread(new_hypers_rpart, name, value)
data_wide$maxdepth[is.na(data_wide$maxdepth)] = 30
data_wide$minsplit[is.na(data_wide$minsplit)] = 20
new_hypers_rpart = gather(data_wide, name, value = value, cp, maxdepth, minbucket, minsplit)
hyperparameters = rbind(hyperparameters[hyperparameters$fullName != "mlr.classif.rpart",], new_hypers_rpart)

# kknn
missing = evaluation_results[!(evaluation_results$setup %in% unique(hyperparameters$setup)), ]
head(evaluation_results$setup)
tabelle = table(hyperparameters[hyperparameters$fullName == "mlr.classif.kknn",]$value)

missing = evaluation_results[!(evaluation_results$setup %in% unique(hyperparameters$setup)), ]
missing_uni = sort(unique(missing$setup))
algos = character(length(missing_uni))
for(i in seq_along(missing_uni)) {
  examine = missing[which(missing$setup == missing_uni[i])[1], ]
  runi = getOMLRun(missing$rid[i])
  print(runi$flow.name)
  algos[i] = runi$flow.name
  print(runi$parameter.setting)
}
# all the missing ones are kknn
new_hypers_kknn = data.frame(setup = missing_uni, value = 7, fullName = "mlr.classif.kknn", name = "k")
hyperparameters = rbind(hyperparameters, new_hypers_kknn)

# svm
new_hypers_svm = (hyperparameters[hyperparameters$fullName == "mlr.classif.svm",])
data_wide = spread(new_hypers_svm, name, value)
data_wide$kernel[is.na(data_wide$kernel)] = "radial"
nas = is.na(data_wide[data_wide$kernel == "polynomial",]$degree)
data_wide[data_wide$kernel == "polynomial",]$degree[nas] = 3
new_hypers_svm = gather(data_wide, name, value = value, cost, degree, gamma, kernel)
hyperparameters = rbind(hyperparameters[hyperparameters$fullName != "mlr.classif.svm",], new_hypers_svm)

# ranger
new_hypers_ranger = (hyperparameters[hyperparameters$fullName == "mlr.classif.ranger",])
data_wide = spread(new_hypers_ranger, name, value)
data_wide$num.trees[is.na(data_wide$num.trees)] = 500
data_wide$replace[is.na(data_wide$replace)] = TRUE
data_wide$respect.unordered.factors[is.na(data_wide$respect.unordered.factors)] = FALSE
new_hypers_ranger = gather(data_wide, name, value = value, mtry, num.trees, replace, respect.unordered.factors, sample.fraction)
hyperparameters = rbind(hyperparameters[hyperparameters$fullName != "mlr.classif.ranger",], new_hypers_ranger)

#xgboost
new_hypers_xgboost = (hyperparameters[hyperparameters$fullName == "mlr.classif.xgboost",])
data_wide = spread(new_hypers_xgboost, name, value)
data_wide$nrounds[is.na(data_wide$nrounds)] = 1
data_wide$booster[is.na(data_wide$booster)] = "gbtree"
nas = is.na(data_wide[data_wide$booster == "gbtree",]$max_depth)
data_wide[data_wide$booster == "gbtree",]$max_depth[nas] = 6
new_hypers_xgboost = gather(data_wide, name, value = value, alpha, booster, colsample_bylevel, colsample_bytree, eta, lambda, 
  max_depth, min_child_weight, nrounds, subsample)
hyperparameters = rbind(hyperparameters[hyperparameters$fullName != "mlr.classif.xgboost",], new_hypers_xgboost)

# Reference table
head(hyperparameters)
new_hypers_ranger = hyperparameters[hyperparameters$fullName == "mlr.classif.ranger",]
data_wide = spread(new_hypers_ranger, name, value)
head(data_wide)
data_wide = data_wide[data_wide$num.trees == 2000 & is.na(data_wide$sample.fraction),]
reference = evaluation_results[evaluation_results$setup %in% data_wide$setup, ]
evaluation_results = evaluation_results[!(evaluation_results$setup %in% data_wide$setup), ]
hyperparameters = hyperparameters[!(hyperparameters$setup %in% data_wide$setup), ]

# runtime
runtime = evaluation_results[evaluation_results$function_id == 63, ]
evaluation_results = evaluation_results[evaluation_results$function_id != 63, ]

# scimark benchmark
scimark =  evaluation_results[evaluation_results$function_id == 59, ]
evaluation_results =  evaluation_results[evaluation_results$function_id != 59, ]

delete_runtime = runtime$rid[!(runtime$rid %in% scimark$rid)]
runtime = runtime[runtime$rid != 2414382, ]
all(runtime$rid == scimark$rid)
runtime = data.frame(runtime, scimark = scimark$value)
rm(scimark)
delete_columns = c("uploader", "setup", "task_id", "function_id", "data")
runtime[, delete_columns] = NULL
colnames(runtime)[2] = "runtime"
delete_columns = c("uploader", "function_id")
evaluation_results[, delete_columns] = NULL
reference[, delete_columns] = NULL

# Fit data to our current functions
head(evaluation_results)
head(runtime)
head(meta_features)
head(hyperparameters)
head(reference)

# rename certain columns
colnames(evaluation_results)[1] = "run_id"
colnames(reference)[1] = "run_id"
colnames(runtime)[1] = "run_id"

colnames(evaluation_results)[5] = "data_id"
colnames(reference)[5] = "data_id"
colnames(meta_features)[1] = "data_id"

colnames(evaluation_results)[4] = "area.under.roc.curve"
colnames(reference)[4] = "area.under.roc.curve"

# rename the tables
tbl.results = evaluation_results
tbl.metaFeatures = meta_features
tbl.hypPars = hyperparameters
tbl.runTime = runtime
tbl.resultsReference = reference

# save tables in sql database
overwrite = TRUE
db = paste0("./mlrRandomBotDatabaseSnapshot.db")
if (file.exists(db) & overwrite) {
  unlink(db, force = TRUE)
} 
src = src_sqlite(db, create = !file.exists(db))

copy_to(src, tbl.results, temporary = FALSE)
copy_to(src, tbl.runTime, temporary = FALSE)
copy_to(src, tbl.metaFeatures, temporary = FALSE)
copy_to(src, tbl.hypPars, temporary = FALSE)
copy_to(src, tbl.resultsReference, temporary = FALSE)

save(tbl.results, tbl.runTime, tbl.metaFeatures, tbl.hypPars, 
  tbl.resultsReference, file = "./snapshot_database/mlrRandomBotResults.RData")


# Save it in other formats
load("./snapshot_database/mlrRandomBotResults.RData")

algos = unique(tbl.hypPars$fullName)
algos = algos[c(2, 3, 1, 4, 5, 6)]
meta_features = unique(tbl.metaFeatures$quality)
tbl.metaFeatures.wide = spread(tbl.metaFeatures, quality, value)


results = list()
for(i in seq_along(algos)) {
  print(i)
  results_i = tbl.hypPars[tbl.hypPars$fullName == algos[i],]
  hyp_pars = unique(results_i$name)
  results_i = spread(results_i, name, value)
  results_i = merge(results_i, tbl.results, by = "setup")
  results_i = merge(results_i, tbl.metaFeatures.wide, by = "data_id")
  results_i = merge(results_i, tbl.runTime, by = "run_id")
  results_i = results_i[, c("task_id", hyp_pars, "area.under.roc.curve", "runtime", "scimark", meta_features)]
  results[[i]] = results_i
}
names(results) = algos
save(results, file = "./snapshot_database/mlrRandomBotResultsTables.RData")

library(readr)
for(i in seq_along(algos)) {
  print(i)
  write_csv(results[[i]], path = paste0("./snapshot_database/mlrRandomBotResults_", algos[[i]], ".csv"))
}

