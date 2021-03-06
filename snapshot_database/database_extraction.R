############################################################################
library(RMySQL)
library(dplyr)
library(tidyr)
library(OpenML)
library(data.table)

mydb = dbConnect(MySQL(), user = "root", dbname='openml_new', password = "") # pw as usual
dbListTables(mydb)

############################# evaluation_results ###########################
# run.uploader only our bot
# 4 AUC, 45 accuracy, 54 root mean squared error, 59 scimark_benchmark, 63 usercpu_time_millis
sql.exp = "SELECT eval.source AS rid, eval.function_id, eval.value, run.task_id, run.setup, i.data 
  FROM evaluation As eval 
    INNER JOIN run ON eval.source = run.rid
    INNER JOIN input_data AS i ON eval.source = i.run
  WHERE eval.function_id in (4, 45, 54, 59, 63) AND run.uploader = 2702"
evaluation_results = dbGetQuery(mydb, sql.exp) # AUC, acc, rmse, scimark, runtime

# From long to wide
evaluation_results = spread(evaluation_results, function_id, value)
setnames(evaluation_results, c("4", "45", "54", "59", "63"), c("auc", "accuracy", "rmse", "scimark", "runtime"))
evaluation_results$brier = (evaluation_results$rmse)^2

############################# meta_features ################################
# data quality table
sql.exp = "SELECT d.data, d.quality, d.value
  FROM data_quality AS d
  WHERE d.quality in ('MajorityClassSize', 'MajorityClassPercentage', 'NumberOfClasses', 
  'NumberOfInstances', 'NumberOfFeatures', 'NumberOfNumericFeatures', 'NumberOfSymbolicFeatures')"
data_quality =  dbGetQuery(mydb, sql.exp)

############################# hyperparameters ################################
sql.exp = "SELECT DISTINCT iset.setup, iset.value, i.fullName, i.name
  FROM input_setting As iset
    INNER JOIN input As i ON iset.input_id = i.id
    INNER JOIN run ON run.setup = iset.setup
  WHERE run.uploader = 2702 AND i.fullName LIKE 'mlr.classif.xgboost%' 
  LIMIT 1000000"
hyperparameters = dbGetQuery(mydb, sql.exp)

hyperparameters = hyperparameters[!(hyperparameters$name %in% c("nthread", "num.threads", "openml.kind", "openml.normal.kind", "openml.seed", "s", "verbose", "xval")), ]
hyperparameters$fullName = gsub("\\(.*","",hyperparameters$fullName)


############################# Add default values ################################
# rpart
new_hypers_rpart = (hyperparameters[hyperparameters$fullName == "mlr.classif.rpart",])
data_wide = spread(new_hypers_rpart, name, value)
data_wide$maxdepth[is.na(data_wide$maxdepth)] = 30
data_wide$minsplit[is.na(data_wide$minsplit)] = 20
new_hypers_rpart = gather(data_wide, name, value = value, cp, maxdepth, minbucket, minsplit)
hyperparameters = rbind(hyperparameters[hyperparameters$fullName != "mlr.classif.rpart",], new_hypers_rpart)

# kknn
hyp_kknn = hyperparameters[hyperparameters$fullName == "mlr.classif.kknn",]
sum(run$setup %in% hyp_kknn$setup)
sum(evaluation_results$setup %in% hyp_kknn$setup)

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
data_wide$min.node.size[is.na(data_wide$min.node.size)] = 1
new_hypers_ranger = gather(data_wide, name, value = value, mtry, num.trees, replace, respect.unordered.factors, sample.fraction, min.node.size)
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
new_hypers_xgboost$gamma = NULL
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

gc()

delete_columns = c("uploader", "function_id")
evaluation_results[, delete_columns] = NULL
reference[, delete_columns] = NULL

# rid should be the same for all tables
all(evaluation_results$rid %in% runtime$rid)
all(runtime$rid %in% evaluation_results$rid)
runtime = runtime[runtime$rid %in% evaluation_results$rid, ]
scimark = scimark[scimark$rid %in% evaluation_results$rid, ]


# Fit data to our current functions
head(evaluation_results)
head(runtime)
head(scimark)
head(meta_features)
head(hyperparameters)
head(reference)

# rename certain columns
colnames(evaluation_results)[1] = "run_id"
colnames(reference)[1] = "run_id"
colnames(runtime)[1] = "run_id"
colnames(scimark)[1] = "run_id"

colnames(evaluation_results)[7] = "data_id"
colnames(reference)[7] = "data_id"
colnames(meta_features)[1] = "data_id"

# rename the tables
assign("tbl.results", evaluation_results)
assign("tbl.runTime", runtime)
assign("tbl.scimark", scimark)
assign("tbl.metaFeatures", meta_features)
assign("tbl.hypPars", hyperparameters)
assign("tbl.resultsReference", reference)

# save tables in sql database
overwrite = TRUE
db = paste0("./mlrRandomBotDatabaseSnapshot.db")
if (file.exists(db) & overwrite) {
  unlink(db, force = TRUE)
} 
src = src_sqlite(db, create = !file.exists(db))

copy_to(src, tbl.results, temporary = FALSE)
copy_to(src, tbl.runTime, temporary = FALSE)
copy_to(src, tbl.scimark, temporary = FALSE)
copy_to(src, tbl.metaFeatures, temporary = FALSE)
copy_to(src, tbl.hypPars, temporary = FALSE)
copy_to(src, tbl.resultsReference, temporary = FALSE)

save(tbl.results, tbl.runTime, tbl.scimark, tbl.metaFeatures, tbl.hypPars, 
  tbl.resultsReference, file = "./snapshot_database/OpenMLRandomBotResults.RData")


# Save it in other formats
load("./snapshot_database/OpenMLRandomBotResults.RData")

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
  results_i = merge(results_i, tbl.scimark, by = "run_id")
  results_i = results_i[, c("task_id", hyp_pars, "auc", "accuracy", "brier", "runtime", "scimark", meta_features)]
  results[[i]] = results_i
}
names(results) = algos
save(results, file = "./snapshot_database/OpenMLRandomBotResultsTables.RData")

library(readr)
for(i in seq_along(algos)) {
  print(i)
  write_csv(results[[i]], path = paste0("./snapshot_database/OpenML_RBot_", algos[[i]], ".csv"))
}

# Subset of tables: Maximum of 500000 results per learner
library(dplyr)
library(tidyr)
library(OpenML)

calculateDataIds = function(tbl.results, tbl.hypPars, min.experiments = 200) {
  whole.table = inner_join(tbl.results, tbl.hypPars, by = "setup") %>% select(., data_id, fullName)
  cross.table = table(whole.table$data_id, whole.table$fullName)
  bigger = rowSums(cross.table > min.experiments)
  data.ids = names(bigger)[bigger == 6] 
  return(data.ids)
}

# Exclude dataset which does not provide results for many learners
data.ids = calculateDataIds(tbl.results, tbl.hypPars, min.experiments = 200)
# Only results for OpenML100 datasets
tasks = listOMLTasks(number.of.classes = 2L, tag = "OpenML100", estimation.procedure = "10-fold Crossvalidation", number.of.missing.values = 0)
data.ids = data.ids[data.ids %in% tasks$data.id]

run.ids = numeric()
algos = unique(tbl.hypPars$fullName)
algos = algos[c(2, 3, 1, 4, 5, 6)]
set.seed(123)
for(i in seq_along(algos)) {
  print(i)
  results_i = tbl.hypPars[tbl.hypPars$fullName == algos[i],]
  hyp_pars = unique(results_i$name)
  results_i = spread(results_i, name, value)
  results_i = merge(results_i, tbl.results, by = "setup", all.x = T, all.y = F)
  results_i = results_i[results_i$data_id %in% data.ids,]
  if(i == 5) {
    results_i = results_i[results_i$min.node.size != 1, ]
  }
  
  kumi = sort(table(results_i$data_id))
  
  kumi_val = numeric(length(kumi))
  kumi_val[1] = kumi[1] * length(kumi)
  for(j in 2:length(kumi)) {
    kumi_val[j] = cumsum(kumi)[j-1] + kumi[j]*(length(kumi)-j + 1)
  }
  maximo = max(kumi[kumi_val < 500000])
  good_ids = names(kumi[kumi <= maximo])
  bad_ids = names(kumi[kumi > maximo])
  
  rest = 500000 - max(kumi_val[kumi_val < 500000])
  
  if (i != 3){
    extra_nr = floor(rest/length(bad_ids))
    # fill up to 500000
    rest2 = rest - extra_nr * length(bad_ids)
    extra_ids = sample(c(rep(1, rest2), rep(0, length(bad_ids)- rest2)))
  } 
  run.ids = c(run.ids, results_i$run_id[results_i$data_id %in% good_ids])
  for(j in seq_along(bad_ids)) {
    print(paste(i,j))
    setup_bad = sample(results_i$run_id[results_i$data_id %in% bad_ids[j]], maximo + extra_nr + extra_ids[j], replace = F)
    run.ids = c(run.ids, setup_bad)
  }
  print(length(run.ids))
}
tbl.results = tbl.results[tbl.results$run_id %in% run.ids,]
tbl.hypPars = tbl.hypPars[tbl.hypPars$setup %in% unique(tbl.results$setup),]
tbl.runTime = tbl.runTime[tbl.runTime$run_id %in% run.ids,]
tbl.scimark = tbl.scimark[tbl.scimark$run_id %in% run.ids,]
tbl.metaFeatures = tbl.metaFeatures[tbl.metaFeatures$data_id %in% data.ids,]
tbl.resultsReference = tbl.resultsReference[tbl.resultsReference$data_id %in% data.ids,]

# exclude senseless data of kknn
hypPars.kknn = tbl.hypPars[tbl.hypPars$fullName == "mlr.classif.kknn",]
results.kknn = tbl.results[tbl.results$setup %in% hypPars.kknn$setup,]
resi.kknn = merge(results.kknn, hypPars.kknn, by = "setup")
dups = resi.kknn[duplicated(resi.kknn[, c("data_id", "value")]),]
no_dups = resi.kknn[!duplicated(resi.kknn[, c("data_id", "value")]),]

tbl.results = tbl.results[!(tbl.results$run_id %in% dups$run_id), ]
tbl.hypPars = tbl.hypPars[(tbl.hypPars$setup %in% tbl.results$setup), ]
tbl.runTime = tbl.runTime[!(tbl.runTime$run_id %in% dups$run_id), ]
tbl.scimark = tbl.scimark[!(tbl.scimark$run_id %in% dups$run_id), ]

# delete duplicates of reference results
head(tbl.resultsReference)
tbl.resultsReference = tbl.resultsReference[!duplicated(tbl.resultsReference[, c("data_id")]),]

save(tbl.results, tbl.runTime, tbl.scimark, tbl.metaFeatures, tbl.hypPars, 
  tbl.resultsReference, file = "./snapshot_database/OpenMLRandomBotResultsFinal.RData")

# Save it in other formats
load("./snapshot_database/OpenMLRandomBotResultsFinal.RData")

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
  results_i = merge(results_i, tbl.scimark, by = "run_id")
  results_i = results_i[, c("data_id", hyp_pars, "auc", "accuracy", "brier", "runtime", "scimark", meta_features)]
  results[[i]] = results_i
}
names(results) = algos
save(results, file = "./snapshot_database/OpenMLRandomBotResultsFinalTables.RData")

library(readr)
for(i in seq_along(algos)) {
  print(i)
  write_csv(results[[i]], path = paste0("./snapshot_database/OpenMLRandomBotResultsFinal_", algos[[i]], ".csv"))
}

load("./snapshot_database/OpenMLRandomBotResultsFinalTables.RData")

# Number of results per dataset and algorithm
levs = levels(as.factor(results[[2]]$data_id))
nr_results = data.frame(t(as.numeric(table(factor(results[[1]]$data_id, levels = levs)))))
for(i in 2:6)
  nr_results = rbind(nr_results, table(factor(results[[i]]$data_id, levels = levs)))
colnames(nr_results) = levs

save(nr_results, file = "./snapshot_database/nr_results.RData")

