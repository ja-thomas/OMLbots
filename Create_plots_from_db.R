library(dplyr)
library(ggplot2)
library(tidyr)

db = "C:/Users/Family/Dropbox/OMLbots/mlrRandomBotDatabase.db"
src = src_sqlite(db, create = !file.exists(db))
setwd("D:/03_GitHub/04_ML_Bot/OMLbots")


pdf("BotOverview.pdf", width = 10, height = 8)
### Dataset and learner exploration
sql.qry = "SELECT DISTINCT t.[run.id], t.[flow.name], t.[data.name] FROM [run.table] t"
db.data = src %>% tbl(sql(sql.qry)) %>% collect(n = Inf)

p = ggplot(db.data) + theme_light()
p + geom_bar(aes(flow.name)) + coord_flip() + ggtitle("Count of learners")
p + geom_bar(aes(data.name)) + coord_flip() + ggtitle("Count of datasets")

### Hyperparam and measure exploration
sql.qry = "SELECT DISTINCT t.[run.id], t.[flow.name], t.[data.name], t.[measure.name], t.[measure.value], h.[hyperpar.name], h.[hyperpar.value] FROM [run.table] t JOIN [hyperpar.table] h ON t.[run.id] = h.[run.id] WHERE t.[measure.name] = 'area.under.roc.curve'"
db.data = src %>% tbl(sql(sql.qry)) %>% collect(n = Inf)

unique.measure = unique(db.data[,c("run.id", "flow.name", "data.name", "measure.value")])
p = ggplot(unique.measure) + theme_light() + theme(legend.position = "bottom", legend.text = element_text(size = 8))
p + geom_boxplot(aes(data.name, measure.value)) + coord_flip() + ggtitle("AUC by datasets") 
p + geom_boxplot(aes(flow.name, measure.value)) + coord_flip() + ggtitle("AUC by flow")

unique.ranger = subset(db.data, flow.name %in% c("mlr.classif.ranger(9)", "mlr.classif.ranger(10)", "mlr.classif.ranger(8)")) %>% spread(., hyperpar.name, hyperpar.value)
unique.ranger$num.trees = as.numeric(unique.ranger$num.trees)
unique.ranger$sample.fraction = as.numeric(unique.ranger$sample.fraction)
unique.ranger$mtry = as.numeric(unique.ranger$mtry)
p = ggplot(unique.ranger) + theme_light() + theme(legend.position = "bottom", legend.text = element_text(size = 8))
p + geom_point(aes(num.trees, measure.value, color = data.name)) + ggtitle("AUC by num.trees")
p + geom_point(aes(sample.fraction, measure.value, color = data.name))+ ggtitle("AUC by sample.fraction")
p + geom_point(aes(mtry, measure.value, color = data.name)) + scale_x_log10() + ggtitle("AUC by mtry")

### runtime exploration
sql.qry = "SELECT DISTINCT r.[run.id], r.[sci.mark], r.[run.time], t.[data.name], t.[flow.name]
  FROM [runtime.table] r JOIN [run.table] t ON t.[run.id] = r.[run.id]"
db.data = src %>% tbl(sql(sql.qry)) %>% collect(n = Inf)
p = ggplot(db.data) + theme_light() + theme(legend.position = "bottom", legend.text = element_text(size = 8))
p + geom_boxplot(aes(data.name, run.time)) + scale_y_log10() + coord_flip() + ggtitle("runtime by datasets")
p + geom_boxplot(aes(flow.name, run.time)) + scale_y_log10() + coord_flip() + ggtitle("runtime by flow")
p + geom_point(aes(sci.mark, run.time, color = data.name)) + ggtitle("runtime by sci.mark and data.name") + theme(legend.position = "bottom", legend.text = element_text(size = 8))
p + geom_point(aes(sci.mark, run.time, color = flow.name)) + ggtitle("runtime by sci.mark and flow.name")

dev.off()
