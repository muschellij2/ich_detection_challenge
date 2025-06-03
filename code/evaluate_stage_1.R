library(ROCR)
library(dplyr)
library(readr)
library(tidyr)
library(cutpointr)
setwd(here::here())

loss = function(label, prediction, tol = 1e-15) {
  label[ label < tol] = tol
  label[ label > (1 - tol)] = 1 - tol
  
  prediction[ prediction < tol] = tol
  prediction[ prediction > (1 - tol)] = 1 - tol
  
  res = (label * log(prediction) + (1 - label) * log(1 - prediction))
  -mean(res)
}

check = FALSE
group = "test"
outcomes = c("any", "epidural", "intraparenchymal", "intraventricular", 
             "subarachnoid", "subdural")
num_trees = 2000
if (check) {
  truth2 = read_csv("stage_2_train.csv.gz")
  truth = read_csv("stage_1_train.csv.gz")
  stopifnot(all(truth$ID %in% truth2$ID))
}
if (group == "test") {
  stage_1 = read_csv("stage_1_sample_submission.csv.gz")
}
if (group == "train") {
  stage_1 = read_rds(paste0("stage_1_", group, ".rds")) %>% 
    gather(key = outcome, value = Label, -ID) %>% 
    unite(ID, ID, outcome, sep = "_")
}

truth = read_csv("stage_2_train.csv.gz")


stopifnot(all(stage_1$ID %in% truth$ID))

pred = lapply(outcomes, function(x) {
  xx = paste0("predictions/rf_", group, "_", x, "_", num_trees, ".rds")
  read_rds(xx)
})
pred = bind_rows(pred) %>% 
  unite(ID, ID, outcome, sep = "_")

truth = truth[ truth$ID %in% stage_1$ID,]

truth = truth %>% 
  rename(y = Label)
stopifnot(!any(is.na(truth$y)))

truth = left_join(pred, truth)

truth = truth %>% 
  separate(ID, into = c("ID", "uid", "outcome"), 
           sep = "_", remove = TRUE) %>% 
  unite(col = "ID", ID, uid, sep = "_") 

ss = split(truth, truth$outcome)

cp <- lapply(ss, function(x) {
  xx = cutpointr(x, Label, y, 
            method = maximize_metric, metric = accuracy)
  xx
})
cp = bind_rows(cp, .id = "outcome")
loss_value = sapply(cp$data, function(x) {
  loss(x$y, x$Label)
})

threshes = seq(0, 1, by = 0.005)
losses = pbapply::pbsapply(cp$data, function(x) {
  sapply(threshes, function(r) {
    loss(x$y, x$Label >= r)
  })
})
colnames(losses) = cp$outcome

cp$data = NULL
cutoffs = threshes[apply(losses, 2, which.min)]
names(cutoffs) = colnames(losses)

cp$cutoff = cutoffs
results = list(cutpoints = cp,
               thresholds = threshes,
               losses = loss, 
               loss_value = loss_value,
               cutoffs = cutoffs)
readr::write_rds(cp, paste0("results/cutpoints_", group, ".rds"))





wide = truth %>% 
  arrange(ID) %>% 
  select(-Label) %>% 
  spread(outcome, value = y)

wide_lab = truth %>% 
  arrange(ID) %>% 
  select(-y) %>% 
  spread(outcome, value = Label)

x = ss$intraparenchymal

mat = wide %>% 
  select(-ID) %>% 
  as.matrix
lab_mat = wide_lab %>% 
  select(-ID) %>% 
  as.matrix
pred_obj = prediction(predictions = lab_mat, mat)
perf = performance(pred_obj, "tpr", "fpr")
auc = performance(pred_obj, "auc")
names(auc@y.values) = colnames(mat)
unlist(auc@y.values)


tpred_obj = prediction(predictions = (lab_mat > 0.5)*1, mat)
tperf = performance(tpred_obj, "tpr", "fpr")
plot(perf)
points(
  sapply(tperf@x.values, function(x) x[2]), 
  sapply(tperf@y.values, function(x) x[2]), col = "blue")

.
pngname = paste0("results/roc_", group, ".png")


# 
# pred_obj = prediction(predictions = x$Label, x$y)
# perf = performance(pred_obj, "tpr", "fpr")
# 
# mat = sapply(ss,  function(x) {
#   x$Label
# })


