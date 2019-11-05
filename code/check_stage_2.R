library(dplyr)
library(readr)
setwd(here::here())

test_1 = readr::read_rds(paste0("stage_", 1, "_test.rds"))
train_1 = readr::read_rds(paste0("stage_", 1, "_train.rds"))
train_2 = readr::read_rds(paste0("stage_", 2, "_train.rds"))
test_2 = readr::read_rds(paste0("stage_", 2, "_test.rds"))

# all old test are now in the train 2
stopifnot(all(test_1$ID %in% train_2$ID))

# no overlap between new 
stopifnot(!any(test_2$ID %in% train_1$ID))
stopifnot(!any(test_2$ID %in% test_1$ID))

# the new training data is not "new" - but we have outcomes now
old_ids = sort(c(test_1$ID, train_1$ID))
new_train = sort(train_2$ID)
stopifnot(all( old_ids %in% train_2$ID))

stopifnot(length(old_ids) == length(new_train))
stopifnot(old_ids == new_train)

stopifnot(length(intersect(test_2$ID, old_ids)) == 0)
stopifnot(length(intersect(test_2$ID, train_2$ID)) == 0)
