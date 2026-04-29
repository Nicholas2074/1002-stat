# //SECTION - boruta

library(mlr3verse)

library(tidyverse)

set.seed(0)

# //ANCHOR - preprocess

dfDis <- merge(varsImp, gcs[, c(1, 3)], by = "icuid", all = FALSE)

# delete icuid
dfDisDel <- dfDis[, -1]

# regroup
# dfDisDel$group <- ifelse(dfDisDel$group == 5, 1, 0)
dfDisDel$group <- as.factor(dfDisDel$group)

# task definition
taskDis <- as_task_classif(dfDisDel, target = "disgcs", positive = "1")

# pipline building
po1Dis <-
    po(
        "removeconstants" # rm constant vars
    ) %>>%
    # po("encode",
    #     method = "one-hot", # one-hot encoding
    #     affect_columns = selector_type("factor")
    # ) %>>%
    po("scale",
        scale = TRUE, # scale
        affect_columns = selector_type("numeric")
    ) %>>%
    po("filter", # rm highly correlated vars
        filter = flt("find_correlation"), filter.cutoff = 0.8
    ) 
    # %>>%
    # po("filter", # boruta filter
    #     filter = mlr3filters::flt("boruta"), filter.cutoff = 1
    # ) %>>%
    # po("classbalancing", # classbalancing
    #     reference = "major", adjust = "minor", shuffle = FALSE, ratio = 1
    # )

# pipline application
taskDisPo1 <- po1Dis$train(taskDis)[[1]]
print(names(taskDisPo1$data()))

# //!SECTION

# //SECTION - shap

# //ANCHOR - feature selection

# load learner
learnerRpart2 = lrn("classif.rpart", predict_type = "prob")
learnerRanger2 = lrn("classif.ranger", importance = "impurity", predict_type = "prob")

# feature selection
instanceDis = fselect(
  fselector = fs("rfecv"),
  task = taskDisPo1,
  learner = learnerRanger2,
  resampling = rsmp("cv", folds = 5),
  measure = msr("classif.ce"),
  store_models = TRUE
)

# best performing feature subset
instanceDis$result

# all evaluated feature subsets
as.data.table(instanceDis$archive)

# subset the task and fit the final model
taskDisPo1$select(instanceDis$result_feature_set)
learnerRanger2$train(taskDisPo1)

# //ANCHOR - visualization

dfShapDis <- taskDisPo1$data()
print(names(dfShapDis))

# shap
library(kernelshap)

shapKsDis <- kernelshap(learnerRanger2, dfShapDis[1:150, -1], predict_type = "prob")

# viz
library(shapviz)

vizKsDis <- shapviz(shapKsDis, which_class = 1)

sv_importance(vizKsDis, kind = "beeswarm")

# dfShapDis[, ]
# sv_force(vizKsDis, row_id = 1) # ture negative
# dfShapDis[, ]
# sv_force(vizKsDis, row_id = 9) # ture positive

# //!SECTION