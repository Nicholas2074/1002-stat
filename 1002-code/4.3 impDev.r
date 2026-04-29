# //SECTION - boruta

library(mlr3verse)

library(tidyverse)

set.seed(0)

# //ANCHOR - preprocess

dfDev <- merge(varsImp, gcs[, c(1, 4)], by = "icuid", all = FALSE)

# delete icuid
dfDevDel <- dfDev[, -1]

# regroup
# dfDevDel$group <- ifelse(dfDevDel$group == 5, 1, 0)
dfDevDel$group <- as.factor(dfDevDel$group)

# task definition
taskDev <- as_task_classif(dfDevDel, target = "devgcs", positive = "1")

# pipline building
po1Dev <-
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
    ) 
    # %>>%
    # po("filter", # rm highly correlated vars
    #     filter = flt("find_correlation"), filter.cutoff = 0.8
    # ) %>>%
    # po("filter", # boruta filter
    #     filter = mlr3filters::flt("boruta"), filter.cutoff = 1
    # ) 
    # %>>%
    # po("classbalancing", # classbalancing
    #     reference = "major", adjust = "minor", shuffle = FALSE, ratio = 1
    # )

# pipline application
taskDevPo1 <- po1Dev$train(taskDev)[[1]]
print(names(taskDevPo1$data()))

# //!SECTION

# //SECTION - shap

# //ANCHOR - feature selection

# load learner
learnerRpart3 = lrn("classif.rpart", predict_type = "prob")
learnerRanger3 = lrn("classif.ranger", importance = "impurity", predict_type = "prob")

# feature selection
instanceDev = fselect(
  fselector = fs("rfecv"),
  task = taskDevPo1,
  learner = learnerRanger3,
  resampling = rsmp("cv", folds = 5),
  measure = msr("classif.ce"),
  store_models = TRUE
)

# best performing feature subset
instanceDev$result

# all evaluated feature subsets
as.data.table(instanceDev$archive)

# subset the task and fit the final model
taskDevPo1$select(instanceDev$result_feature_set)
learnerRanger3$train(taskDevPo1)

# //ANCHOR - visualization

dfShapDev <- taskDevPo1$data()
print(names(dfShapDev))

# shap
library(kernelshap)

shapKsDev <- kernelshap(learnerRanger3, dfShapDev[1:150, -1], predict_type = "prob")

# viz
library(shapviz)

vizKsDev <- shapviz(shapKsDev, which_class = 1)

sv_importance(vizKsDev, kind = "beeswarm")

# dfShapDev[, ]
# sv_force(vizKsDev, row_id = 1) # ture negative
# dfShapDev[, ]
# sv_force(vizKsDev, row_id = 9) # ture positive

# //!SECTION