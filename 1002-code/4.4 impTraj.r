# //SECTION - boruta

library(mlr3verse)

library(tidyverse)

set.seed(0)

# //ANCHOR - preprocess

# regroup
dfTraj <- varsImp %>% 
    mutate(
        group = ifelse(group == 5, 1, 0)
    )

# delete icuid
dfTrajDel <- dfTraj[, -1]

# task definition
taskTraj <- as_task_classif(dfTrajDel, target = "group", positive = "1")

# pipline building
po1Traj <-
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
    # po("filter", # rm highly correlated vars
    #     filter = flt("find_correlation"), filter.cutoff = 0.7
    # ) %>>%
    po("filter", # boruta filter
        filter = mlr3filters::flt("boruta"), filter.cutoff = 1
    ) 
    # %>>%
    # po("classbalancing", # classbalancing
    #     reference = "major", adjust = "minor", shuffle = FALSE, ratio = 1
    # )

# pipline application
taskTrajPo1 <- po1Traj$train(taskTraj)[[1]]
print(names(taskTrajPo1$data()))

# //!SECTION

# //SECTION - shap

# //ANCHOR - feature selection

# load learner
learnerRpart4 = lrn("classif.rpart", predict_type = "prob")
learnerRanger4 = lrn("classif.ranger", importance = "impurity", predict_type = "prob")

# feature selection
instanceTraj = fselect(
  fselector = fs("rfecv"),
  task = taskTrajPo1,
  learner = learnerRanger4,
  resampling = rsmp("cv", folds = 5),
  measure = msr("classif.ce"),
  store_models = TRUE
)

# best performing feature subset
instanceTraj$result

# all evaluated feature subsets
as.data.table(instanceTraj$archive)

# subset the task and fit the final model
taskTrajPo1$select(instanceTraj$result_feature_set)
learnerRanger4$train(taskTrajPo1)

# //ANCHOR - visualization

dfShapTraj <- taskTrajPo1$data()
print(names(dfShapTraj))

# shap
library(kernelshap)

shapKsTraj <- kernelshap(learnerRanger4, dfShapTraj[1:150, -1], predict_type = "prob")

# viz
library(shapviz)

vizKsTraj <- shapviz(shapKsTraj, which_class = 1)

sv_importance(vizKsTraj, kind = "beeswarm")

# dfShapTraj[, ]
# sv_force(vizKsTraj, row_id = 1) # ture negative
# dfShapTraj[, ]
# sv_force(vizKsTraj, row_id = 9) # ture positive

# //!SECTION