# //SECTION - tuning

varsAvg1 <- c("icp", "isbp", "idbp", "hr")

# //ANCHOR - grid search

# parameter
paraGrid <- expand.grid(2:3, 2:6)
dim(paraGrid)
# [1] 10  2

# parallel computing
library(gbmt)

library(doParallel)

# clear env
# env <- foreach:::.foreachGlobals
# rm(list = ls(name = env), pos = env)

# cores
cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)

# 0 (no normalisation)
# 1 (centering)
# 2 (standardization)
# 3 (ratio to the mean) 
# 4 (logarithmic ratio to the mean)
# Default is 2 (standardization)

gbmtList <- list()

gbmtList <- foreach(i = 1:10, .packages = "gbmt") %dopar% {
    gbmt(
        x.names = varsAvg1,
        unit = "icuid",
        time = "interval",
        d = paraGrid[i, 1],
        ng = paraGrid[i, 2],
        data = trajImp,
        scaling = 1,
        maxit = 200
    )
}

# stop
stopCluster(cl)

gbmtListInfo <- lapply(gbmtList, function(mod) {
    mod$ic
})

print(gbmtListInfo)

gbmtList[[8]] # model 8 is the best

print(paraGrid)

# # //ANCHOR - modeling

# library(gbmt)

# gbmt35 <- gbmt(
#     x.names = varsAvg1,
#     unit = "icuid",
#     time = "interval",
#     d = 3,
#     ng = 5,
#     data = trajImp,
#     scaling = 1,
#     maxit = 200
# )

gbmt35

# # gbmt35 is the best

# # traj group
# trajGroup <- as.data.frame(unique(trajImp$icuid))
# names(trajGroup) <- "icuid"

# trajAssign <- gbmt35$assign.list

# trajGroup$group[trajGroup$icuid %in% trajAssign[[1]]] <- 1
# trajGroup$group[trajGroup$icuid %in% trajAssign[[2]]] <- 2
# trajGroup$group[trajGroup$icuid %in% trajAssign[[3]]] <- 3
# trajGroup$group[trajGroup$icuid %in% trajAssign[[4]]] <- 4
# trajGroup$group[trajGroup$icuid %in% trajAssign[[5]]] <- 5
# # trajGroup$group[trajGroup$icuid %in% trajAssign[[6]]] <- 6

# trajGroup$group <- as.factor(trajGroup$group)

# # //!SECTION

# # //SECTION - plot

# mar0 <- c(3.1, 2.55, 3.1, 1.2)

# plot(gbmt35,
#     n.ahead = 3,
#     bands = FALSE,
#     # ylim = c(-0.1, 0.1),
#     mar = mar0,
#     equal.scale = TRUE
#     # trim = 0.05
#     # transparency = 95
# )

# plot(gbmt35,
#     group = 5,
#     n.ahead = 3,
#     # bands = FALSE,
#     ylim = c(-30, 30),
#     mar = mar0,
#     equal.scale = TRUE
# )

# # //!SECTION