# # //SECTION - import

# library(tidyverse)

# # ---------------------------------------------------------------------------- #
# #                             time interval: 5 mins                            #
# # ---------------------------------------------------------------------------- #

# # //ANCHOR - hirid

# # icp
# hicp <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/hicp.csv", header = TRUE)

# names(hicp)[1] <- "icuid"

# # bp
# hbp <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/hbp.csv", header = TRUE)

# names(hbp)[1] <- "icuid"

# # hr
# hhr <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/hhr.csv", header = TRUE)

# names(hhr)[1] <- "icuid"

# # merge
# hBpHr <- merge(hbp, hhr, by = c("icuid", "interval"), all = FALSE)
# hIcpBpHr <- merge(hicp, hBpHr, by = c("icuid", "interval"), all = FALSE)

# # //!SECTION

# # //SECTION - preprocess

# # //ANCHOR - denoise

# hIcpBpHr0 <- hIcpBpHr

# # denoise of icp
# hIcpBpHr0$icp <-
#     ifelse(hIcpBpHr0$icp >= 100, NA, hIcpBpHr0$icp)

# # denoise of sbp
# hIcpBpHr0$sbp <-
#     ifelse(hIcpBpHr0$sbp < 30 | hIcpBpHr0$sbp > 300, NA, hIcpBpHr0$sbp)

# # denoise of dbp
# hIcpBpHr0$dbp <-
#     ifelse(hIcpBpHr0$dbp < 10 | hIcpBpHr0$dbp > 200, NA, hIcpBpHr0$dbp)

# # denoise of hr
# hIcpBpHr0$hr <-
#     ifelse(hIcpBpHr0$hr < 10, NA, hIcpBpHr0$hr)

# # //ANCHOR - derive

# hIcpBpHr1 <- hIcpBpHr0

# hIcpBpHr1$mbp <- round(((hIcpBpHr1$sbp + 2 * hIcpBpHr1$dbp) / 3))
# hIcpBpHr1$pp <- round((hIcpBpHr1$sbp - hIcpBpHr1$dbp))
# hIcpBpHr1$cpp <- round((hIcpBpHr1$mbp - hIcpBpHr1$icp))
# hIcpBpHr1$rpp <- round(hIcpBpHr1$sbp * hIcpBpHr1$hr)

# # //ANCHOR - fill

# # filling the missing data with n/a
# hIcpBpHr2 <- hIcpBpHr1 %>% 
#     complete(
#         icuid,
#         interval = seq(0, 1439),
#         fill = list(
#             icp = NA,
#             sbp = NA,
#             dbp = NA,
#             hr = NA,
#             mbp = NA,
#             pp = NA,
#             cpp = NA,
#             rpp = NA
#         )
#     )

# # //!SECTION

# # //SECTION - sample

# # //ANCHOR - avg

# # avg of 4h
# hAvg4H <- hIcpBpHr2 %>%
#     mutate(interval = floor(interval / 48)) %>%
#     group_by(icuid, interval) %>%
#     summarize(
#         icp = round(mean(icp, na.rm = TRUE), 2),
#         sbp = round(mean(sbp, na.rm = TRUE), 2),
#         dbp = round(mean(dbp, na.rm = TRUE), 2),
#         hr = round(mean(hr, na.rm = TRUE), 2),
#         mbp = round(mean(mbp, na.rm = TRUE), 2),
#         pp = round(mean(pp, na.rm = TRUE), 2),
#         cpp = round(mean(cpp, na.rm = TRUE), 2),
#         rpp = round(mean(rpp, na.rm = TRUE), 2),
#         .groups = "drop"
#     )

# # inclusion
# hAvgDf <- hAvg4H %>%
#     group_by(icuid) %>%
#     filter(sum(is.na(icp)) <= 25) %>%
#     filter(sum(is.na(sbp)) <= 25) %>%
#     filter(sum(is.na(dbp)) <= 25) %>%
#     filter(sum(is.na(hr)) <= 25) %>%
#     as.data.frame()

# colMeans(is.na(hAvgDf))

print(length(unique(hAvg4H$icuid)))
print(length(unique(hAvgDf$icuid)))

# # //ANCHOR - imputation

# library(missForest)

# set.seed(0)

# hTrajMf <- missForest(hAvgDf)

# hTrajImp <- hTrajMf$ximp

# # //!SECTION

# # //SECTION - gbmt

# # //ANCHOR - modeling

# varsAvg2 <- c("icp", "sbp", "dbp", "hr")

# library(gbmt)

# hGbmt35 <- gbmt(
#     x.names = varsAvg2,
#     unit = "icuid",
#     time = "interval",
#     d = 3,
#     ng = 5,
#     data = hTrajImp,
#     scaling = 1,
#     maxit = 200
# )

# hGbmt35

# # hGbmt35 is the best

# # traj group
# hTrajGroup <- as.data.frame(unique(hTrajImp$icuid))
# names(hTrajGroup) <- "icuid"

# hTrajAssign <- hGbmt35$assign.list

# hTrajGroup$group[hTrajGroup$icuid %in% hTrajAssign[[1]]] <- 1
# hTrajGroup$group[hTrajGroup$icuid %in% hTrajAssign[[2]]] <- 2
# hTrajGroup$group[hTrajGroup$icuid %in% hTrajAssign[[3]]] <- 3
# hTrajGroup$group[hTrajGroup$icuid %in% hTrajAssign[[4]]] <- 4
# hTrajGroup$group[hTrajGroup$icuid %in% hTrajAssign[[5]]] <- 5
# # hTrajGroup$group[hTrajGroup$icuid %in% hTrajAssign[[6]]] <- 6

# hTrajGroup$group <- as.factor(hTrajGroup$group)

# # //ANCHOR - plot

# library(gbmt)

# mar0 <- c(3.1, 2.55, 3.1, 1.2)

plot(hGbmt35,
    n.ahead = 3,
    bands = FALSE,
    # conf = 0.95,
    # ylim = c(-0.1, 0.1),
    mar = mar0,
    equal.scale = TRUE
    # trim = 0.05
    # transparency = 95
)

plot(hGbmt35,
    group = 5,
    n.ahead = 3,
    # bands = FALSE,
    ylim = c(-30, 30),
    mar = mar0,
    equal.scale = TRUE
)

# # //!SECTION

# # //SECTION - outcome

# # //ANCHOR - hospMortality

# hmortality <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/hmortality.csv", header = TRUE)

# names(hmortality) <- c("icuid", "hospMortality")

# hmortality$hospMortality[is.na(hmortality$hospMortality)] <- 0

# # //ANCHOR - gcs

# hgcs <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/hdev_gcs.csv", header = TRUE)

# names(hgcs) <- c("icuid", "admgcs", "disgcs", "devgcs")

# # filling
# hgcs$disgcs[is.na(hgcs$disgcs)] <- 0

# hgcs$devgcs[is.na(hgcs$devgcs)] <- 0

# # relabel
# hgcs$disgcs <- ifelse(hgcs$disgcs <= 8, 1, 0)

# hgcs$devgcs <- ifelse(hgcs$devgcs <= 0, 1, 0)

# # //ANCHOR - link

# # merge
# hTrajMor <- merge(hTrajGroup, hmortality, by = "icuid", all.x = TRUE)
# hTrajDis <- merge(hTrajGroup, hgcs[, c(1, 3)], by = "icuid", all.x = TRUE)
# hTrajDev <- merge(hTrajGroup, hgcs[, c(1, 4)], by = "icuid", all.x = TRUE)

# print(length(unique(hTrajMor$icuid)))
# print(length(unique(hTrajDis$icuid)))
# print(length(unique(hTrajDev$icuid)))

# any(is.na(hTrajMor))
# any(is.na(hTrajDis))
# any(is.na(hTrajDev))

# # //!SECTION