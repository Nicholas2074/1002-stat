# //SECTION - 1day mean indicators

# //ANCHOR - preprocess

library(tidyverse)

compareGroupDay1 <- icpBpHr2 %>% 
    filter(interval < 288) %>% 
    group_by(icuid) %>% 
    summarize(
        avgicp = round(mean(icp, na.rm = TRUE), 2),
        avgisbp = round(mean(isbp, na.rm = TRUE), 2),
        avgidbp = round(mean(idbp, na.rm = TRUE), 2),
        avghr = round(mean(hr, na.rm = TRUE), 2),
        avgmbp = round(mean(mbp, na.rm = TRUE), 2),
        avgpp = round(mean(pp, na.rm = TRUE), 2),
        avgcpp = round(mean(cpp, na.rm = TRUE), 2),
        avgrpp = round(mean(rpp, na.rm = TRUE), 2),
        cvicp = round((sd(icp, na.rm = TRUE) / mean(icp, na.rm = TRUE)), 2),
        cvisbp = round((sd(isbp, na.rm = TRUE) / mean(isbp, na.rm = TRUE)), 2),
        cvidbp = round((sd(idbp, na.rm = TRUE) / mean(idbp, na.rm = TRUE)), 2),
        cvhr = round((sd(hr, na.rm = TRUE) / mean(hr, na.rm = TRUE)), 2),
        cvmbp = round((sd(mbp, na.rm = TRUE) / mean(mbp, na.rm = TRUE)), 2),
        cvpp = round((sd(pp, na.rm = TRUE) / mean(pp, na.rm = TRUE)), 2),
        cvcpp = round((sd(cpp, na.rm = TRUE) / mean(cpp, na.rm = TRUE)), 2),
        cvrpp = round((sd(rpp, na.rm = TRUE) / mean(rpp, na.rm = TRUE)), 2),
        .groups = "drop"
    )

# including patient, score, diagnosis, surgery, day1vital, chemistry group
compareGroupDay1 <- merge(compareGroupDay1, varsImp[, c(1, 31, 67, 69, 71, 77)], by = "icuid", all.y = TRUE)

# //ANCHOR - comparegroups

library(compareGroups)

tableDay1Traj <- descrTable(group ~ . - icuid,
    data = compareGroupDay1,
    method = NA,
    show.all = TRUE
)

# export2word(tableDay1Traj, file = "tableDay1Traj.docx")

# //ANCHOR - pairwise

bonfAvgICPDay1 <- pairwise.t.test(compareGroupDay1$avgicp, compareGroupDay1$group, p.adj = "bonf", data = compareGroupDay1)
print(bonfAvgICPDay1)

bonfAvgIsbpDay1 <- pairwise.t.test(compareGroupDay1$avgisbp, compareGroupDay1$group, p.adj = "bonf", data = compareGroupDay1)
print(bonfAvgIsbpDay1)

bonfAvgIdbpDay1 <- pairwise.t.test(compareGroupDay1$avgidbp, compareGroupDay1$group, p.adj = "bonf", data = compareGroupDay1)
print(bonfAvgIdbpDay1)

bonfAvgHrDay1 <- pairwise.t.test(compareGroupDay1$avghr, compareGroupDay1$group, p.adj = "bonf", data = compareGroupDay1)
print(bonfAvgHrDay1)

bonfAvgMbpDay1 <- pairwise.t.test(compareGroupDay1$avgmbp, compareGroupDay1$group, p.adj = "bonf", data = compareGroupDay1)
print(bonfAvgMbpDay1)

bonfAvgPPDay1 <- pairwise.t.test(compareGroupDay1$avgpp, compareGroupDay1$group, p.adj = "bonf", data = compareGroupDay1)
print(bonfAvgPPDay1)

bonfAvgCPPDay1 <- pairwise.t.test(compareGroupDay1$avgcpp, compareGroupDay1$group, p.adj = "bonf", data = compareGroupDay1)
print(bonfAvgCPPDay1)

# bonfAvgRPPDay1 <- pairwise.t.test(compareGroupDay1$avgrpp, compareGroupDay1$group, p.adj = "bonf", data = compareGroupDay1)
# print(bonfAvgRPPDay1)

bonfCVICPDay1 <- pairwise.t.test(compareGroupDay1$cvicp, compareGroupDay1$group, p.adj = "bonf", data = compareGroupDay1)
print(bonfCVICPDay1)

bonfCVIsbpDay1 <- pairwise.t.test(compareGroupDay1$cvisbp, compareGroupDay1$group, p.adj = "bonf", data = compareGroupDay1)
print(bonfCVIsbpDay1)

bonfCVIdbpDay1 <- pairwise.t.test(compareGroupDay1$cvidbp, compareGroupDay1$group, p.adj = "bonf", data = compareGroupDay1)
print(bonfCVIdbpDay1)

bonfCVHrDay1 <- pairwise.t.test(compareGroupDay1$cvhr, compareGroupDay1$group, p.adj = "bonf", data = compareGroupDay1)
print(bonfCVHrDay1)

bonfCVMbpDay1 <- pairwise.t.test(compareGroupDay1$cvmbp, compareGroupDay1$group, p.adj = "bonf", data = compareGroupDay1)
print(bonfCVMbpDay1)

bonfCVPPDay1 <- pairwise.t.test(compareGroupDay1$cvpp, compareGroupDay1$group, p.adj = "bonf", data = compareGroupDay1)
print(bonfCVPPDay1)

bonfCVCPPDay1 <- pairwise.t.test(compareGroupDay1$cvcpp, compareGroupDay1$group, p.adj = "bonf", data = compareGroupDay1)
print(bonfCVCPPDay1)

# bonfCVRPPDay1 <- pairwise.t.test(compareGroupDay1$cvrpp, compareGroupDay1$group, p.adj = "bonf", data = compareGroupDay1)
# print(bonfCVRPPDay1)

bonfChlorideDay1 <- pairwise.t.test(compareGroupDay1$chloride, compareGroupDay1$group, p.adj = "bonf", data = compareGroupDay1)
print(bonfChlorideDay1)

bonfPhDay1 <- pairwise.t.test(compareGroupDay1$ph, compareGroupDay1$group, p.adj = "bonf", data = compareGroupDay1)
print(bonfPhDay1)

bonfPaco2Day1 <- pairwise.t.test(compareGroupDay1$paco2, compareGroupDay1$group, p.adj = "bonf", data = compareGroupDay1)
print(bonfPaco2Day1)

bonfSpo2Day1 <- pairwise.t.test(compareGroupDay1$spo2_cv, compareGroupDay1$group, p.adj = "bonf", data = compareGroupDay1)
print(bonfSpo2Day1)

# //!SECTION

# //SECTION - 5day mean indicators

# //ANCHOR - preprocess

library(tidyverse)

compareGroupDay5 <- icpBpHr2 %>%
    group_by(icuid) %>%
    summarize(
        avgicp = round(mean(icp, na.rm = TRUE), 2),
        avgisbp = round(mean(isbp, na.rm = TRUE), 2),
        avgidbp = round(mean(idbp, na.rm = TRUE), 2),
        avghr = round(mean(hr, na.rm = TRUE), 2),
        avgmbp = round(mean(mbp, na.rm = TRUE), 2),
        avgpp = round(mean(pp, na.rm = TRUE), 2),
        avgcpp = round(mean(cpp, na.rm = TRUE), 2),
        avgrpp = round(mean(rpp, na.rm = TRUE), 2),
        cvicp = round((sd(icp, na.rm = TRUE) / mean(icp, na.rm = TRUE)), 2),
        cvisbp = round((sd(isbp, na.rm = TRUE) / mean(isbp, na.rm = TRUE)), 2),
        cvidbp = round((sd(idbp, na.rm = TRUE) / mean(idbp, na.rm = TRUE)), 2),
        cvhr = round((sd(hr, na.rm = TRUE) / mean(hr, na.rm = TRUE)), 2),
        cvmbp = round((sd(mbp, na.rm = TRUE) / mean(mbp, na.rm = TRUE)), 2),
        cvpp = round((sd(pp, na.rm = TRUE) / mean(pp, na.rm = TRUE)), 2),
        cvcpp = round((sd(cpp, na.rm = TRUE) / mean(cpp, na.rm = TRUE)), 2),
        cvrpp = round((sd(rpp, na.rm = TRUE) / mean(rpp, na.rm = TRUE)), 2),
        .groups = "drop"
    )

compareGroupDay5 <- merge(compareGroupDay5, varsImp[, c(1, 77)], by = "icuid", all.y = TRUE)

# //ANCHOR - comparegroups

library(compareGroups)

tableDay5Traj <- descrTable(group ~ . - icuid,
    data = compareGroupDay5,
    method = NA,
    show.all = TRUE
)

# export2word(tableDay5Traj, file = "tableDay5Traj.docx")

# //ANCHOR - pairwise

bonfAvgICPDay5 <- pairwise.t.test(compareGroupDay5$avgicp, compareGroupDay5$group, p.adj = "bonf", data = compareGroupDay5)
print(bonfAvgICPDay5)

bonfAvgIsbpDay5 <- pairwise.t.test(compareGroupDay5$avgisbp, compareGroupDay5$group, p.adj = "bonf", data = compareGroupDay5)
print(bonfAvgIsbpDay5)

bonfAvgIdbpDay5 <- pairwise.t.test(compareGroupDay5$avgidbp, compareGroupDay5$group, p.adj = "bonf", data = compareGroupDay5)
print(bonfAvgIdbpDay5)

bonfAvgHrDay5 <- pairwise.t.test(compareGroupDay5$avghr, compareGroupDay5$group, p.adj = "bonf", data = compareGroupDay5)
print(bonfAvgHrDay5)

bonfAvgMbpDay5 <- pairwise.t.test(compareGroupDay5$avgmbp, compareGroupDay5$group, p.adj = "bonf", data = compareGroupDay5)
print(bonfAvgMbpDay5)

bonfAvgPPDay5 <- pairwise.t.test(compareGroupDay5$avgpp, compareGroupDay5$group, p.adj = "bonf", data = compareGroupDay5)
print(bonfAvgPPDay5)

bonfAvgCPPDay5 <- pairwise.t.test(compareGroupDay5$avgcpp, compareGroupDay5$group, p.adj = "bonf", data = compareGroupDay5)
print(bonfAvgCPPDay5)

# bonfAvgRPPDay5 <- pairwise.t.test(compareGroupDay5$avgrpp, compareGroupDay5$group, p.adj = "bonf", data = compareGroupDay5)
# print(bonfAvgRPPDay5)

bonfCVICPDay5 <- pairwise.t.test(compareGroupDay5$cvicp, compareGroupDay5$group, p.adj = "bonf", data = compareGroupDay5)
print(bonfCVICPDay5)

bonfCVIsbpDay5 <- pairwise.t.test(compareGroupDay5$cvisbp, compareGroupDay5$group, p.adj = "bonf", data = compareGroupDay5)
print(bonfCVIsbpDay5)

bonfCVIdbpDay5 <- pairwise.t.test(compareGroupDay5$cvidbp, compareGroupDay5$group, p.adj = "bonf", data = compareGroupDay5)
print(bonfCVIdbpDay5)

bonfCVHrDay5 <- pairwise.t.test(compareGroupDay5$cvhr, compareGroupDay5$group, p.adj = "bonf", data = compareGroupDay5)
print(bonfCVHrDay5)

bonfCVMbpDay5 <- pairwise.t.test(compareGroupDay5$cvmbp, compareGroupDay5$group, p.adj = "bonf", data = compareGroupDay5)
print(bonfCVMbpDay5)

bonfCVPPDay5 <- pairwise.t.test(compareGroupDay5$cvpp, compareGroupDay5$group, p.adj = "bonf", data = compareGroupDay5)
print(bonfCVPPDay5)

bonfCVCPPDay5 <- pairwise.t.test(compareGroupDay5$cvcpp, compareGroupDay5$group, p.adj = "bonf", data = compareGroupDay5)
print(bonfCVCPPDay5)

# bonfCVRPPDay5 <- pairwise.t.test(compareGroupDay5$cvrpp, compareGroupDay5$group, p.adj = "bonf", data = compareGroupDay5)
# print(bonfCVRPPDay5)

# //!SECTION