# //SECTION - internal datasets

# //ANCHOR - cushing reflex

reTrajImp <- trajImp %>%
    mutate(
        cushing = case_when(
            icp > 22 & isbp > 140 & hr < 60 ~ 1,
            TRUE ~ 0
        )
    )

reTrajImp  <- reTrajImp %>% 
    group_by(icuid) %>% 
    mutate(cushingCount = sum(cushing == 1)) %>% 
    ungroup()

table(reTrajImp$cushing)

reTrajCushing <- merge(varsImp[, c("icuid", "group")], reTrajImp, by = "icuid", all.x = TRUE)

library(compareGroups)

reTableCushing <- descrTable(group ~ cushing,
    data = reTrajCushing,
    method = NA,
    show.all = TRUE,
    show.p.mul = TRUE,
    hide.no = "0"

)
# reTableCushing

export2word(reTableCushing, file = "reTableCushing.docx")

# //ANCHOR - outcome

reTrajOut <- merge(varsImp[, c("icuid", "group")], mortality, by = "icuid", all.x = TRUE)
reTrajOut <- merge(reTrajOut, gcs, by = "icuid", all.x = TRUE)

library(compareGroups)

reTableOut <- descrTable(group ~ . -icuid,
    data = reTrajOut,
    method = NA,
    show.all = TRUE,
    show.p.mul = TRUE
)
# reTableOut

export2word(reTableOut, file = "reTableOut.docx")

# //!SECTION

# //SECTION - external datasets

# //ANCHOR - cushing reflex

hReTrajImp <- hTrajImp %>%
    mutate(
        cushing = case_when(
            icp > 22 & sbp > 140 & hr < 60 ~ 1,
            TRUE ~ 0
        )
    )

hReTrajImp  <- hReTrajImp %>% 
    group_by(icuid) %>% 
    mutate(cushingCount = sum(cushing == 1)) %>% 
    ungroup()

table(hReTrajImp$cushing)

hReTrajCushing <- merge(hTrajGroup, hReTrajImp, by = "icuid", all.x = TRUE)

library(compareGroups)

hReTableCushing <- descrTable(group ~ cushing,
    data = hReTrajCushing,
    method = NA,
    show.all = TRUE,
    show.p.mul = TRUE,
    hide.no = "0"

)
# hReTableCushing

export2word(hReTableCushing, file = "hReTableCushing.docx")

# //ANCHOR - outcome

hTrajOut <- merge(hTrajGroup, hmortality, by = "icuid", all.x = TRUE)
hTrajOut <- merge(hTrajOut, hgcs, by = "icuid", all.x = TRUE)

library(compareGroups)

hTableOut <- descrTable(group ~ . -icuid,
    data = hTrajOut,
    method = NA,
    show.all = TRUE,
    show.p.mul = TRUE
)
# hTableOut

export2word(hTableOut, file = "hTableOut.docx")

# //ANCHOR - patient

hpatient <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/hpatient.csv", header = TRUE)

names(hpatient) <- c("icuid", "age", "gender", "bmi")

hpatient <- hpatient %>% 
    mutate(gender = factor(
        gender,
        levels = c(0, 1),
        labels = c("Male", "Female")
    ))

hpatient <- hpatient %>% distinct(icuid,.keep_all = TRUE)

hTrajPat <- merge(hTrajGroup, hpatient, by = "icuid", all.x = TRUE)

library(compareGroups)

hTablePat <- descrTable(group ~ . -icuid,
    data = hTrajPat,
    method = NA,
    show.all = TRUE,
    show.p.mul = TRUE
)
# hTablePat

export2word(hTablePat, file = "hTablePat.docx")

# //ANCHOR - 1day mean indicators

library(tidyverse)

hCompareGroupDay1 <- hIcpBpHr2 %>% 
    filter(interval < 288) %>% 
    group_by(icuid) %>% 
    summarize(
        avgicp = round(mean(icp, na.rm = TRUE), 2),
        avgsbp = round(mean(sbp, na.rm = TRUE), 2),
        avgdbp = round(mean(dbp, na.rm = TRUE), 2),
        avghr = round(mean(hr, na.rm = TRUE), 2),
        avgmbp = round(mean(mbp, na.rm = TRUE), 2),
        avgpp = round(mean(pp, na.rm = TRUE), 2),
        avgcpp = round(mean(cpp, na.rm = TRUE), 2),
        avgrpp = round(mean(rpp, na.rm = TRUE), 2),
        cvicp = round((sd(icp, na.rm = TRUE) / mean(icp, na.rm = TRUE)), 2),
        cvsbp = round((sd(sbp, na.rm = TRUE) / mean(sbp, na.rm = TRUE)), 2),
        cvdbp = round((sd(dbp, na.rm = TRUE) / mean(dbp, na.rm = TRUE)), 2),
        cvhr = round((sd(hr, na.rm = TRUE) / mean(hr, na.rm = TRUE)), 2),
        cvmbp = round((sd(mbp, na.rm = TRUE) / mean(mbp, na.rm = TRUE)), 2),
        cvpp = round((sd(pp, na.rm = TRUE) / mean(pp, na.rm = TRUE)), 2),
        cvcpp = round((sd(cpp, na.rm = TRUE) / mean(cpp, na.rm = TRUE)), 2),
        cvrpp = round((sd(rpp, na.rm = TRUE) / mean(rpp, na.rm = TRUE)), 2),
        .groups = "drop"
    )

hCompareGroupDay1 <- merge(hTrajGroup, hCompareGroupDay1, by = "icuid", all.x = TRUE)

library(missForest)

set.seed(0)

hCompareGroupDay1Mf <- missForest(hCompareGroupDay1)

hCompareGroupDay1Imp <- hCompareGroupDay1Mf$ximp

library(compareGroups)

hTableDay1Traj <- descrTable(group ~ . -icuid,
    data = hCompareGroupDay1Imp,
    method = NA,
    show.all = TRUE
)
hTableDay1Traj

export2word(hTableDay1Traj, file = "hTableDay1Traj.docx")

# //ANCHOR - 5day mean indicators

library(tidyverse)

hCompareGroupDay5 <- hIcpBpHr2 %>% 
    group_by(icuid) %>% 
    summarize(
        avgicp = round(mean(icp, na.rm = TRUE), 2),
        avgsbp = round(mean(sbp, na.rm = TRUE), 2),
        avgdbp = round(mean(dbp, na.rm = TRUE), 2),
        avghr = round(mean(hr, na.rm = TRUE), 2),
        avgmbp = round(mean(mbp, na.rm = TRUE), 2),
        avgpp = round(mean(pp, na.rm = TRUE), 2),
        avgcpp = round(mean(cpp, na.rm = TRUE), 2),
        avgrpp = round(mean(rpp, na.rm = TRUE), 2),
        cvicp = round((sd(icp, na.rm = TRUE) / mean(icp, na.rm = TRUE)), 2),
        cvsbp = round((sd(sbp, na.rm = TRUE) / mean(sbp, na.rm = TRUE)), 2),
        cvdbp = round((sd(dbp, na.rm = TRUE) / mean(dbp, na.rm = TRUE)), 2),
        cvhr = round((sd(hr, na.rm = TRUE) / mean(hr, na.rm = TRUE)), 2),
        cvmbp = round((sd(mbp, na.rm = TRUE) / mean(mbp, na.rm = TRUE)), 2),
        cvpp = round((sd(pp, na.rm = TRUE) / mean(pp, na.rm = TRUE)), 2),
        cvcpp = round((sd(cpp, na.rm = TRUE) / mean(cpp, na.rm = TRUE)), 2),
        cvrpp = round((sd(rpp, na.rm = TRUE) / mean(rpp, na.rm = TRUE)), 2),
        .groups = "drop"
    )

hCompareGroupDay5 <- merge(hTrajGroup, hCompareGroupDay5, by = "icuid", all.x = TRUE)

library(compareGroups)

hTableDay5Traj <- descrTable(group ~ . -icuid,
    data = hCompareGroupDay5,
    method = NA,
    show.all = TRUE
)

export2word(hTableDay5Traj, file = "hTableDay5Traj.docx")

# //!SECTION