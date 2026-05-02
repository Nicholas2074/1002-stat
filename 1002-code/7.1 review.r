# //SECTION - cushing reflex

dim(trajImp)
names(trajImp)
summary(trajImp)

reTrajImp <- trajImp %>%
    mutate(
        cushing = case_when(
            icp > 22 & isbp > 140 & hr < 60 ~ 1,
            TRUE ~ 0
        )
    )

table(reTrajImp$cushing)

hTrajCushing <- merge(trajGroup, reTrajImp, by = "icuid", all = FALSE)

library(compareGroups)

reTableCushing <- descrTable(group ~ cushing,
    data = hTrajCushing,
    method = NA,
    show.all = TRUE,
    show.p.mul = TRUE,
    hide.no = "0"

)
reTableCushing

# export2word(reTableCushing, file = "reTableCushing.docx")

# //ANCHOR - outcome

# ----------------------------- external datasets ---------------------------- #

hTrajOut <- merge(hTrajGroup, hmortality, by = "icuid", all.x = TRUE)
hTrajOut <- merge(hTrajOut, hgcs, by = "icuid", all.x = TRUE)

library(compareGroups)

hTableOut <- descrTable(group ~ . - icuid,
    data = hTrajOut,
    method = NA,
    show.all = TRUE
)
hTableOut

# export2word(hTableOut, file = "hTableOut.docx")

# ----------------------------- internal datasets ---------------------------- #

trajOut <- merge(trajGroup, mortality, by = "icuid", all.x = TRUE)
trajOut <- merge(trajOut, gcs, by = "icuid", all.x = TRUE)

library(compareGroups)

tableOut <- descrTable(group ~ . - icuid,
    data = trajOut,
    method = NA,
    show.all = TRUE
)
tableOut

export2word(tableOut, file = "tableOut.docx")
