# //ANCHOR - preprocess

library(tidyverse)

# filter
dfICP1 <- icpBpHr2 %>%
    select(icuid, interval, icp) %>%
    filter(!is.na(icp), icp >= 20)

# calculate seq
dfICP1 <- dfICP1 %>%
    group_by(icuid) %>%
    arrange(icuid, interval) %>%
    mutate(
        seq = cumsum(c(0, diff(interval) != 1L))
    ) %>%
    ungroup()

# filter the data with 2 consecutive records of ih
dfICP1 <- dfICP1 %>%
    group_by(icuid, seq) %>%
    filter(n() >= 2) %>%
    ungroup()

# calculate start time and end time
dfICP1 <- dfICP1 %>%
    group_by(icuid, seq) %>%
    summarize(
        starttime = first(interval),
        endtime = last(interval),
        los = endtime - starttime
    ) %>%
    ungroup()

# filter the data with >= 24h los
dfICP1 <- dfICP1 %>%
    group_by(icuid) %>%
    filter(!any(los >= 288)) %>% # the interval is set at 5 mins, and the unit of los is also 5 mins
    ungroup()

# renumber
dfICP1 <- dfICP1 %>%
    group_by(icuid) %>%
    mutate(
        seq = row_number()
    ) %>%
    ungroup()

intGroupDay5 <- dfICP1 %>%
    group_by(icuid) %>%
    mutate(
        pct = sum(los) / 1440,
    ) %>% 
    ungroup()

names(intGroupDay5)

intGroupDay5 <- merge(intGroupDay5[, c("icuid", "seq", "los", "pct")], varsImp[, c(1, 77)], by = "icuid", all.y = TRUE)

any(is.na(intGroupDay5))

intGroupDay5[is.na(intGroupDay5)] <- 0

# //ANCHOR - comparegroups

library(compareGroups)

tableDay5Stat <- descrTable(group ~ . - icuid,
    data = intGroupDay5,
    method = NA,
    show.all = TRUE
)

# export2word(tableDay5Stat, file = "tableDay5Stat.docx")

# //ANCHOR - pairwise

bonfSeqICPDay5 <- pairwise.t.test(intGroupDay5$seq, intGroupDay5$group, p.adj = "bonf", data = intGroupDay5)
print(bonfSeqICPDay5)

bonfLosICPDay5 <- pairwise.t.test(intGroupDay5$los, intGroupDay5$group, p.adj = "bonf", data = intGroupDay5)
print(bonfLosICPDay5)

bonfPctICPDay5 <- pairwise.t.test(intGroupDay5$pct, intGroupDay5$group, p.adj = "bonf", data = intGroupDay5)
print(bonfPctICPDay5)
