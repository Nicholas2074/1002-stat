# //SECTION - import

library(tidyverse)

# ---------------------------------------------------------------------------- #
#                             time interval: 5 mins                            #
# ---------------------------------------------------------------------------- #

# //ANCHOR - eicu

# icp
eicp <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/eicp.csv", header = TRUE)

names(eicp)[1] <- c("icuid")

# bp
ebp <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/ebp.csv", header = TRUE)

names(ebp)[1] <- c("icuid")

ebp <- ebp[, c("icuid", "interval", "isbp", "idbp")]

# hr
ehr <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/ehr.csv", header = TRUE)

names(ehr)[1] <- c("icuid")

# //ANCHOR - mimic

# icp
micp <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/micp.csv", header = TRUE)

names(micp)[1] <- c("icuid")

# bp
mbp <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/mbp.csv", header = TRUE)

names(mbp)[1] <- c("icuid")

mbp <- mbp[, c("icuid", "interval", "isbp", "idbp")]

# hr
mhr <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/mhr.csv", header = TRUE)

names(mhr)[1] <- c("icuid")

# //ANCHOR - combine

# combine
icp <- rbind(eicp, micp)
bp <- rbind(ebp, mbp)
hr <- rbind(ehr, mhr)

# merge
bpHr <- merge(bp, hr, by = c("icuid", "interval"), all = FALSE)
icpBpHr <- merge(icp, bpHr, by = c("icuid", "interval"), all = FALSE)

# //!SECTION

# //SECTION - preprocess

# //ANCHOR - denoise

icpBpHr0 <- icpBpHr

# denoise of icp
icpBpHr0$icp <-
    ifelse(icpBpHr0$icp >= 100, NA, icpBpHr0$icp)

# denoise of sbp
icpBpHr0$isbp <-
    ifelse(icpBpHr0$isbp < 30 | icpBpHr0$isbp > 300, NA, icpBpHr0$isbp)

# denoise of dbp
icpBpHr0$idbp <-
    ifelse(icpBpHr0$idbp < 10 | icpBpHr0$idbp > 200, NA, icpBpHr0$idbp)

# denoise of hr
icpBpHr0$hr <-
    ifelse(icpBpHr0$hr < 10, NA, icpBpHr0$hr)

# summary
# hist(icpBpHr0$icp)
# qqnorm(icpBpHr0$icp)
# qqline(icpBpHr0$icp)

# hist(icpBpHr0$isbp)
# qqnorm(icpBpHr0$isbp)
# qqline(icpBpHr0$isbp)

# hist(icpBpHr0$idbp)
# qqnorm(icpBpHr0$idbp)
# qqline(icpBpHr0$idbp)

# hist(icpBpHr0$hr)
# qqnorm(icpBpHr0$hr)
# qqline(icpBpHr0$hr)

# //ANCHOR - derive

icpBpHr1 <- icpBpHr0

icpBpHr1$mbp <- round(((icpBpHr1$isbp + 2 * icpBpHr1$idbp) / 3))
icpBpHr1$pp <- round((icpBpHr1$isbp - icpBpHr1$idbp))
icpBpHr1$cpp <- round((icpBpHr1$mbp - icpBpHr1$icp))
icpBpHr1$rpp <- round(icpBpHr1$isbp * icpBpHr1$hr)

# //ANCHOR - fill

# filling the missing data with n/a
icpBpHr2 <- icpBpHr1 %>%
    complete(
        icuid,
        interval = seq(0, 1439),
        fill = list(
            icp = NA,
            isbp = NA,
            idbp = NA,
            hr = NA,
            mbp = NA,
            pp = NA,
            cpp = NA,
            rpp = NA
        )
    )

print(sum(unique(icpBpHr2$icuid) %in% eicp$icuid))
print(sum(unique(icpBpHr2$icuid) %in% micp$icuid))

# //!SECTION

# //SECTION - sample

# //ANCHOR - avg

# avg of 4h
avg4H <- icpBpHr2 %>%
    mutate(interval = floor(interval / 48)) %>%
    group_by(icuid, interval) %>%
    summarize(
        icp = round(mean(icp, na.rm = TRUE), 2),
        isbp = round(mean(isbp, na.rm = TRUE), 2),
        idbp = round(mean(idbp, na.rm = TRUE), 2),
        hr = round(mean(hr, na.rm = TRUE), 2),
        mbp = round(mean(mbp, na.rm = TRUE), 2),
        pp = round(mean(pp, na.rm = TRUE), 2),
        cpp = round(mean(cpp, na.rm = TRUE), 2),
        rpp = round(mean(rpp, na.rm = TRUE), 2),
        .groups = "drop"
    )

# inclusion
avgDf <- avg4H %>%
    group_by(icuid) %>%
    filter(sum(is.na(icp)) <= 25) %>%
    filter(sum(is.na(isbp)) <= 25) %>%
    filter(sum(is.na(idbp)) <= 25) %>%
    filter(sum(is.na(hr)) <= 25) %>%
    as.data.frame()

colMeans(is.na(avg4H))

print(length(unique(avg4H$icuid)))
print(length(unique(avgDf$icuid)))

print(sum(unique(avg4H$icuid) %in% eicp$icuid))
print(sum(unique(avg4H$icuid) %in% micp$icuid))
print(sum(unique(avgDf$icuid) %in% eicp$icuid))
print(sum(unique(avgDf$icuid) %in% micp$icuid))

# //ANCHOR - imputation

library(missForest)

set.seed(0)

trajMf <- missForest(avgDf)

trajImp <- trajMf$ximp

# //!SECTION