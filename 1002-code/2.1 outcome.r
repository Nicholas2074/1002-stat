# //ANCHOR - hospMortality

# eicu
emortality <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/emortality.csv", header = TRUE)

names(emortality) <- c("icuid", "hospMortality")

# mimic
mmortality <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/mmortality.csv", header = TRUE)

names(mmortality) <- c("icuid", "hospMortality")

# combine
mortality <- rbind(emortality, mmortality)

# filling
mortality$hospMortality[is.na(mortality$hospMortality)] <- 0

# //ANCHOR - gcs

# eicu
egcs <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/edev_gcs.csv", header = TRUE)

names(egcs) <- c("icuid", "admgcs", "disgcs", "devgcs")

# mimic
mgcs <- read.csv("D:/Hai/321-stat/1002-stat/1002-oridata/mdev_gcs.csv", header = TRUE)

names(mgcs) <- c("icuid", "admgcs", "disgcs", "devgcs")

# combine
gcs <- rbind(mgcs, egcs)

# filling
gcs$disgcs[is.na(gcs$disgcs)] <- 0
# without missing data !!!

gcs$devgcs[is.na(gcs$devgcs)] <- 0
# without missing data !!!

# relabel
gcs$disgcs <- ifelse(gcs$disgcs <= 8, 1, 0)

gcs$devgcs <- ifelse(gcs$devgcs <= 0, 1, 0)

# //ANCHOR - link

# merge
trajMor <- merge(trajGroup, mortality, by = "icuid", all.x = TRUE)
trajDis <- merge(trajGroup, gcs[, c(1, 3)], by = "icuid", all.x = TRUE)
trajDev <- merge(trajGroup, gcs[, c(1, 4)], by = "icuid", all.x = TRUE)

print(length(unique(trajMor$icuid)))
print(length(unique(trajDis$icuid)))
print(length(unique(trajDev$icuid)))

any(is.na(trajMor))
any(is.na(trajDis))
any(is.na(trajDev))
