# //SECTION - glm

# //ANCHOR - hospMortality

hLogMorCrude <- glm(hospMortality ~ group, data = hTrajMor, family = binomial)

summary(hLogMorCrude)

hPMorCrude <- coef(summary(hLogMorCrude))[, "Pr(>|z|)"]

hORMorCrude <- exp(coef(hLogMorCrude))

hCIMorCrude <- exp(confint(hLogMorCrude))

print(hPMorCrude)
print(hORMorCrude)
print(hCIMorCrude)

# //ANCHOR - disgcs

hLogDisCrude <- glm(disgcs ~ group, data = hTrajDis, family = binomial)

summary(hLogDisCrude)

hPDisCrude <- coef(summary(hLogDisCrude))[, "Pr(>|z|)"]

hORDisCrude <- exp(coef(hLogDisCrude))

hCIDisCrude <- exp(confint(hLogDisCrude))

print(hPDisCrude)
print(hORDisCrude)
print(hCIDisCrude)

# //ANCHOR - devgcs

hLogDevCrude <- glm(devgcs ~ group, data = hTrajDev, family = binomial)

summary(hLogDevCrude)

hPDevCrude <- coef(summary(hLogDevCrude))[, "Pr(>|z|)"]

hORDevCrude <- exp(coef(hLogDevCrude))

hCIDevCrude <- exp(confint(hLogDevCrude))

print(hPDevCrude)
print(hORDevCrude)
print(hCIDevCrude)

# //!SECTION

# //SECTION - forestploter

# //ANCHOR - hospMortality

library(tidyverse)

hDfForestMorCrude <- data.frame(
    "Variable" = names(hPMorCrude),
    "P value" = hPMorCrude,
    "OR" = hORMorCrude,
    "Lower" = hCIMorCrude[, 1],
    "Upper" = hCIMorCrude[, 2],
    row.names = NULL
)

hDfForestMorCrude[, -1] <- round(hDfForestMorCrude[, -1], 3)

hDfForestMorCrude <- hDfForestMorCrude %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(hDfForestMorCrude)

hResMorCrude <- hDfForestMorCrude

hResMorCrude$" " <- paste(rep("    ", nrow(hResMorCrude)), collapse = " ")

colnames(hResMorCrude) <- paste0(colnames(hResMorCrude), "1")

dim(hResMorCrude)
print(hResMorCrude)

# //ANCHOR - disgcs

library(tidyverse)

hDfForestDisCrude <- data.frame(
    "Variable" = names(hPDisCrude),
    "P value" = hPDisCrude,
    "OR" = hORDisCrude,
    "Lower" = hCIDisCrude[, 1],
    "Upper" = hCIDisCrude[, 2],
    row.names = NULL
)

hDfForestDisCrude[, -1] <- round(hDfForestDisCrude[, -1], 3)

hDfForestDisCrude <- hDfForestDisCrude %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(hDfForestDisCrude)

hResDisCrude <- hDfForestDisCrude

hResDisCrude$" " <- paste(rep("    ", nrow(hResDisCrude)), collapse = " ")

colnames(hResDisCrude) <- paste0(colnames(hResDisCrude), "2")

dim(hResDisCrude)
print(hResDisCrude)

# //ANCHOR - devgcs

library(tidyverse)

hDfForestDevCrude <- data.frame(
    "Variable" = names(hPDevCrude),
    "P value" = hPDevCrude,
    "OR" = hORDevCrude,
    "Lower" = hCIDevCrude[, 1],
    "Upper" = hCIDevCrude[, 2],
    row.names = NULL
)

hDfForestDevCrude[, -1] <- round(hDfForestDevCrude[, -1], 3)

hDfForestDevCrude <- hDfForestDevCrude %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(hDfForestDevCrude)

hResDevCrude <- hDfForestDevCrude

hResDevCrude$" " <- paste(rep("    ", nrow(hResDevCrude)), collapse = " ")

colnames(hResDevCrude) <- paste0(colnames(hResDevCrude), "3")

dim(hResDevCrude)
print(hResDevCrude)

# //ANCHOR - cbind

hResAllCrude <- cbind(hResMorCrude, hResDisCrude, hResDevCrude)

hResAllCrude$" " <- paste(rep("NA", nrow(hResAllCrude)))

dim(hResAllCrude)
hResAllCrude[, 1]

# //ANCHOR - plot

library(forestploter)

hResFohRestCrude <- forest(
    data = hResAllCrude[, c(1, 2, 7, 6, 22, 9, 14, 13, 22, 16, 21, 20)],
    lower = list(
        hResAllCrude$Lower1, 
        hResAllCrude$Lower2,
        hResAllCrude$Lower3
    ),
    upper = list(
        hResAllCrude$Upper1, 
        hResAllCrude$Upper2,
        hResAllCrude$Upper3
    ),
    est = list(
        hResAllCrude$OR1, 
        hResAllCrude$OR2,
        hResAllCrude$OR3
    ),
    ci_column = c(3, 7, 11),
    ref_line = 1,
    xlim = c(0, 25)
)

# Insert text at the top
library(grid)

hResFohRestCrude <- insert_text(hResFohRestCrude,
    text = c("In-hospital mortality", "Discharge GCS", "GCS difference"),
    col = c(3, 7, 11),
    part = "header",
    just = "center",
    gp = gpar(fontface = "bold")
)

# Add underline at the bottom of the header
hResFohRestCrude <- add_border(hResFohRestCrude, part = "header", row = 1, where = "top", gp = gpar(lwd = 1))
hResFohRestCrude <- add_border(hResFohRestCrude, part = "header", row = 2, where = "bottom", gp = gpar(lwd = 1))
hResFohRestCrude <- add_border(hResFohRestCrude, part = "header", row = 1, where = "bottom", col = c(2:4, 6:8, 10:12), gp = gpar(lwd = 0.5))

print(hResFohRestCrude)

# //!SECTION