# //SECTION - glm

# //ANCHOR - hospMortality

logMorCrude <- glm(hospMortality ~ group, data = trajMor, family = binomial)

summary(logMorCrude)

pMorCrude <- coef(summary(logMorCrude))[, "Pr(>|z|)"]

orMorCrude <- exp(coef(logMorCrude))

ciMorCrude <- exp(confint(logMorCrude))

print(pMorCrude)
print(orMorCrude)
print(ciMorCrude)

# //ANCHOR - disgcs

logDisCrude <- glm(disgcs ~ group, data = trajDis, family = binomial)

summary(logDisCrude)

pDisCrude <- coef(summary(logDisCrude))[, "Pr(>|z|)"]

orDisCrude <- exp(coef(logDisCrude))

ciDisCrude <- exp(confint(logDisCrude))

print(pDisCrude)
print(orDisCrude)
print(ciDisCrude)

# //ANCHOR - devgcs

logDevCrude <- glm(devgcs ~ group, data = trajDev, family = binomial)

summary(logDevCrude)

pDevCrude <- coef(summary(logDevCrude))[, "Pr(>|z|)"]

orDevCrude <- exp(coef(logDevCrude))

ciDevCrude <- exp(confint(logDevCrude))

print(pDevCrude)
print(orDevCrude)
print(ciDevCrude)

# //!SECTION

# //SECTION - forestploter

# //ANCHOR - hospMortality

library(tidyverse)

dfForestMorCrude <- data.frame(
    "Variable" = names(pMorCrude),
    "P value" = pMorCrude,
    "OR" = orMorCrude,
    "Lower" = ciMorCrude[, 1],
    "Upper" = ciMorCrude[, 2],
    row.names = NULL
)

dfForestMorCrude[, -1] <- round(dfForestMorCrude[, -1], 3)

dfForestMorCrude <- dfForestMorCrude %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(dfForestMorCrude)

resMorCrude <- dfForestMorCrude

resMorCrude$" " <- paste(rep("    ", nrow(resMorCrude)), collapse = " ")

colnames(resMorCrude) <- paste0(colnames(resMorCrude), "1")

dim(resMorCrude)
print(resMorCrude)

# //ANCHOR - disgcs

library(tidyverse)

dfForestDisCrude <- data.frame(
    "Variable" = names(pDisCrude),
    "P value" = pDisCrude,
    "OR" = orDisCrude,
    "Lower" = ciDisCrude[, 1],
    "Upper" = ciDisCrude[, 2],
    row.names = NULL
)

dfForestDisCrude[, -1] <- round(dfForestDisCrude[, -1], 3)

dfForestDisCrude <- dfForestDisCrude %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(dfForestDisCrude)

resDisCrude <- dfForestDisCrude

resDisCrude$" " <- paste(rep("    ", nrow(resDisCrude)), collapse = " ")

colnames(resDisCrude) <- paste0(colnames(resDisCrude), "2")

dim(resDisCrude)
print(resDisCrude)

# //ANCHOR - devgcs

library(tidyverse)

dfForestDevCrude <- data.frame(
    "Variable" = names(pDevCrude),
    "P value" = pDevCrude,
    "OR" = orDevCrude,
    "Lower" = ciDevCrude[, 1],
    "Upper" = ciDevCrude[, 2],
    row.names = NULL
)

dfForestDevCrude[, -1] <- round(dfForestDevCrude[, -1], 3)

dfForestDevCrude <- dfForestDevCrude %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(dfForestDevCrude)

resDevCrude <- dfForestDevCrude

resDevCrude$" " <- paste(rep("    ", nrow(resDevCrude)), collapse = " ")

colnames(resDevCrude) <- paste0(colnames(resDevCrude), "3")

dim(resDevCrude)
print(resDevCrude)

# //ANCHOR - cbind

resAllCrude <- cbind(resMorCrude, resDisCrude, resDevCrude)

resAllCrude$" " <- paste(rep("NA", nrow(resAllCrude)))

dim(resAllCrude)
resAllCrude[, 1]

# //ANCHOR - plot

library(forestploter)

resForestCrude <- forest(
    data = resAllCrude[, c(1, 2, 7, 6, 22, 9, 14, 13, 22, 16, 21, 20)],
    lower = list(
        resAllCrude$Lower1, 
        resAllCrude$Lower2,
        resAllCrude$Lower3
    ),
    upper = list(
        resAllCrude$Upper1, 
        resAllCrude$Upper2,
        resAllCrude$Upper3
    ),
    est = list(
        resAllCrude$OR1, 
        resAllCrude$OR2,
        resAllCrude$OR3
    ),
    ci_column = c(3, 7, 11),
    ref_line = 1,
    xlim = c(0, 10)
)

# Insert text at the top
library(grid)

resForestCrude <- insert_text(resForestCrude,
    text = c("In-hospital mortality", "Discharge GCS", "GCS difference"),
    col = c(3, 7, 11),
    part = "header",
    just = "center",
    gp = gpar(fontface = "bold")
)

# Add underline at the bottom of the header
resForestCrude <- add_border(resForestCrude, part = "header", row = 1, where = "top", gp = gpar(lwd = 1))
resForestCrude <- add_border(resForestCrude, part = "header", row = 2, where = "bottom", gp = gpar(lwd = 1))
resForestCrude <- add_border(resForestCrude, part = "header", row = 1, where = "bottom", col = c(2:4, 6:8, 10:12), gp = gpar(lwd = 0.5))

print(resForestCrude)

# //!SECTION