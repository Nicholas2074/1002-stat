# //SECTION - covariate

# //ANCHOR - hospMortality

library(tidyverse)

# subset
dfCovMor2 <- dfMor[, c(
    "icuid", 
    "hospMortality", 
    "group",
    "age", 
    "gender", 
    "bmi", 
    "hypertension", 
    "cerebrovascular_disease"
    )]

# //ANCHOR - disgcs

# subset
dfCovDis2 <- dfDis[, c(
    "icuid", 
    "disgcs", 
    "group",
    "age", 
    "gender", 
    "bmi", 
    "hypertension", 
    "cerebrovascular_disease"
    )]

# //ANCHOR - devgcs

# subset
dfCovDev2 <- dfDev[, c(
    "icuid", 
    "devgcs", 
    "group",
    "age", 
    "gender", 
    "bmi", 
    "hypertension", 
    "cerebrovascular_disease"
    )]

# //ANCHOR - pivot

dfCovMor2[, c(3, 5, 7, 8)] <- lapply(dfCovMor2[, c(3, 5, 7, 8)], as.character)

dfCovDis2[, c(3, 5, 7, 8)] <- lapply(dfCovDis2[, c(3, 5, 7, 8)], as.character)

dfCovDev2[, c(3, 5, 7, 8)] <- lapply(dfCovDev2[, c(3, 5, 7, 8)], as.character)

# //!SECTION

# //SECTION - glm

# //ANCHOR - hospMortality

# glm
print(names(dfCovMor2))

logMorAdjusted2 <- glm(
    hospMortality ~
        group +
        age +
        gender +
        bmi +
        hypertension + 
        cerebrovascular_disease,
    family = binomial,
    data = dfCovMor2
)

# summary
summary(logMorAdjusted2)

pMorAdjusted2 <- coef(summary(logMorAdjusted2))[, "Pr(>|z|)"]
pMorAdjusted2

orMorAdjusted2 <- exp(coef(logMorAdjusted2))
orMorAdjusted2

ciMorAdjusted2 <- exp(confint(logMorAdjusted2))
ciMorAdjusted2

# //ANCHOR - disgcs

# glm
print(names(dfCovDis2))

logDisAdjusted2 <- glm(
    disgcs ~
        group +
        age +
        gender +
        bmi +
        hypertension + 
        cerebrovascular_disease,
    family = binomial,
    data = dfCovDis2
)

# summary
summary(logDisAdjusted2)

pDisAdjusted2 <- coef(summary(logDisAdjusted2))[, "Pr(>|z|)"]
pDisAdjusted2

orDisAdjusted2 <- exp(coef(logDisAdjusted2))
orDisAdjusted2

ciDisAdjusted2 <- exp(confint(logDisAdjusted2))
ciDisAdjusted2

# //ANCHOR - devgcs

# glm
print(names(dfCovDev2))

logDevAdjusted2 <- glm(
    devgcs ~
        group +
        age +
        gender +
        bmi +
        hypertension + 
        cerebrovascular_disease,
    family = binomial,
    data = dfCovDev2
)

# summary
summary(logDevAdjusted2)

pDevAdjusted2 <- coef(summary(logDevAdjusted2))[, "Pr(>|z|)"]
pDevAdjusted2

orDevAdjusted2 <- exp(coef(logDevAdjusted2))
orDevAdjusted2

ciDevAdjusted2 <- exp(confint(logDevAdjusted2))
ciDevAdjusted2

# //!SECTION

# //SECTION - forestploter

# //ANCHOR - hospMortality

library(tidyverse)

dfForestMorAdjusted2 <- data.frame(
    "Variable" = names(pMorAdjusted2),
    "P value" = pMorAdjusted2,
    "OR" = orMorAdjusted2,
    "Lower" = ciMorAdjusted2[, 1],
    "Upper" = ciMorAdjusted2[, 2],
    row.names = NULL
)

dfForestMorAdjusted2[, -1] <- round(dfForestMorAdjusted2[, -1], 3)

dfForestMorAdjusted2 <- dfForestMorAdjusted2 %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(dfForestMorAdjusted2)

resMor2 <- dfForestMorAdjusted2

resMor2$" " <- paste(rep("    ", nrow(resMor2)), collapse = " ")

colnames(resMor2) <- paste0(colnames(resMor2), "1")

dim(resMor2)
print(resMor2)

# //ANCHOR - disgcs

library(tidyverse)

dfForestDisAdjusted2 <- data.frame(
    "Variable" = names(pDisAdjusted2),
    "P value" = pDisAdjusted2,
    "OR" = orDisAdjusted2,
    "Lower" = ciDisAdjusted2[, 1],
    "Upper" = ciDisAdjusted2[, 2],
    row.names = NULL
)

dfForestDisAdjusted2[, -1] <- round(dfForestDisAdjusted2[, -1], 3)

dfForestDisAdjusted2 <- dfForestDisAdjusted2 %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(dfForestDisAdjusted2)

resDis2 <- dfForestDisAdjusted2

resDis2$" " <- paste(rep("    ", nrow(resDis2)), collapse = " ")

colnames(resDis2) <- paste0(colnames(resDis2), "2")

dim(resDis2)
print(resDis2)

# //ANCHOR - devgcs

library(tidyverse)

dfForestDevAdjusted2 <- data.frame(
    "Variable" = names(pDevAdjusted2),
    "P value" = pDevAdjusted2,
    "OR" = orDevAdjusted2,
    "Lower" = ciDevAdjusted2[, 1],
    "Upper" = ciDevAdjusted2[, 2],
    row.names = NULL
)

dfForestDevAdjusted2[, -1] <- round(dfForestDevAdjusted2[, -1], 3)

dfForestDevAdjusted2 <- dfForestDevAdjusted2 %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(dfForestDevAdjusted2)

resDev2 <- dfForestDevAdjusted2

# Add a blank column for the forest plot to display CI
# Adjust the column width with space
resDev2$" " <- paste(rep("    ", nrow(resDev2)), collapse = " ")

colnames(resDev2) <- paste0(colnames(resDev2), "3")

dim(resDev2)
print(resDev2)

# //ANCHOR - cbind

resAll2 <- cbind(resMor2, resDis2, resDev2)

resAll2$" " <- paste(rep("NA", nrow(resAll2)))

resAll2 <- resAll2 %>%
    mutate(Variable1 = recode(Variable1,
        "group" = "Group",
        "age" = "Age",
        "gender1" = "Gender",
        "bmi" = "BMI",
        "hypertension2" = "Hypertension",
        "cerebrovascular_disease2" = "Cerebrovascular Disease",
        .default = Variable1
    ))

print(resAll2$Variable1)

resAll2 <- resAll2[-1, ]

# //ANCHOR - plot

library(forestploter)

resForest2 <- forest(
    data = resAll2[, c(1, 2, 7, 6, 22, 9, 14, 13, 22, 16, 21, 20)],
    lower = list(
        resAll2$Lower1, 
        resAll2$Lower2,
        resAll2$Lower3
    ),
    upper = list(
        resAll2$Upper1, 
        resAll2$Upper2,
        resAll2$Upper3
    ),
    est = list(
        resAll2$OR1, 
        resAll2$OR2,
        resAll2$OR3
    ),
    ci_column = c(3, 7, 11),
    ref_line = 1,
    xlim = c(0, 10)
)

# Insert text at the top
library(grid)

resForest2 <- insert_text(resForest2,
    text = c("In-hospital mortality", "Discharge GCS", "GCS difference"),
    col = c(3, 7, 11),
    part = "header",
    just = "center",
    gp = gpar(fontface = "bold")
)

# Add underline at the bottom of the header
resForest2 <- add_border(resForest2, part = "header", row = 1, where = "top", gp = gpar(lwd = 1))
resForest2 <- add_border(resForest2, part = "header", row = 2, where = "bottom", gp = gpar(lwd = 1))
resForest2 <- add_border(resForest2, part = "header", row = 1, where = "bottom", col = c(2:4, 6:8, 10:12), gp = gpar(lwd = 0.5))

print(resForest2)

# //ANCHOR - coplot

resAllCo <- rbind(resAll1, resAll2)

resAllCo[, 1] <- gsub(pattern = "group", replacement = "Cluster", x = resAllCo[, 1])

library(forestploter)

resForestCo <- forest(
    data = resAllCo[, c(1, 2, 7, 6, 22, 9, 14, 13, 22, 16, 21, 20)],
    lower = list(
        resAllCo$Lower1, 
        resAllCo$Lower2,
        resAllCo$Lower3
    ),
    upper = list(
        resAllCo$Upper1, 
        resAllCo$Upper2,
        resAllCo$Upper3
    ),
    est = list(
        resAllCo$OR1, 
        resAllCo$OR2,
        resAllCo$OR3
    ),
    ci_column = c(3, 7, 11),
    ref_line = 1,
    xlim = c(0, 10)
)

# Insert text at the top
library(grid)

resForestCo <- insert_text(resForestCo,
    text = c("In-hospital mortality", "Discharge GCS", "GCS difference"),
    col = c(3, 7, 11),
    part = "header",
    just = "center",
    gp = gpar(fontface = "bold")
)

resForestCoTwo <- insert_text(resForestCo,
    text = c("Adjusted model 1", "Adjusted model 2"),
    row = c(1, 8),
    col = 1,
    part = "body",
    just = "left",
    gp = gpar(fontface = "bold")
)

# Add underline at the bottom of the header
resForestCoTwo <- add_border(resForestCoTwo, part = "header", row = 1, where = "top", gp = gpar(lwd = 1))
resForestCoTwo <- add_border(resForestCoTwo, part = "header", row = 2, where = "bottom", gp = gpar(lwd = 1))
resForestCoTwo <- add_border(resForestCoTwo, part = "header", row = 1, where = "bottom", col = c(2:4, 6:8, 10:12), gp = gpar(lwd = 0.5))

print(resForestCoTwo)

# //!SECTION