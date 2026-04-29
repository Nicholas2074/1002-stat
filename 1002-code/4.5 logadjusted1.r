# //SECTION - covariate

# //ANCHOR - hospMortality

library(tidyverse)

# subset
dfCovMor1 <- dfMor[, c(
    "icuid", 
    "hospMortality", 
    "group",
    "age", 
    "gender", 
    "bmi"
    )]

# //ANCHOR - disgcs

# subset
dfCovDis1 <- dfDis[, c(
    "icuid", 
    "disgcs", 
    "group",
    "age", 
    "gender", 
    "bmi"
    )]

# //ANCHOR - devgcs

# subset
dfCovDev1 <- dfDev[, c(
    "icuid", 
    "devgcs", 
    "group",
    "age", 
    "gender", 
    "bmi"
    )]

# //ANCHOR - pivot

dfCovMor1[, c(3, 5)] <- lapply(dfCovMor1[, c(3, 5)], as.character)

dfCovDis1[, c(3, 5)] <- lapply(dfCovDis1[, c(3, 5)], as.character)

dfCovDev1[, c(3, 5)] <- lapply(dfCovDev1[, c(3, 5)], as.character)

# //!SECTION

# //SECTION - glm

# //ANCHOR - hospMortality

# glm
print(names(dfCovMor1))

logMorAdjusted1 <- glm(
    hospMortality ~
        group +
        age +
        gender +
        bmi,
    family = binomial,
    data = dfCovMor1
)

# summary
summary(logMorAdjusted1)

pMorAdjusted1 <- coef(summary(logMorAdjusted1))[, "Pr(>|z|)"]
pMorAdjusted1

orMorAdjusted1 <- exp(coef(logMorAdjusted1))
orMorAdjusted1

ciMorAdjusted1 <- exp(confint(logMorAdjusted1))
ciMorAdjusted1

# //ANCHOR - disgcs

# glm
print(names(dfCovDis1))

logDisAdjusted1 <- glm(
    disgcs ~
        group +
        age +
        gender +
        bmi,
    family = binomial,
    data = dfCovDis1
)

# summary
summary(logDisAdjusted1)

pDisAdjusted1 <- coef(summary(logDisAdjusted1))[, "Pr(>|z|)"]
pDisAdjusted1

orDisAdjusted1 <- exp(coef(logDisAdjusted1))
orDisAdjusted1

ciDisAdjusted1 <- exp(confint(logDisAdjusted1))
ciDisAdjusted1

# //ANCHOR - devgcs

# glm
print(names(dfCovDev1))

logDevAdjusted1 <- glm(
    devgcs ~
        group +
        age +
        gender +
        bmi,
    family = binomial,
    data = dfCovDev1
)

# summary
summary(logDevAdjusted1)

pDevAdjusted1 <- coef(summary(logDevAdjusted1))[, "Pr(>|z|)"]
pDevAdjusted1

orDevAdjusted1 <- exp(coef(logDevAdjusted1))
orDevAdjusted1

ciDevAdjusted1 <- exp(confint(logDevAdjusted1))
ciDevAdjusted1

# //!SECTION

# //SECTION - forestploter

# //ANCHOR - hospMortality

library(tidyverse)

dfForestMorAdjusted1 <- data.frame(
    "Variable" = names(pMorAdjusted1),
    "P value" = pMorAdjusted1,
    "OR" = orMorAdjusted1,
    "Lower" = ciMorAdjusted1[, 1],
    "Upper" = ciMorAdjusted1[, 2],
    row.names = NULL
)

dfForestMorAdjusted1[, -1] <- round(dfForestMorAdjusted1[, -1], 3)

dfForestMorAdjusted1 <- dfForestMorAdjusted1 %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(dfForestMorAdjusted1)

resMor1 <- dfForestMorAdjusted1

resMor1$" " <- paste(rep("    ", nrow(resMor1)), collapse = " ")

colnames(resMor1) <- paste0(colnames(resMor1), "1")

dim(resMor1)
print(resMor1)

# //ANCHOR - disgcs

library(tidyverse)

dfForestDisAdjusted1 <- data.frame(
    "Variable" = names(pDisAdjusted1),
    "P value" = pDisAdjusted1,
    "OR" = orDisAdjusted1,
    "Lower" = ciDisAdjusted1[, 1],
    "Upper" = ciDisAdjusted1[, 2],
    row.names = NULL
)

dfForestDisAdjusted1[, -1] <- round(dfForestDisAdjusted1[, -1], 3)

dfForestDisAdjusted1 <- dfForestDisAdjusted1 %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(dfForestDisAdjusted1)

resDis1 <- dfForestDisAdjusted1

resDis1$" " <- paste(rep("    ", nrow(resDis1)), collapse = " ")

colnames(resDis1) <- paste0(colnames(resDis1), "2")

dim(resDis1)
print(resDis1)

# //ANCHOR - devgcs

library(tidyverse)

dfForestDevAdjusted1 <- data.frame(
    "Variable" = names(pDevAdjusted1),
    "P value" = pDevAdjusted1,
    "OR" = orDevAdjusted1,
    "Lower" = ciDevAdjusted1[, 1],
    "Upper" = ciDevAdjusted1[, 2],
    row.names = NULL
)

dfForestDevAdjusted1[, -1] <- round(dfForestDevAdjusted1[, -1], 3)

dfForestDevAdjusted1 <- dfForestDevAdjusted1 %>%
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"), )

print(dfForestDevAdjusted1)

resDev1 <- dfForestDevAdjusted1

# Add a blank column for the forest plot to display CI
# Adjust the column width with space
resDev1$" " <- paste(rep("    ", nrow(resDev1)), collapse = " ")

colnames(resDev1) <- paste0(colnames(resDev1), "3")

dim(resDev1)
print(resDev1)

# //ANCHOR - cbind

resAll1 <- cbind(resMor1, resDis1, resDev1)

resAll1$" " <- paste(rep("NA", nrow(resAll1)))

resAll1 <- resAll1 %>%
    mutate(Variable1 = recode(Variable1,
        "group" = "Group",
        "age" = "Age",
        "gender1" = "Gender",
        "bmi" = "BMI",
        "hypertension" = "Hypertension",
        "cerebrovascular_disease" = "Cerebrovascular Disease",
        .default = Variable1
    ))

resAll1 <- resAll1[-1, ]

# //ANCHOR - plot

library(forestploter)

resForest1 <- forest(
    data = resAll1[, c(1, 2, 7, 6, 22, 9, 14, 13, 22, 16, 21, 20)],
    lower = list(
        resAll1$Lower1, 
        resAll1$Lower2,
        resAll1$Lower3
    ),
    upper = list(
        resAll1$Upper1, 
        resAll1$Upper2,
        resAll1$Upper3
    ),
    est = list(
        resAll1$OR1, 
        resAll1$OR2,
        resAll1$OR3
    ),
    ci_column = c(3, 7, 11),
    ref_line = 1,
    xlim = c(0, 10)
)

# Insert text at the top
library(grid)

resForest1 <- insert_text(resForest1,
    text = c("In-hospital mortality", "Discharge GCS", "GCS difference"),
    col = c(3, 7, 11),
    part = "header",
    just = "center",
    gp = gpar(fontface = "bold")
)

# Add underline at the bottom of the header
resForest1 <- add_border(resForest1, part = "header", row = 1, where = "top", gp = gpar(lwd = 1))
resForest1 <- add_border(resForest1, part = "header", row = 2, where = "bottom", gp = gpar(lwd = 1))
resForest1 <- add_border(resForest1, part = "header", row = 1, where = "bottom", col = c(2:4, 6:8, 10:12), gp = gpar(lwd = 0.5))

print(resForest1)

# //!SECTION