# //SECTION - multiglm

df1 <- merge(varsImpICD, mortality, by = "icuid", all.x = TRUE)
df2 <- merge(df1, gcs[, c(1, 3)], by = "icuid", all.x = TRUE)
df3 <- merge(df2, gcs[, c(1, 4)], by = "icuid", all.x = TRUE)

dfSubGroup <- df3[, c(
    "icuid",
    "hospMortality",
    "disgcs",
    "devgcs",
    "group",
    "age",
    "gender",
    "bmi",
    "gcs",
    "hypertension",
    "cerebrovascular_disease",
    "diabetes",
    "craniotomy",
    "ventriculostomy",
    "icd"
)]

subVars <- c(
    "icd",
    "age",
    "gender",
    "bmi",
    # "gcs",
    "hypertension",
    "cerebrovascular_disease",
    "diabetes",
    "craniotomy",
    "ventriculostomy"
)

# regroup
dfSubGroup$group <- ifelse(dfSubGroup$group == 5, 1, 0)

# factor
summary(dfSubGroup$age)

dfSubGroup <- dfSubGroup %>%
    mutate(
        age = case_when(
            age < 56 ~ 1,
            age < 71 ~ 2,
            TRUE ~ 3
        ),
        age = factor(
            age,
            levels = c(1, 2, 3),
            labels = c("< 56", "56 ~ 70", "≥ 71")
        )
    )

summary(dfSubGroup$bmi)

dfSubGroup <- dfSubGroup %>%
    mutate(bmi = case_when(
        bmi < 27.4 ~ 1,
        bmi < 31.2 ~ 2,
        TRUE ~ 3
    ),
    bmi = factor(
        bmi,
        levels = c(1, 2, 3),
        labels = c("< 27.4", "27.4 ~ 31.2", "≥ 31.2")
    )
    )

summary(dfSubGroup$gcs)

dfSubGroup <- dfSubGroup %>%
    mutate(gcs = factor(
        gcs,
        levels = c(1, 2),
        labels = c("≤ 8", "≥ 9")
    ))

dfSubGroup <- dfSubGroup %>%
    mutate(
        gender = factor(
        gender,
        levels = c(0, 1),
        labels = c("Male", "Female")
        ),
        hypertension = factor(
        hypertension,
        levels = c(1, 2),
        labels = c("NO", "YES")
        ),
        cerebrovascular_disease = factor(
        cerebrovascular_disease,
        levels = c("1", "2"),
        labels = c("NO", "YES")
        ),
        diabetes = factor(
        diabetes,
        levels = c(1, 2),
        labels = c("NO", "YES")
        ),
        craniotomy = factor(
        craniotomy,
        levels = c(1, 2),
        labels = c("NO", "YES")
        ),
        ventriculostomy = factor(
        ventriculostomy,
        levels = c(1, 2),
        labels = c("NO", "YES")
    ))

# //ANCHOR - hospMortality

# install.packages("jstable")

library(jstable)

jsMor <- TableSubgroupMultiGLM(
    formula = hospMortality ~ group,
    var_subgroups = subVars,
    data = dfSubGroup,
    family = "binomial"
)

print(jsMor)

jsMor <- jsMor[, c(
    "Variable", "Count", "OR", "Lower", "Upper", "P value", "P for interaction"
)]

jsMor[, 3:5] <- lapply(jsMor[, 3:5], as.numeric)

jsMor <- jsMor %>% 
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"))

jsMor$" " <- paste(rep(" ", nrow(jsMor)), collapse = " ")

colnames(jsMor) <- paste0(colnames(jsMor), "1")

jsMor[, c(2, 6, 7)][is.na(jsMor[, c(2, 6, 7)])] <- " "

# //ANCHOR - disgcs

library(jstable)

jsDis <- TableSubgroupMultiGLM(
    formula = disgcs ~ group,
    var_subgroups = subVars,
    data = dfSubGroup,
    family = "binomial"
)

print(jsDis)

jsDis <- jsDis[, c(
    "Variable", "Count", "OR", "Lower", "Upper", "P value", "P for interaction"
)]

jsDis[, 3:5] <- lapply(jsDis[, 3:5], as.numeric)

jsDis <- jsDis %>% 
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"))

jsDis$" " <- paste(rep(" ", nrow(jsDis)), collapse = " ")

colnames(jsDis) <- paste0(colnames(jsDis), "2")

jsDis[, c(2, 6, 7)][is.na(jsDis[, c(2, 6, 7)])] <- " "

# //ANCHOR - devgcs

library(jstable)

jsDev <- TableSubgroupMultiGLM(
    formula = devgcs ~ group,
    var_subgroups = subVars,
    data = dfSubGroup,
    family = "binomial"
)

print(jsDev)

jsDev <- jsDev[, c(
    "Variable", "Count", "OR", "Lower", "Upper", "P value", "P for interaction"
)]

jsDev[, 3:5] <- lapply(jsDev[, 3:5], as.numeric)

jsDev <- jsDev %>% 
    mutate("OR(95%CI)" = paste(OR, "(", Lower, ",", Upper, ")"))

jsDev$" " <- paste(rep(" ", nrow(jsDev)), collapse = " ")

colnames(jsDev) <- paste0(colnames(jsDev), "3")

jsDev[, c(2, 6, 7)][is.na(jsDev[, c(2, 6, 7)])] <- " "

# //ANCHOR - cbind

jsAll <- cbind(jsMor, jsDis, jsDev)

jsAll$" " <- paste(rep("NA", nrow(jsAll)))

jsAll[, 1]

jsAll <- jsAll %>%
    mutate(Variable1 = recode(Variable1,
        "age" = "Age",
        "gender" = "Gender",
        "bmi" = "BMI",
        "hypertension" = "Hypertension",
        "cerebrovascular_disease" = "Cerebrovascular Disease",
        "diabetes" = "Diabetes",
        "craniotomy" = "Craniotomy",
        "ventriculostomy" = "Ventriculostomy",
        "icd" = "Diagnosis",
        .default = Variable1
    ))

dim(jsAll)
names(jsAll)

# //ANCHOR - forestplot

library(forestploter)

jsForest <- forest(
    data = jsAll[, c(1, 2, 8, 9, 6, 7, 28, 17, 18, 15, 16, 28, 26, 27, 24, 25)],
    lower = list(
        jsAll$Lower1, 
        jsAll$Lower2,
        jsAll$Lower3
    ),
    upper = list(
        jsAll$Upper1, 
        jsAll$Upper2,
        jsAll$Upper3
    ),
    est = list(
        jsAll$OR1, 
        jsAll$OR2,
        jsAll$OR3
    ),
    ci_column = c(4, 9, 14),
    ref_line = 1,
    xlim = c(0, 5)
)

# Insert text at the top
library(grid)

jsForest <- insert_text(jsForest,
    text = c("In-hospital mortality", "Discharge GCS", "GCS difference"),
    col = c(4, 9, 14),
    part = "header",
    just = "center",
    gp = gpar(fontface = "bold")
)

# Add underline at the bottom of the header
jsForest <- add_border(jsForest, part = "header", row = 1, where = "top", gp = gpar(lwd = 1))
jsForest <- add_border(jsForest, part = "header", row = 2, where = "bottom", gp = gpar(lwd = 1))
jsForest <- add_border(jsForest, part = "header", row = 1, where = "bottom", col = c(3:6, 8:11, 13:16), gp = gpar(lwd = 0.5))

print(jsForest)

# //!SECTION