# Invariance Testing #
pacman::p_load(tidyverse, ggplot2, ggthemes, haven, lavaan, lavaanPlot, knitr, psych, 
               semPlot, semTools, wesanderson, psychTools)
# CFA with lavaan
valid_data <- data
valid_data <- valid_data %>%
  select(-Q16)
valid_data <- valid_data %>%
  mutate_at(vars(Q14:Q26), 
            ~factor(., levels = c("Strongly disagree", 
                                  "Somewhat disagree", 
                                  "Neither agree nor disagree", 
                                  "Somewhat agree", 
                                  "Strongly agree")))
valid_data <- valid_data %>%
  mutate(across(Q14:Q26, 
                ~ as.numeric(factor(., 
                                    levels = c("Strongly disagree", 
                                               "Somewhat disagree", 
                                               "Neither agree nor disagree", 
                                               "Somewhat agree", 
                                               "Strongly agree"),
                                    labels = c(1, 2, 3, 4, 5))
                )))
valid_data <- valid_data %>%
  filter(Gender2 != "Other")


# Configural Invariance #
# Model Syntax
model <- '
Belonging =~ Q14 + Q15 + Q21 + Q17 + Q18 + Q19 + Q20
Utility =~ Q22 + Q23 + Q24 + Q25 + Q26
Belonging ~~ Utility
Q23 ~~ Q24
Q15 ~~ Q17
'

# Run Model
fit_configural <- cfa(model = model, data = valid_data,
           std.lv = T, mimic = "Mplus", fixed.x = F,
           group = "Gender2", ordered = T)
getCov(fit)

# Check Goodness-of-Fit Indices
lavaan::summary(fit_configural, fit.measures = T, standardized = T)
lavaanPlot(fit_configural)

# Metric Invariance #
model <- '
Belonging =~ Q14 + Q15 + Q21 + Q17 + Q18 + Q19 + Q20
Utility =~ Q22 + Q23 + Q24 + Q25 + Q26
Belonging ~~ Utility
Q23 ~~ Q24
Q15 ~~ Q17
'

fit_metric <- cfa(model = model, data = valid_data,
                  std.lv = T, mimic = "Mplus", fixed.x = F, ordered = T,
                  group = "Gender2", group.equal = "loadings")
lavaan::summary(fit_metric, fit.measures = T, standardized = T)

lavTestLRT(fit_configural, fit_metric)
compare_cfa <- compareFit(fit_configural, fit_metric)
summary(compare_cfa)


# Scalar Invariance
model <- '
Belonging =~ Q14 + Q15 + Q21 + Q17 + Q18 + Q19 + Q20
Utility =~ Q22 + Q23 + Q24 + Q25 + Q26
Belonging ~~ Utility
Q23 ~~ Q24
Q15 ~~ Q17
'

fit_scalar <- cfa(model = model, data = valid_data,
                  std.lv = T, mimic = "Mplus", fixed.x = F, ordered = T,
                  group = "Gender2", group.equal = c("loadings", "thresholds"))
lavaan::summary(fit_scalar, fit.measures = T, standardized = T)
scalar_compare <- compareFit(fit_metric, fit_scalar)
summary(scalar_compare)

# Strict Invariance 
fit_strict <- measEq.syntax(configural.model = fit_configural,
                            ordered = T, ID.fac = "std.lv", ID.cat = "Wu",
                            group = "Gender2", group.equal = c("loadings", "thresholds"),
                            parameterization = "delta")