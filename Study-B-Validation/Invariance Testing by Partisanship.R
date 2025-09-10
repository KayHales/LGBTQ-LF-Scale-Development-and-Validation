# Invariance Testing by Partisanship 
pacman::p_load(tidyverse, ggplot2, ggthemes, haven, lavaan, lavaanPlot, knitr, psych, 
               semPlot, semTools, wesanderson, psychTools)
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
  mutate(Party = fct_recode(Party,
                            "Democrat" = "Strong Democrat",
                            "Democrat" = "Weak Democrat",
                            "Democrat" = "Independent Democrat",
                            "Independent" = "Independent",
                            "Republican" = "Independent Republican",
                            "Republican" = "Weak Republican",
                            "Republican" = "Strong Republican")) %>%
  # Drop levels that are not needed
  mutate(Party = fct_drop(Party))

valid_data <- valid_data %>%
  filter(Party != "I have a different affiliation or preference, please specify.") %>%
  mutate(Party = fct_drop(Party))

table(valid_data$Party)

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
                      group = "Party", ordered = T, auto.fix.first = F)
summary(fit_configural, fit.measures = T, standardized = T)

fit_metric <- measEq.syntax(fit_configural, ID.fac = "std.lv", ID.cat = "Wu",
                            group = "Party", group.equal = "loadings",
                            data = valid_data, ordered = T, mimic = "Mplus",
                            return.fit = T, auto.fix.first = F, fixed.x = F)
summary(fit_metric, fit.measures = T, standardized = T)
compare_cfa <- compareFit(fit_configural, fit_metric)
summary(compare_cfa)
lavTestLRT(fit_configural, fit_metric)

fit_scalar <- measEq.syntax(fit_configural, ID.fac = "std.lv", ID.cat = "Wu",
                            group = "Party", group.equal = c("loadings","thresholds"),
                            data = valid_data, ordered = T,
                            return.fit = T, auto.fix.first = F, fixed.x = F)
summary(fit_scalar, fit.measures = T, standardized = T)
compare_cfa <- compareFit(fit_metric, fit_scalar)
summary(compare_cfa)
lavTestLRT(fit_metric, fit_scalar)
# Strict Invariance
model <- '
Belonging =~ Q14 + Q15 + Q21 + Q17 + Q18 + Q19 + Q20
Utility =~ Q22 + Q23 + Q24 + Q25 + Q26
Belonging ~~ Utility
Q23 ~~ c(NA, NA, NA)*Q24
Q15 ~~ c(NA, NA, NA)*Q17
'
fit_strict <- measEq.syntax(model, ID.cat = "Wu",
                            group = "Sexuality", group.equal = c("loadings","thresholds","residuals"),
                            data = valid_data, ordered = T,
                            return.fit = T, auto.fix.first = F, fixed.x = F,
                            parameterization = "theta")
summary(fit_strict, fit.measures = T, standardized = T)
compare_cfa <- compareFit(fit_scalar, fit_strict)
summary(compare_cfa)