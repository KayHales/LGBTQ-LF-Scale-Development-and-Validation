# Measurement Invariance by Race
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
  filter(Race != "Other") 

valid_data <- valid_data %>%
  mutate(Q14 = fct_collapse(Q14,
                            "Disagree" = c("Strongly disagree", "Somewhat disagree")))
valid_data <- valid_data %>%
  mutate(Q15 = fct_collapse(Q15,
                            "Disagree" = c("Strongly disagree", "Somewhat disagree")))
valid_data <- valid_data %>%
  mutate(Q17 = fct_collapse(Q17,
                            "Disagree" = c("Strongly disagree", "Somewhat disagree")))
valid_data <- valid_data %>%
  mutate(Q21 = fct_collapse(Q21,
                            "Disagree" = c("Strongly disagree", "Somewhat disagree")))


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
                      group = "Race", ordered = T, auto.fix.first = F)
summary(fit_configural, fit.measures = T, standardized = T)

fit_metric <- measEq.syntax(fit_configural, ID.fac = "std.lv", ID.cat = "Wu",
                            group = "Race", group.equal = "loadings",
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