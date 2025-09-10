# Predictive Validity # 
# install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, ggthemes, haven, lavaan, lavaanPlot, knitr, psych, 
               semPlot, semTools, wesanderson, psychTools)
# Data Preparation
valid_data <- data
valid_data <- valid_data %>%
  select(-Q16)
valid_data$Educ <- as.numeric(valid_data$Educ)
valid_data <- valid_data %>%
  mutate(Age = as.numeric(Age))
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
  filter(if_any(Q14:Q26, ~ !is.na(.)))
valid_data <- valid_data %>%
  mutate(Q31 = case_when(
    Q31 == "I do not wish to respond" ~ NA_character_,
    TRUE ~ as.character(Q31)
  ))
valid_data$Q31 <- as.numeric(valid_data$Q31)
valid_data <- valid_data %>%
  mutate_at(vars(Q40), ~na_if(., "I do not wish to respond"))
valid_data$Q40 <- droplevels(valid_data$Q40)
valid_data <- valid_data %>%
  mutate_at(vars(Q40), ~na_if(., "Never"))
valid_data$Q40 <- droplevels(valid_data$Q40)
valid_data <- valid_data %>%
  mutate_at(vars(Q33), ~na_if(., "I do not wish to respond"))
valid_data$Q33 <- droplevels(valid_data$Q33)
valid_data <- valid_data %>%
  mutate_at(vars(Q34), ~na_if(., "I do not wish to respond"))
valid_data$Q34 <- droplevels(valid_data$Q34)
valid_data <- valid_data %>%
  mutate(across(starts_with("Q30_"), 
                ~ factor(., 
                         levels = c("Strongly disagree", 
                                    "Somewhat disagree", 
                                    "Neither agree nor disagree", 
                                    "Somewhat agree", 
                                    "Strongly agree"),
                         ordered = TRUE) %>%
                  droplevels() %>%
                  fct_drop("I do not wish to respond")))
valid_data <- valid_data %>%
  mutate(Party = fct_recode(Party,
                            "Democrat" = "Strong Democrat",
                            "Democrat" = "Weak Democrat",
                            "Democrat" = "Independent Democrat",
                            "Independent" = "Independent",
                            "Republican" = "Independent Republican",
                            "Republican" = "Weak Republican",
                            "Republican" = "Strong Republican")) %>%
  droplevels() 
valid_data <- valid_data %>%
  mutate(Q37 = na_if(Q37, "I do not wish to respond")) %>%
  droplevels()
valid_data <- valid_data %>%
  mutate(Q38 = na_if(Q38, "I do not wish to respond")) %>%
  droplevels()
# Other Scales 
ordered_levels <- c("Disagree strongly", "Disagree", "Neither agree nor disagree", 
                    "Agree", "Agree stronly")
valid_data <- valid_data %>%
  mutate(across(starts_with("Q28_"),
                ~ factor(.x, levels = ordered_levels, ordered = TRUE)))

ordered_levels <- c("Strongly disagree", "Disagree", "Neither agree nor disagree", 
                    "Agree", "Strongly agree")
valid_data <- valid_data %>%
  mutate(across(starts_with("Q29_"),
                ~ factor(.x, levels = ordered_levels, ordered = TRUE)))
valid_data$Q34 <- factor(valid_data$Q34, 
                         levels = c("Strongly oppose", "Oppose", "Neither favor nor oppose", 
                                    "Favor", "Strongly favor"), 
                         ordered = TRUE)

# Race, Gender, and Sexuality Dummies
valid_data$Gender2 <- relevel(valid_data$Gender2, ref = "Other")
valid_data$Sexuality <- relevel(valid_data$Sexuality, ref = "Other")
valid_data$Race <- relevel(valid_data$Race, ref = "Other")
gender_dummies <- model.matrix(~ Gender2 - 1, data = valid_data)
colnames(gender_dummies) <- gsub("Gender2", "Gender2", colnames(gender_dummies))
sexuality_dummies <- model.matrix(~ Sexuality - 1, data = valid_data)
colnames(sexuality_dummies) <- gsub("Sexuality", "Sexuality", colnames(sexuality_dummies))
race_dummies <- model.matrix(~ Race - 1, data = valid_data)
colnames(race_dummies) <- gsub("Race", "Race", colnames(race_dummies))

# Bind these dummy columns to the original dataframe
valid_data <- cbind(valid_data, gender_dummies, sexuality_dummies, race_dummies)
colnames(valid_data)[colnames(valid_data) == "RaceAsian American"] <- "RaceAsianAmerican"

# Optionally, remove the original factor columns if not needed
valid_data$Gender2 <- NULL
valid_data$Sexuality <- NULL
valid_data$Gender2_dummy <- NULL
valid_data$Sexuality_dummy <- NULL
valid_data$Race <- NULL


model <- '
#---------------- Latent Constructs ----------------#

Belonging =~ Q14 + Q15 + Q21 + Q17 + Q18 + Q19 + Q20
Utility   =~ Q22 + Q23 + Q24 + Q25 + Q26

Closeness       =~ Q28_1 + Q28_3
Positivity      =~ Q28_2 + Q28_4
Problem_Focused =~ Q28_5 + Q28_6 + Q28_7
CC              =~ Closeness + Positivity + Problem_Focused

#---------------- Phantom Residuals ----------------#

E1 =~ lambda1*Belonging
Belonging ~~ 0*Belonging
E1 ~~ 1*E1

E2 =~ lambda2*Utility
Utility ~~ 0*Utility
E2 ~~ 1*E2

E3 =~ lambda3*CC
CC ~~ 0*CC
E3 ~~ 1*E3

#---------------- Shared Composite ----------------#

omega_BU =~ lambdaB*Belonging + lambdaU*Utility
omega_BU ~~ 0*omega_BU
omega_BU ~ d11*E1 + d12*E2
E1 ~~ phi1*E2

#---------------- Orthogonalize Residuals ----------------#

E1 ~~ 0*E2 + 0*E3
E2 ~~ 0*E3
E3 ~~ 0*omega_BU

#---------------- Outcome Model ----------------#
BACA1 =~ lambdaB1*Q30_1 + NA*Q30_1
Q30_1 ~~ 0*Q30_1
BACA1 ~~ zetaB1*BACA1
BACA1 ~ gamma1_1*E1 + gamma2_1*E2 + gamma3_1*omega_BU + gamma4_1*E3 + gamma5_1*Educ
1 == zetaB1 + gamma1_1^2 + gamma2_1^2 + gamma3_1^2 + gamma4_1^2 + gamma5_1^2

#---------------- Constraints ----------------#

# Standardize omega_BU (phantom latent composite)
1 == d11^2 + d12^2 + 2*d11*d12*phi1

# Enforce non-negative loadings
lambda1 > 0
lambda2 > 0
lambda3 > 0
lambdaB > 0
lambdaU > 0
lambdaT > 0

#---------------- R² Definitions ----------------#

RBelonging_1 := gamma1_1^2
RUtility_1   := gamma2_1^2
RLF_1        := gamma3_1^2
RCC_1        := gamma4_1^2
REduc_1      := gamma5_1^2


'


fit <- lavaan(model, data = valid_data, se = "bootstrap", bootstrap = 100,
              estimator = "DWLS")

summary(fit)
fitMeasures(fit)
standardizedSolution(fit, type = "std.all", level = 0.95)

parameterEstimates(fit, level = 0.95)

# BACA Model

model2 <- '
#---------------- Measurement Model ----------------#

# Belonging latent factor
Belonging =~ Q14 + Q15 + Q21 + Q17 + Q18 + Q19 + Q20

# Utility latent factor
Utility =~ Q22 + Q23 + Q24 + Q25 + Q26

# Connectedness Composite
CC =~ Closeness + Positivity + Problem_Focused

Closeness       =~ Q28_1 + Q28_3
Positivity      =~ Q28_2 + Q28_4
Problem_Focused =~ Q28_5 + Q28_6 + Q28_7

# Phantom latent residuals
E1 =~ lambda1*Belonging
Belonging ~~ 0*Belonging
E1 ~~ 1*E1

E2 =~ lambda2*Utility
Utility ~~ 0*Utility
E2 ~~ 1*E2

E3 =~ lambda3*CC
CC ~~ 0*CC
E3 ~~ 1*E3

# Orthogonal residuals
E1 ~~ 0*E2 + 0*E3
E2 ~~ 0*E3

# Latent outcome: Normative Behaviors
eta_NormBeh =~ Q30_1 + Q30_2 + Q30_3 + Q30_4 + Q30_5 + Q30_6

# Standardize latent outcome variance
eta_NormBeh ~~ zetaNB*eta_NormBeh
zetaNB == 1 - (gamma1^2 + gamma2^2 + gamma3^2 + gamma4^2)

# Outcome regression
eta_NormBeh ~ gamma1*E1 + gamma2*E2 + gamma3*E3 + gamma4*Educ

# R-squared partitions
RBelonging := gamma1^2
RUtility   := gamma2^2
RCC        := gamma3^2
REduc      := gamma4^2
'

fit2 <- lavaan(model2, valid_data)
summary(fit2)
standardizedSolution(fit2, type = "std.all", level = 0.95)

