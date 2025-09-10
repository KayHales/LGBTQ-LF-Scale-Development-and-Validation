# Preparing Scale Items for Later Renaming
survey_q <- survey_questions(surveyID = "SV_cCHDLaZMmtKWNaS")
question_map <- setNames(survey_q$question, survey_q$qname)


# Packages 
library(lavaan)
library(psych)
library(psychTools)
pacman::p_load(tidyverse, ggplot2, ggthemes, haven, lavaan, lavaanPlot, knitr, psych, 
               semPlot, semTools, wesanderson, psychTools)
pacman::p_load(dplyr)

# CFA with lavaan
cfa_data <- data %>%
  select(
    Gender,
    Sexuality,
    Age, 
    Party,
    Educ,
    Metro,
    Q14,
    Q15,
    Q17,
    Q18,
    Q19,
    Q20,
    Q21,
    Q22,
    Q23,
    Q24,
    Q25,
    Q26
  )
cfa_data <- cfa_data %>%
  mutate_at(vars(Q14:Q26), 
            ~factor(., levels = c("Strongly disagree", 
                                  "Somewhat disagree", 
                                  "Neither agree nor disagree", 
                                  "Somewhat agree", 
                                  "Strongly agree")))
cfa_data <- cfa_data %>%
  mutate(across(Q14:Q26, 
                ~ as.numeric(factor(., 
                                    levels = c("Strongly disagree", 
                                               "Somewhat disagree", 
                                               "Neither agree nor disagree", 
                                               "Somewhat agree", 
                                               "Strongly agree"),
                                    labels = c(1, 2, 3, 4, 5))
                )))
cfa_data %>%
  summarise(across(Q14:Q26, ~ sum(is.na(.))))
cfa_data <- cfa_data %>%
  filter(if_any(Q14:Q26, ~ !is.na(.)))
# Model Syntax
model <- '
Belonging =~ Q14 + Q15 + Q21 + Q17 + Q18 + Q19 + Q20
Utility =~ Q22 + Q23 + Q24 + Q25 + Q26
Belonging ~~ Belonging 
Utility ~~ Utility
Belonging ~~ Utility

Q23 ~~ Q24
Q15 ~~ Q17
'

# Run Model
fit <- cfa(model = model, data = valid_data,
           std.lv = TRUE, ordered = T, auto.fix.first = F)
getCov(fit)

# Check Goodness-of-Fit Indices
summary(fit, rsquare = T)

# Potential Modifications
modificationindices(fit)
modificationindices(fit) %>%
  as_tibble() %>%
  arrange(-mi) %>%
  filter(mi > 4 ) %>%
  select(lhs, op, rhs, mi, epc) 

# Reliability
semTools::AVE(fit)


# Visualizing CFA
semPaths(fit, what = "paths", intercepts = F,
          layout = "tree2", sizeLat2 = 10,
         width = 6, height = 2, label.cex = 1, nCharNodes = 0, curve = 2.5,
         label.scale = T, whatLabels = "est")
semPaths(fit, intercepts = F, layout = "tree2",
         style = "ram", residuals = F)

# Plot Diagram
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
model <- '
Belonging =~ Q14 + Q15 + Q21 + Q17 + Q18 + Q19 + Q20
Utility =~ Q22 + Q23 + Q25 + Q24 + Q26


Q34 ~ Utility
'

fit <- cfa(model = model, data = valid_data, std.lv = TRUE, 
           auto.fix.first = F, fixed.x = F, mimic = "Mplus")
lavInspect(fit, "cov.lv")
summary(fit, fit.measure = TRUE, standardized = TRUE)
modificationindices(fit) %>%
  as_tibble() %>%
  arrange(-mi) %>%
  filter(mi > 6 ) %>%
  select(lhs, op, rhs, mi, epc) 
semPaths(fit, 
         whatLabels = "est", 
         layout = "tree", 
         style = "ram",
         rotation = 2, 
         residuals = TRUE, 
         edge.label.cex = 0.6,  # Increased edge label size
         edge.label.color = "black",  # Changed edge label color
         edge.width = 2,  # Increased edge line width
         nCharNodes = 0, 
         sizeMan = 6,  # Size of manifest variable boxes
         sizeLat = 10,  # Size of latent variable boxes
         mar = c(5, 5, 5, 5),  # Increased margins
         thresholds = FALSE,
         intercepts = FALSE,
         measurementLayout = TRUE
)
semPaths(fit, 
         whatLabels = "std", 
         what = "path",
         layout = "tree", 
         style = "ram", 
         rotation = 2, 
         residuals = TRUE, 
         nCharNodes = 0, 
         sizeMan = 5,  
         sizeLat = 10, 
         thresholds = FALSE,
         intercepts = FALSE,
         curve = 1.6,
         edge.label.cex = .6,
         reorder = FALSE,
)
title("LGBT-LF CFA Path Diagram")

lavaanPlot2(
  fit,
  include = "covs",
  coef_labels = TRUE
)

# Distribution of factor scores 
library(ggplot2)
model <- '
Belonging =~ Q14 + Q15 + Q21 + Q17 + Q18 + Q19 + Q20
Utility =~ Q22 + Q23 + Q24 + Q25 + Q26
Belonging ~~ Belonging 
Utility ~~ Utility
Belonging ~~ Utility

Q23 ~~ Q24
Q15 ~~ Q17
'

# Run Model
fit <- cfa(model = model, data = valid_data,
           std.lv = TRUE, ordered = T, auto.fix.first = F)

# Extract and Clean Scores
factor_scores <- lavPredict(fit)
factor_scores_long <- factor_scores %>% 
  as.data.frame() %>% 
  pivot_longer(cols = everything(), names_to = "Factor", values_to = "Score")

# Summaries
describe(factor_scores)

# Plot
ggplot(factor_scores_long, aes(x = Score)) +
  geom_density(color = "navy", linewidth = 1) +
  facet_wrap(~ Factor, scales = "free") +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11)
  ) +
  labs(
    title = "Density Distributions of Factor Scores",
    x = "Factor Score",
    y = "Density"
  )

## Marginal Distribution
factor_scores <- lavPredict(fit)
factor_scores_long <- factor_scores %>% 
  as.data.frame() %>% 
  pivot_longer(cols = everything(), names_to = "Factor", values_to = "Score")

ggplot(factor_scores_long, aes(x = Score)) +
  geom_histogram(binwidth = 0.25, fill = "gray30", color = "white", boundary = 0) +
  facet_wrap(~ Factor, scales = "free", ncol = 2) +
  theme_minimal(base_size = 12, base_family = "Times New Roman") +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "Marginal Distributions of Factor Scores",
    x = "Factor Score",
    y = "Frequency"
  )
ggsave("marginal.png")



