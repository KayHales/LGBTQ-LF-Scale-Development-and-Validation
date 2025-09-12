# Preparing Scale Items for Later Renaming
survey_q <- survey_questions(surveyID = "SV_cCHDLaZMmtKWNaS")
question_map <- setNames(survey_q$question, survey_q$qname)


# Packages 
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
tidyverse, 
lavaan,
lavaanPlot,   
semPlot, # path diagrams
semTools # reliability, AVE, etc.
)

# Optional Qualtrics Mapping
# library(qualtRics)
survey_q <- survey_questions(surveyID = "SV_cCHDLaZMmtKWNaS")
question_map <- setNames(survey_q$question, survey_q$qname)

# Data Prep
likert_levels <- c(
"Strongly disagree",
"Somewhat disagree",
"Neither agree nor disagree",
"Somewhat agree",
"Strongly agree"
)


valid_data <- data %>%
# Drop Q16 if present (no error if absent)
{
if ("Q16" %in% names(.)) select(., -Q16) else .
} %>%
# Keep only the fields we actually use for the CFA (demographics optional)
select(
# Demographics 
any_of(c("Gender", "Sexuality", "Age", "Party", "Educ", "Metro")),
# Items
Q14:Q26
) %>%
# Recode Likert responses to ordered 1–5 (preserve NAs)
mutate(across(Q14:Q26, ~ {
x <- factor(., levels = likert_levels, ordered = TRUE)
as.integer(x) # returns 1–5 with NA preserved
})) %>%
# Keep rows with at least one observed item (drop all-missing rows across items)
filter(if_any(Q14:Q26, ~ !is.na(.)))


# Quick NA check per item
na_counts <- valid_data %>% summarise(across(Q14:Q26, ~ sum(is.na(.))))
print(na_counts)

# Model Specification 
# Two-factor model with targeted residual covariances
model_cfa <- '
Belonging =~ Q14 + Q15 + Q21 + Q17 + Q18 + Q19 + Q20
Utility =~ Q22 + Q23 + Q24 + Q25 + Q26

# Factor variances and covariance 
Belonging ~~ Belonging
Utility ~~ Utility
Belonging ~~ Utility

# Residual correlations 
Q23 ~~ Q24
Q15 ~~ Q17
'

# Fit Model
fit <- cfa(
model = model_cfa,
data = valid_data,
std.lv = TRUE,
ordered = TRUE,
auto.fix.first = FALSE # free all loadings, fix latent variances via std.lv
)

# Inspect latent covariance matrix
print(lavInspect(fit, "cov.lv"))

# Fit summary with indices
summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# Modification Incdices
modificationindices(fit)
modificationindices(fit) %>%
  as_tibble() %>%
  arrange(-mi) %>%
  filter(mi > 4 ) %>%
  select(lhs, op, rhs, mi, epc) 

# Reliability
ave_vals <- semTools::AVE(fit) # AVE per factor
print(ave_vals)


# Visualizing CFA
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

# Distribution of factor scores 
fscores <- lavPredict(fit) %>% as.data.frame()

# Descriptives
print(psych::describe(fscores))

# Long format for ggplot
fs_long <- fscores %>%
pivot_longer(cols = everything(), names_to = "Factor", values_to = "Score")

# Density plots
fs_long %>%
ggplot(aes(x = Score)) +
geom_density(linewidth = 1) +
facet_wrap(~ Factor, scales = "free") +
theme_minimal(base_size = 14) +
theme(
strip.text = element_text(face = "bold", size = 14),
plot.title = element_text(hjust = 0.5, face = "bold"),
axis.title = element_text(size = 13),
axis.text = element_text(size = 11)
) +
labs(title = "Density Distributions of Factor Scores",
x = "Factor Score", y = "Density")

# Histograms (saved to file)
plt_hist <- fs_long %>%
ggplot(aes(x = Score)) +
geom_histogram(binwidth = 0.25, color = "white", boundary = 0) +
facet_wrap(~ Factor, scales = "free", ncol = 2) +
theme_minimal(base_size = 12) +
theme(
strip.text = element_text(size = 12, face = "bold"),
plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
axis.title = element_text(size = 12),
axis.text = element_text(size = 10),
panel.grid.major = element_blank(),
panel.grid.minor = element_blank()
) +
labs(title = "Marginal Distributions of Factor Scores",
x = "Factor Score", y = "Frequency")

print(plt_hist)

ggsave(filename = "factor_marginals.png", plot = plt_hist, width = 8, height = 5, dpi = 300)




