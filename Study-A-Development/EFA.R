############################################################
# EFA with FIML correlations + FIML factor scores
# Goal: Show factor structure (EFA) and marginal distributions of factor scores 
############################################################

library(EFAtools)
library(psych)
library(lavaan)
library(dplyr)
library(ggplot2)
library(tidyr)

# --- Prepare data (run 'cleaning' before this step) ---
efa_raw <- lgbt_dat

# ---  Estimate FIML correlation matrix for EFA ---
R_fiml <- corFiml(efa_raw)

N_efa <- nrow(efa_raw)

# --- Factorability checks ---
det(R_fiml)
psych::KMO(R_fiml)
bart <- psych::cortest.bartlett(R_fiml, n = N_efa)
print(bart)


# --- 3. Run EFA with FIML correlations ---
# Example: 2 factors, ML extraction, oblimin rotation

efa_fit <- fa(R_fiml,
              nfactors = 2,
              rotate   = "oblimin",
              fm       = "ml",
              n.obs    = N_efa,
              warnings = TRUE)

# Inspect loadings & fit
print(efa_fit$loadings, cutoff = 0.30)
print(efa_fit$CFI)

# Optional: look at residual correlations (FIML-based)
resid_vec <- efa_fit$residual[lower.tri(efa_fit$residual, diag = FALSE)]
hist(resid_vec,
     main = "Residual correlations (FIML)",
     xlab = "Residual r")

# --- 4. Translate EFA -> CFA for factor scores ---
# Cleaning names for lavaan
belong_items <- c(" I feel closer to the LGBTQ community when I participate in LGBTQ groups.",
              " I feel closer to the LGBTQ community when I spend time with LGBTQ people.",
              " When something bad happens to me, other LGBTQ people will have my back.",
              "I feel a sense of belonging around other LGBTQ people.",
              " I feel connected to other members of the LGBTQ community, regardless of their other identities, beliefs, or experiences.",
              "My life is better when I spend time with other LGBTQ people.",
              "The LGBTQ community provides me with a sense of community I cannot get anywhere else.")

utility_items <- c(" I coordinate my political actions with other LGBQT people.",
              " I feel the need to engage in political action when LGBTQ people are attacked by politicians.",
              " I make a difference when I engage in political actions on behalf of the LGBTQ community.",
              " I try to convince other LGBTQ people to engage in political action.",
              " My LGBTQ friends keep me politically informed.")

# Remove spaces so lavaan will model data
lavaan_dat <- efa_raw |> janitor::clean_names()
belong_items  <- janitor::make_clean_names(belong_items)
utility_items <- janitor::make_clean_names(utility_items)

# CFA model
model_cfa <- paste0(
  "F1 =~ ", paste(belong_items,  collapse = " + "), "\n",
  "F2 =~ ", paste(utility_items, collapse = " + "), "\n",
  "F1 ~~ F2"
)

# --- Fit CFA with FIML for factor scores ---
fit_cfa <- cfa(model_cfa, data = lavaan_dat,
               estimator = "MLR", missing = "fiml")

# Extract case-level FIML factor scores
scores_fiml <- lavPredict(fit_cfa, method = "regression")
scores_df <- as.data.frame(scores_fiml)

# --- Summarize distributions ---
library(psych)
desc <- psych::describe(scores_df)
quant <- apply(scores_df, 2, quantile,
               probs = c(.10,.25,.50,.75,.90), na.rm = TRUE)

list(descriptives = desc[, c("mean","sd","skew","kurtosis","min","max")],
     quantiles = quant)

# --- Plot marginal distributions ---
scores_long <- scores_df |>
  pivot_longer(cols = everything(),
               names_to = "Factor", values_to = "Score")

p <- ggplot(scores_long, aes(x = Score)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30, fill = "gray30", color = "white") +
  geom_density(linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ Factor, scales = "free", ncol = 2) +
  labs(title = "Marginal Distributions of Linked Fate Factors (FIML)",
       subtitle = "Dashed line at 0",
       x = "Factor score", y = "Density") +
  theme_minimal(base_size = 12)
print(p)
ggsave("factor_marginals.png", p, width = 8, height = 4.5, dpi = 300)


