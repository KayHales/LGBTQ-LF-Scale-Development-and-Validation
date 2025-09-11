############################################################
# Project: Study A of Scale Development and Validation of the LGBTQ-LF
# Author: Kay Hales, Ellen Riggle
# Notes:
#  - Minimal, reproducible package set
#  - Recoding and Cleaning
#  - Outlier detection on raw rows (before correlations/EFA)
############################################################

# ---------- Packages ----------
# pacman convenient for loading/auto-installing
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  dplyr, tidyr, stringr, tibble, haven,
  sjlabelled, psych, EFAtools, GPArotation, psychTools,
  qualtRics, flextable
)

# ---------- Credentials (read from .Renviron) ----------
readRenviron("~/.Renviron")

qualtrics_api_credentials(
  api_key  = Sys.getenv("QUALTRICS_API_KEY"),
  base_url = Sys.getenv("QUALTRICS_BASE_URL"),
  install  = FALSE
)

# ---------- Data ingest ----------
surveys <- all_surveys()
# Tip: choose by Name or ID explicitly to avoid accidental [1]
# e.g., surveys |> filter(name == "Your Survey Name") |> pull(id)
survey_id <- surveys$id[1]

raw <- fetch_survey(
  surveyID            = survey_id,
  verbose             = TRUE,
  include_metadata    = NA,
  include_display_order = FALSE
)

# Work on a copy
dat <- raw


# ---------- Helper utilities ----------
# Convert specific Qualtrics checkbox column to 1/NA if it matches the target label
recode_checkbox <- function(x, target_label) ifelse(x == target_label, 1, NA_real_)

# Reverse-score Likert items coded 1..7 to keep "higher = more of construct"
reverse_1_to_7 <- function(x) ifelse(is.na(x), NA_real_, 8 - as.numeric(x))

# Drop columns by exact prefixes safely
drop_prefix <- function(.data, prefixes) {
  sel <- purrr::map(prefixes, ~ dplyr::starts_with(.x))
  dplyr::select(.data, -any_of(dplyr::select_helpers$where(function(nm) any(purrr::map_lgl(sel, ~ .x(nm))))))
}

# ---------- Sexual orientation (Q5_*) ----------
# Map each checkbox to a canonical category, then derive collapsed groups
# Columns assumed: Q5_1..Q5_7 each holding option strings
dat <- dat |>
  mutate(
    Q5_1 = recode_checkbox(Q5_1, "Asexual"),
    Q5_2 = recode_checkbox(Q5_2, "Bisexual"),
    Q5_3 = recode_checkbox(Q5_3, "Lesbian"),
    Q5_4 = recode_checkbox(Q5_4, "Heterosexual/Straight"),
    Q5_5 = recode_checkbox(Q5_5, "Gay"),
    Q5_6 = recode_checkbox(Q5_6, "Pansexual"),
    Q5_7 = recode_checkbox(Q5_7, "Queer")
  ) |>
  # Priority order if multiple boxes are checked (explicitly encode rule)
  mutate(
    sexuality_code = case_when(
      !is.na(Q5_5) ~ 1,  # Gay
      !is.na(Q5_3) ~ 2,  # Lesbian
      !is.na(Q5_2) ~ 3,  # Bisexual
      !is.na(Q5_7) ~ 4,  # Queer
      !is.na(Q5_1) ~ 5,  # Asexual
      !is.na(Q5_4) ~ 6,  # Heterosexual
      !is.na(Q5_6) ~ 7,  # Pansexual
      TRUE ~ NA_real_
    ),
    sexuality = factor(sexuality_code,
                       levels = 1:7,
                       labels = c("Gay", "Lesbian", "Bisexual", "Queer", "Asexual", "Heterosexual", "Pansexual")
    ),
    # Collapsed identity used downstream (document the grouping logic)
    sexual_identity = case_when(
      sexuality %in% c("Gay") ~ "Gay",
      sexuality %in% c("Lesbian") ~ "Lesbian",
      sexuality %in% c("Bisexual", "Pansexual") ~ "Bisexual",
      sexuality %in% c("Queer", "Asexual", "Heterosexual") ~ "Other",
      TRUE ~ NA_character_
    ) |> factor(levels = c("Gay", "Lesbian", "Bisexual", "Other"))
  ) |>
  select(-starts_with("Q5_"), -sexuality_code, -sexuality)

# ---------- Gender (Q4_*) ----------
# Encode each checkbox and then derive a mutually exclusive category with explicit rules
dat <- dat |>
  mutate(
    Q4_1 = recode_checkbox(Q4_1, "Cisgender Woman"),
    Q4_2 = recode_checkbox(Q4_2, "Cisgender Man"),
    Q4_3 = recode_checkbox(Q4_3, "Transman"),
    Q4_4 = recode_checkbox(Q4_4, "Transwoman"),
    Q4_5 = recode_checkbox(Q4_5, "Man"),
    Q4_6 = recode_checkbox(Q4_6, "Woman"),
    Q4_7 = recode_checkbox(Q4_7, "Transgender"),
    Q4_8 = recode_checkbox(Q4_8, "Intersex"),
    Q4_9 = recode_checkbox(Q4_9, "Non-binary"),
    Q4_10 = recode_checkbox(Q4_10, "I use different words, please share below")
  ) |>
  mutate(
    # Primary derivation with transparent precedence (ties resolved top-to-bottom)
    gender_cat = case_when(
      !is.na(Q4_4) ~ "Trans W",
      !is.na(Q4_3) ~ "Trans M",
      !is.na(Q4_7) & is.na(Q4_5) & is.na(Q4_6) & is.na(Q4_3) & is.na(Q4_4) ~ "Transgender",
      !is.na(Q4_9) & is.na(Q4_7) ~ "Non-binary",
      !is.na(Q4_1) ~ "Cis W",
      !is.na(Q4_2) ~ "Cis M",
      !is.na(Q4_6) & is.na(Q4_7) ~ "Woman",
      !is.na(Q4_5) & is.na(Q4_7) ~ "Man",
      !is.na(Q4_10) ~ "Other",
      TRUE ~ NA_character_
    ),
    gender_cat = factor(gender_cat, levels = c("Cis W","Cis M","Trans M","Trans W","Transgender","Non-binary","Man","Woman","Other")),
    # Collapsed form for analysis
    gender_identity = case_when(
      gender_cat %in% c("Trans M","Trans W","Transgender","Other") ~ "Trans",
      gender_cat == "Cis W" ~ "Cis W",
      gender_cat == "Cis M" ~ "Cis M",
      gender_cat == "Non-binary" ~ "Non-binary",
      gender_cat == "Man" ~ "Man",
      gender_cat == "Woman" ~ "Woman",
      TRUE ~ NA_character_
    ) |> factor(levels = c("Trans","Cis W","Cis M","Non-binary","Man","Woman"))
  ) |>
  select(-starts_with("Q4_"), -gender_cat)

# ---------- Race (Q6_*) ----------
dat <- dat |>
  mutate(
    Q6_1 = recode_checkbox(Q6_1, "Black/African American"),
    Q6_2 = recode_checkbox(Q6_2, "Asian American or Pacific Islander"),
    Q6_3 = recode_checkbox(Q6_3, "Hispanic or Latino/a/x"),
    Q6_4 = recode_checkbox(Q6_4, "Native American or Alaskan Native"),
    Q6_5 = recode_checkbox(Q6_5, "White/European American"),
    Q6_6 = recode_checkbox(Q6_6, "Multiracial or Biracial"),
    Q6_7 = recode_checkbox(Q6_7, "I have a different identity, please specify ")
  ) |>
  mutate(
    race = case_when(
      !is.na(Q6_1) ~ "Black",
      !is.na(Q6_2) ~ "Asian American",
      !is.na(Q6_3) ~ "Latinx",
      !is.na(Q6_4) ~ "Native American",
      !is.na(Q6_5) ~ "White",
      !is.na(Q6_6) ~ "Multiracial",
      !is.na(Q6_7) ~ "Other",
      TRUE ~ NA_character_
    ) |> factor(levels = c("Black","Asian American","Latinx","Native American","White","Multiracial","Other"))
  ) |>
  select(-starts_with("Q6_"))

# ---------- Age, globals drops ----------
dat <- dat |>
  mutate(Q8 = suppressWarnings(as.numeric(Q8))) |>
  # Drop Qualtrics auto-group headers if they exist
  select(-starts_with("Measurement"), -starts_with("Demographics"))

# ---------- Core item block: Q14–Q72 ----------
item_cols <- paste0("Q", 14:72)
# Coerce to numeric; set "8" (e.g., DK/Refuse) to NA if present
dat <- dat |>
  mutate(across(all_of(item_cols), ~ suppressWarnings(as.numeric(.x)))) |>
  mutate(across(all_of(item_cols), ~ replace(.x, .x == 8, NA_real_))) |>
  # Drop non-item early questions
  select(-any_of(c("Q1","Q2"))) |>
  # Remove check 
  dat <- dat |> select(-Q55)

# ---------- Rename demographics ----------
dat <- dat |>
  rename(
    educ     = Q7,
    age      = Q8,
    region   = Q9,
    party    = Q10,
    ideology = Q11,
    area     = Q12
  )

# ---------- Map Qualtrics names to question text (for items) ----------
sq <- survey_questions(surveyID = survey_id)
qmap <- setNames(sq$question, sq$qname)

# LGBT analytic subset:
#  - Keep Gay/Lesbian/Bisexual or Trans
#  - Exclude records with >15 missing across Q14:Q72
lgbt_dat <- dat |>
  filter(sexual_identity %in% c("Gay","Lesbian","Bisexual") | gender_identity == "Trans") |>
  select(any_of(item_cols)) |>
  rowwise() |>
  filter(sum(is.na(c_across(everything()))) <= 15) |>
  ungroup()

# Make names readable for sanity
cn <- names(lgbt_dat)
names(lgbt_dat) <- ifelse(cn %in% names(qmap), qmap[cn], cn)

# ---------- Reverse-score items (documented list) ----------
# Ensure these exact strings match the renamed column headers.
reverse_items <- c(
  "I have primarily experienced positive interactions with other LGBTQ people.",
  "When I hear about anti-trans legislation, I am afraid other LGBTQ groups will be targeted next.",
  "I feel impacted by anti-LGBTQ legislation even when I am not directly impacted.",
  "I support policies that benefit the LGBTQ community.",
  "I feel isolated from the LGBTQ community.",
  "I feel rejected within the LGBTQ community.",
  "I feel bad when I hear about anti-LGBTQ legislation.",
  "I feel marginalized within the LGBTQ community."
)

# Trim any accidental leading spaces in labels
names(lgbt_dat) <- stringr::str_replace_all(names(lgbt_dat), "^\\s+", "")


lgbt_dat <- lgbt_dat |>
  mutate(across(intersect(names(lgbt_dat), reverse_items), reverse_1_to_7))

# ---------- Outlier screening (Mahalanobis) on row-level item data ----------
# Use complete cases for the covariance; keep rows under a conservative cutoff
cc <- lgbt_dat |> drop_na()
p  <- ncol(cc)
cutoff <- stats::qchisq(0.999, df = p)  # 99.9% cutoff
m_dist <- stats::mahalanobis(cc, colMeans(cc), cov(cc), inverted = FALSE)
keepers <- rep(TRUE, nrow(lgbt_dat))
keepers[as.integer(rownames(cc))] <- (m_dist < cutoff)
lgbt_dat <- lgbt_dat[keepers, , drop = FALSE]

# ---------- Plotting distance distribution, checking for overcleaning ----------
# Use complete cases for the covariance; keep rows under a conservative cutoff
library(ggplot2)
ggplot(data.frame(mahal), aes(x = mahal)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  geom_vline(xintercept = cutoff, color = "red") +
  labs(title = "Mahalanobis Distance Distribution", x = "Distance", y = "Count")

# Checking chi-squared distribution compared to expected chi-square distribution
# p = number of variables used in mahalanobis()
p <- ncol(cc) 

# Sort distances
mahal_sorted <- sort(mahal)

# Expected quantiles from chi-square distribution
theoretical <- qchisq(ppoints(length(mahal_sorted)), df = p)

# Q–Q plot
qq_df <- data.frame(
  Theoretical = theoretical,
  Observed = mahal_sorted
)

library(ggplot2)
ggplot(qq_df, aes(x = Theoretical, y = Observed)) +
  geom_point(color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Q–Q Plot of Mahalanobis Distances",
    subtitle = "Against chi-square distribution",
    x = "Theoretical Quantiles (Chi-square)",
    y = "Observed Mahalanobis Distances"
  ) +
  theme_minimal()

# ---------- Correlations (FIML) for EFA prep ----------
# Keep the row-level version (lgbt_dat) and a correlation version (R_fiml)
R_fiml <- EFAtools::corFiml(lgbt_dat)
