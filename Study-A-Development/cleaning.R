############################################################
# Project: Study A of Scale Development and Validation of the LGBTQ-LF
# Author: Kay Hales, Ellen Riggle
#  - Reads QUALTRICS_API_KEY from .Renviron
#  - Recoding data
#  - Data preparation
############################################################

# ---------- Packages ----------
# Easy loading and installing
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
# Map each checkbox to a category, then collapse groups
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
  # Priority order if multiple boxes are checked
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
    # Collapsed identity used downstream
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
# Encode each check box and then make a mutually exclusive category
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
    # Easier to recode for gender_identity
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

# ---------- Race ----------
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

# ---------- Age, global drops ----------
dat <- dat |>
  mutate(Q8 = suppressWarnings(as.numeric(Q8))) |>
  # Global drop
  select(-starts_with("Measurement"), -starts_with("Demographics"))

# ---------- Core item block: Q14–Q72 ----------
# See MCA.R for multiple correspondence analysis to justify
# treating 8 (i.e., "Don't know) as NA
item_cols <- paste0("Q", 14:72)
dat <- dat |>
  mutate(across(all_of(item_cols), ~ suppressWarnings(as.numeric(.x)))) |>
  mutate(across(all_of(item_cols), ~ replace(.x, .x == 8, NA_real_))) |>
  select(-any_of(c("Q1","Q2","Q55")))
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

# ---------- Quick sanity checks ---------- 
table(dat$sexual_identity) 
table(dat$gender_identity)
table(dat$race)
summary(dat$age)


# ---------- Map Qualtrics names to scale text ----------
sq <- survey_questions(surveyID = survey_id)
qmap <- setNames(sq$question, sq$qname)

# ---------- LGBT analytic subset ----------
lgbt_dat <- dat |>
  filter(sexual_identity %in% c("Gay","Lesbian","Bisexual") | gender_identity == "Trans") |>
  select(any_of(item_cols)) |>
  rowwise() |>
  filter(sum(is.na(c_across(everything()))) <= 15) |>
  ungroup()

# Make names readable and trim spaces imported from Qualtrics
names(lgbt_dat) <- ifelse(names(lgbt_dat) %in% names(qmap), qmap[names(lgbt_dat)], names(lgbt_dat))
names(lgbt_dat) <- stringr::str_replace_all(names(lgbt_dat), "^\\s+", "")

# ---------- Reverse-score items ----------
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

lgbt_dat <- lgbt_dat |>
  mutate(across(intersect(names(lgbt_dat), reverse_items),
                ~ ifelse(is.na(.x), NA_real_, 8 - as.numeric(.x))))

str(lgbt_dat)



# ---------- Mahalanobis distance ----------
cc <- tidyr::drop_na(lgbt_dat)
p  <- ncol(cc)

cutoff <- stats::qchisq(0.999, df = p)
m_dist <- stats::mahalanobis(cc, colMeans(cc), cov(cc))

# keepers: map indices of complete cases back to original rows
keepers <- rep(TRUE, nrow(lgbt_dat))
keepers[which(complete.cases(lgbt_dat))] <- (m_dist < cutoff)
lgbt_dat <- lgbt_dat[keepers, , drop = FALSE]

# Distribution plot
library(ggplot2)
ggplot(data.frame(m_dist = m_dist), aes(x = m_dist)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  geom_vline(xintercept = cutoff, color = "red") +
  labs(title = "Mahalanobis Distance Distribution", x = "Distance", y = "Count")

# Q–Q plot against chi-squared
m_sorted   <- sort(m_dist)
theoretical <- qchisq(ppoints(length(m_sorted)), df = p)
qq_df <- data.frame(Theoretical = theoretical, Observed = m_sorted)

ggplot(qq_df, aes(Theoretical, Observed)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Q–Q Plot of Mahalanobis Distances",
    subtitle = "vs. Chi-square(df = number of items)"
  ) +
  theme_minimal()
