# --- Packages ---
library(dplyr)
library(psych)
library(apaTables)

# --- Common settings ---
likert_levels <- c(
  "Strongly disagree",
  "Somewhat disagree",
  "Neither agree nor disagree",
  "Somewhat agree",
  "Strongly agree"
)

belong_items <- c("Q14","Q15","Q21","Q17","Q18","Q19","Q20")
util_items   <- c("Q22","Q23","Q24","Q25","Q26")
cfa_items    <- paste0("Q", 14:26)

# --- Descriptives for LGBTQ-LF ---
cfa_data <- data %>%
  select(Gender, Sexuality, Age, Party, Educ, Metro, all_of(cfa_items)) %>%
  mutate(across(all_of(cfa_items),
                ~ factor(.x, levels = likert_levels, ordered = TRUE))) %>%
  select(all_of(cfa_items)) %>%
  tidyr::drop_na()

describe(cfa_data)

# --- Scale Descriptives ---
# Belonging
belong_data <- valid_data %>%
  select(all_of(belong_items)) %>%
  mutate(across(everything(),
                ~ factor(.x, levels = likert_levels, ordered = TRUE))) %>%
  mutate(across(everything(), ~ as.numeric(.x))) %>%
  mutate(belong_score = rowMeans(across(all_of(belong_items)), na.rm = TRUE))

apa.cor.table(belong_data %>% select(all_of(belong_items)),
              show.conf.interval = FALSE,
              filename = "belongtable.doc")
describe(belong_data %>% select(all_of(belong_items)),
         skew = TRUE, ranges = TRUE)

# --- Reliabilities ---
valid_numeric <- valid_data %>%
  mutate(across(all_of(c(belong_items, util_items)),
                ~ as.numeric(factor(.x, levels = likert_levels, ordered = TRUE))))

scoring_key <- list(
  Belonging = belong_items,
  Utility   = util_items
)

score_out <- scoreItems(keys = scoring_key, items = valid_numeric)
score_out$alpha

# Alpha on full scale
scale_items <- c("Q14","Q15","Q17","Q18","Q19","Q20","Q22","Q23","Q24","Q25","Q26")
scale_data <- valid_numeric %>% select(all_of(scale_items))
psych::alpha(scale_data, warnings = TRUE, check.keys = TRUE)

# --- Reliabilities for other scales ---
 Political efficacy 
effic_items <- paste0("Q29_", 1:4)
effic_data <- valid_data %>%
  select(all_of(effic_items)) %>%
  mutate(across(everything(), ~ as.numeric(.x)))

apa.cor.table(effic_data, show.conf.interval = FALSE)
psych::alpha(effic_data)

# Community Connectedness
cc_items <- paste0("Q28_", 1:8)
cc_data <- valid_data %>%
  select(all_of(cc_items)) %>%
  mutate(across(everything(), ~ as.numeric(.x)))

apa.cor.table(cc_data,
              show.conf.interval = FALSE,
              filename = "cctable.doc")

# BACA 
baca_items <- paste0("Q30_", 1:6)
baca_data <- valid_data %>%
  select(all_of(baca_items)) %>%
  mutate(across(everything(), ~ as.numeric(.x)))

psych::alpha(baca_data)
apa.cor.table(baca_data,
              show.conf.interval = FALSE,
              filename = "bacatable.doc")

baca_data <- baca_data %>%
  mutate(baca_score = rowMeans(across(all_of(baca_items)), na.rm = TRUE))

baca_data <- baca_data %>%
  mutate(util_score = rowMeans(select(., Q30_1:Q30_6), na.rm = TRUE))
