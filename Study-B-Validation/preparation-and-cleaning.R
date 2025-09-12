# ---- Setup ---------------------------------------------------------------

# Install/load packages conveniently
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  qualtRics, dplyr, tidyr, stringr, forcats, janitor,
  lavaan, tibble

# ---- Fetch survey --------------------------------------------------------

survey_id <- "SV_cCHDLaZMmtKWNaS"  
raw <- qualtRics::fetch_survey(
  surveyID = survey_id,
  label = FALSE,               
  include_display_order = FALSE,
  include_metadata = NA,
  force_request = TRUE
)

# ------------------------- Cleaning & Preparing -----------------------------
# Drop unneeded items and rename selected vars
data <- data %>%
  select(-c(Q1, Q2), -starts_with("LGBTQDemo")) %>%
  rename(
    Educ  = Q7,
    Region = Q9,
    Party  = Q10,
    Metro  = Q12,
    Age    = Q8
  )

# ------------------------------- Gender -------------------------------------

data <- data %>%
  mutate(
    Gender = case_when(
      # Manual recoding from Q3_9_TEXT
      str_detect(Q3_9_TEXT, "Female but not woman") ~ 2,  # Cis woman
      str_detect(Q3_9_TEXT, "non binary woman") ~ 4,      # Trans woman
      str_detect(Q3_9_TEXT, "Woman but gender fluid") ~ 4,# Trans woman
      str_detect(Q3_9_TEXT, "Transsexual man") ~ 3,       # Trans man
      str_detect(Q3_9_TEXT, "Genderqueer afab") ~ 4,
      str_detect(Q3_9_TEXT, "Genderfluid") ~ 4,           # Trans woman

      # Trans Man (Category 3)
      Q3_3 == "Transgender man" ~ 3,                      # Selected "Transgender man"
      Q3_5 == "Man" & Q4 == "Transgender" ~ 3,            # "Man" in Q3_5 and "Transgender" in Q4
      Q3_5 == "Man" & Q3_8 == "Non-binary" ~ 3,
      Q3_1 == "Cisgender man" & Q3_8 == "Non-binary" ~ 3, # "Man" in Q3_5 and "Non-binary" in Q3_8

      # Trans Woman (Category 4)
      Q3_4 == "Transgender woman" ~ 4,
      Q3_6 == "Woman" & Q4 == "Transgender" ~ 4,          # "Woman" in Q3_6 and "Transgender" in Q4
      Q3_6 == "Woman" & Q3_8 == "Non-binary" ~ 4,
      Q3_2 == "Cisgender woman" & Q3_8 == "Non-binary" ~ 4,# "Woman" in Q3_6 and "Non-binary" in Q3_8

      # Cisgender Man (Category 1)
      Q3_1 == "Cisgender man" ~ 1,                        # Selected "Cisgender man"
      Q3_5 == "Man" & Q4 == "Cisgender" ~ 1,              # "Man" in Q3_5 and "Cisgender" in Q4

      # Cisgender Woman (Category 2)
      Q3_2 == "Cisgender woman" ~ 2,                      # Selected "Cisgender woman"
      Q3_6 == "Woman" & Q4 == "Cisgender" ~ 2,            # "Woman" in Q3_6 and "Cisgender" in Q4

      # Default to "Other" if no conditions are met
      TRUE ~ 5
    )
  )

# Convert Gender to factor with labeled levels
data$Gender <- factor(
  data$Gender,
  levels = c(1, 2, 3, 4, 5),
  labels = c("Cis man", "Cis woman", "Trans man", "Trans woman", "Other")
)

# Check
table(data$Gender, useNA = "ifany")

# Alternative Gender specification for later testing
data <- data %>%
  mutate(
    Gender2 = case_when(
      Gender == "Cis man"   | Gender == "Cis woman" ~ 1,
      Gender == "Trans man" | Gender == "Trans woman" ~ 2,
      Gender == "Other" ~ 3,
      TRUE ~ NA_real_
    )
  )

data$Gender2 <- factor(
  data$Gender2,
  levels = c(1, 2, 3),
  labels = c("Cis", "Trans", "Other")
)

table(data$Gender2, useNA = "ifany")

# -------------------------------- Sexuality ---------------------------------
data <- data %>%
  mutate(
    Sexuality = case_when(
      Q5_2 == "Bisexual" ~ 3,
      Q5_3 == "Lesbian"  ~ 1,
      Q5_5 == "Gay"      ~ 2,
      TRUE               ~ 4
    )
  )

data$Sexuality <- factor(
  data$Sexuality,
  levels = c(1, 2, 3, 4),
  labels = c("Lesbian", "Gay", "Bisexual", "Other")
)

table(data$Sexuality)

# ---------------------------------- Race ------------------------------------
data <- data %>%
  mutate(
    Race = case_when(
      Q6_1 == "Black/African American"             ~ 1,
      Q6_2 == "Asian American or Pacific Islander" ~ 2,
      Q6_3 == "Hispanic or Latino/a/x"             ~ 3,
      Q6_5 == "White/European American"            ~ 4,
      TRUE                                         ~ 5
    )
  )

data$Race <- factor(
  data$Race,
  levels = c(1, 2, 3, 4, 5),
  labels = c("Black", "Asian American", "Latine", "White", "Other")
)
