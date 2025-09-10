# Load required packages
pacman::p_load(
  dplyr, sjlabelled, psych, corrr, GPArotation,
  EFAtools, psychTools, haven, qualtRics,
  flextable, tibble
)

# Set up Qualtrics API credentials
qualtrics_api_credentials(
  api_key = "xxx", 
  base_url = "xxx",
  install = TRUE,
  overwrite = TRUE
)

# Reload .Renviron to access the API key
readRenviron("~/.Renviron")

# Pull survey data
surveys <- all_surveys()
data <- fetch_survey(
  surveyID = surveys$id[1],
  verbose = TRUE,
  include_metadata = NA,
  include_display_order = FALSE
)

# Recode sexuality
sexuality_map <- c(
  Q5_5 = "Gay",
  Q5_3 = "Lesbian",
  Q5_2 = "Bisexual",
  Q5_7 = "Queer",
  Q5_1 = "Asexual",
  Q5_4 = "Heterosexual",
  Q5_6 = "Pansexual"
)

# Apply mapping and construct identity variable
for (q in names(sexuality_map)) {
  data[[q]] <- ifelse(data[[q]] == sexuality_map[[q]], 1, NA)
}

data <- data %>%
  mutate(
    sexuality = case_when(
      !is.na(Q5_5) ~ 1,  # Gay
      !is.na(Q5_3) ~ 2,  # Lesbian
      !is.na(Q5_2) ~ 3,  # Bisexual
      !is.na(Q5_7) ~ 4,  # Queer
      !is.na(Q5_1) ~ 5,  # Asexual
      !is.na(Q5_4) ~ 6,  # Heterosexual
      !is.na(Q5_6) ~ 7   # Pansexual
    ),
    sexual_identity = case_when(
      sexuality == 1 ~ 1,
      sexuality == 2 ~ 2,
      sexuality %in% c(3, 7) ~ 3,
      sexuality %in% c(4, 5, 6) ~ 4
    ) %>% labelled(c(
      "Gay" = 1,
      "Lesbian" = 2,
      "Bisexual" = 3,
      "Other" = 4
    )) %>% as_label()
  ) %>%
  select(-starts_with("Q5_"), -sexuality)

table(data$sexual_identity)

# Gender mapping
gender_map <- c(
  Q4_1 = "Cisgender Woman",
  Q4_2 = "Cisgender Man",
  Q4_3 = "Transman",
  Q4_4 = "Transwoman",
  Q4_5 = "Man",
  Q4_6 = "Woman",
  Q4_7 = "Transgender",
  Q4_8 = "Intersex",
  Q4_9 = "Non-binary",
  Q4_10 = "I use different words, please share below"
)

for (q in names(gender_map)) {
  data[[q]] <- ifelse(data[[q]] == gender_map[[q]], 1, NA)
}

# Primary gender recode logic
data <- data %>%
  mutate(gender = case_when(
    !is.na(Q4_1) ~ "Cis W",
    !is.na(Q4_2) ~ "Cis M",
    !is.na(Q4_3) | (!is.na(Q4_3) & !is.na(Q4_10)) | (!is.na(Q4_7) & !is.na(Q4_5)) ~ "Trans M",
    !is.na(Q4_4) | (!is.na(Q4_7) & !is.na(Q4_6)) ~ "Trans W",
    !is.na(Q4_7) & is.na(Q4_3:Q4_6) ~ "Transgender",
    !is.na(Q4_9) & is.na(Q4_7:Q4_4) ~ "Non-binary",
    !is.na(Q4_5) & is.na(Q4_6:Q4_7, Q4_9) ~ "Man",
    !is.na(Q4_6) & is.na(Q4_5:Q4_7, Q4_9) ~ "Woman",
    !is.na(Q4_10) & is.na(Q4_3, Q4_4) ~ "Other",
    TRUE ~ NA_character_
  ))

table(data$gender)

# Derive gender_identity category
data <- data %>%
  mutate(gender_identity = case_when(
    gender %in% c("Trans M", "Trans W", "Transgender", "Other") ~ 1,
    gender == "Cis W" ~ 2,
    gender == "Cis M" ~ 3,
    gender == "Non-binary" ~ 4,
    gender == "Man" ~ 5,
    gender == "Woman" ~ 6
  ) %>% labelled(c(
    "Trans" = 1,
    "Cis W" = 2,
    "Cis M" = 3,
    "Non-binary" = 4,
    "Man" = 5,
    "Woman" = 6
  )) %>% as_label() %>% as_factor())

table(data$gender_identity)

# Drop raw gender columns
data <- data %>% select(-starts_with("Q4_"), -gender)

# Race mapping
race_map <- c(
  Q6_1 = "Black/African American",
  Q6_2 = "Asian American or Pacific Islander",
  Q6_3 = "Hispanic or Latino/a/x",
  Q6_4 = "Native American or Alaskan Native",
  Q6_5 = "White/European American",
  Q6_6 = "Multiracial or Biracial",
  Q6_7 = "I have a different identity, please specify "
)

for (q in names(race_map)) {
  data[[q]] <- ifelse(data[[q]] == race_map[[q]], 1, NA)
}

data <- data %>%
  mutate(race = case_when(
    !is.na(Q6_1) ~ 1,  # Black
    !is.na(Q6_2) ~ 2,  # Asian American
    !is.na(Q6_3) ~ 3,  # Latinx
    !is.na(Q6_4) ~ 4,  # Native American
    !is.na(Q6_5) ~ 5,  # White
    !is.na(Q6_6) ~ 6,  # Multiracial
    TRUE ~ 7           # Other
  )) %>%
  mutate(race = labelled(race, c(
    "Black" = 1,
    "Asian American" = 2,
    "Latinx" = 3,
    "Native American" = 4,
    "White" = 5,
    "Multiracial" = 6,
    "Other" = 7
  )) %>% as_label())

table(data$race)

# Optional: Drop raw Q6 items
data <- data %>% select(-starts_with("Q6_"))

# Age
data <- data %>%
  mutate(Q8 = as.numeric(Q8))
sum(is.na(data$Q8))

# Cleaning
data <- select(data, -starts_with("Measurement"))
data <- select(data, -starts_with("Demographics"))

column_convert <- paste0("Q", 14:72)
data[column_convert] <- lapply(data[column_convert], as.numeric)
column_classes <- sapply(data[ , grep("^Q\\d{2}$", names(data))], class)
print(column_classes)
data <- select(data, -c("Q1", "Q2"))

# Renaming
data <- data %>%
  rename(
    educ = Q7,
    age = Q8,
    region = Q9,
    party = Q10,
    ideology = Q11,
    area = Q12
  )
