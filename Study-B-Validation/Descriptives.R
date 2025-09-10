# Descriptives 
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
    Q16,
    Q17,
    Q18,
    Q19,
    Q20,
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
  select(Q14:Q26) %>%
  drop_na

describe(cfa_data)

# Scale Descriptive 
belong_data <- valid_data %>%
  select(Q14:Q21)
belong_data <- belong_data %>%
  mutate(belong_score = rowMeans(select(., Q14:Q21), na.rm = TRUE))
apa.cor.table(belong_data, 
                        show.conf.interval = F,
                        filename = "belongtable.doc")
describe(belong_data, skew = TRUE, ranges = TRUE)


util_data <- valid_data %>%
  select(Q22:Q26)
util_data <- util_data %>%
  mutate(util_score = rowMeans(select(., Q22:Q26), na.rm = TRUE))

apa.cor.table(util_data, 
              show.conf.interval = F,
              filename = "utiltable.doc")
describe(util_data, skew = TRUE, ranges = TRUE)

util_data <- util_data %>%
  mutate_if(is.factor, ~as.numeric(.))

scoring_key <- list(
  Belonging = c(
    "Q14", "Q15", "Q21", "Q17", "Q18", "Q19", "Q20"
  ),
  Utility = c(
    "Q22", "Q23", "Q24", "Q25", "Q26"
  )
)

test <- scoreItems(
  keys = scoring_key,
  items = valid_data
)
test$alpha

scale_data <- data %>%
  select(
    Q14,
    Q15,
    Q17,
    Q18,
    Q19,
    Q20,
    Q22,
    Q23,
    Q24,
    Q25,
    Q26
  )
scale_data[] <- lapply(scale_data, as.numeric)

psych::alpha(scale_data, warnings = T, check.keys = T)

# Other Scale Descriptives
effic_data <- valid_data %>%
  select(Q29_1:Q29_4)
effic_data <- effic_data %>%
  mutate(across(everything(), ~ as.numeric((.))))
apa.cor.table(effic_data, 
              show.conf.interval = F)
psych::alpha(effic_data)

cc_data <- valid_data %>%
  select(Q28_1:Q28_8)
cc_data <- cc_data %>%
  mutate(across(everything(), ~ as.numeric((.))))
apa.cor.table(cc_data, 
              show.conf.interval = F,
              filename = "cctable.doc")

baca_data <- valid_data %>%
  select(Q30_1:Q30_6)
baca_data <- baca_data %>%
  mutate(across(everything(), ~ as.numeric((.))))
psych::alpha(baca_data)
apa.cor.table(baca_data, 
              show.conf.interval = F,
              filename = "bacatable.doc")
baca_data <- baca_data %>%
  mutate(util_score = rowMeans(select(., Q30_1:Q30_6), na.rm = TRUE))