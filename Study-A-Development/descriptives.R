## ---- packages ----
library(dplyr)
library(stringr)
library(ggplot2)
library(psych) 

# ---------- Belonging ----------
belong_items <- c(
  "I feel closer to the LGBTQ community when I participate in LGBTQ groups.",
  "I feel closer to the LGBTQ community when I spend time with LGBTQ people.",
  "When something bad happens to me, other LGBTQ people will have my back.",
  "I feel a sense of belonging around other LGBTQ people.",
  "I feel connected to other members of the LGBTQ community, regardless of their other identities, beliefs, or experiences.",
  "My life is better when I spend time with other LGBTQ people.",
  "The LGBTQ community provides me with a sense of community I cannot get anywhere else."
)

belong_data <- lgbt_dat |>
  select(all_of(belong_items)) |>
  mutate(belong_score = rowMeans(across(all_of(belong_items)), na.rm = TRUE))

psych::describe(belong_data, skew = TRUE, ranges = TRUE)


# ---------- Utility ----------
utility_items <- c(
  "I coordinate my political actions with other LGBTQ people.",
  "I feel the need to engage in political action when LGBTQ people are attacked by politicians.",
  "I make a difference when I engage in political actions on behalf of the LGBTQ community.",
  "I try to convince other LGBTQ people to engage in political action.",
  "My LGBTQ friends keep me politically informed."
)

utility_data <- lgbt_dat |>
  select(all_of(utility_items)) |>
  mutate(utility_score = rowMeans(across(all_of(utility_items)), na.rm = TRUE))

psych::describe(utility_data, skew = TRUE, ranges = TRUE)
