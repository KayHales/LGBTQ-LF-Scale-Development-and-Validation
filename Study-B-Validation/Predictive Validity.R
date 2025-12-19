# Predictive Validity # 
# install.packages("pacman")
library(lavaan)
library(dplyr)
library(stringr)
library(purrr)

model_data <- valid_data
scale_vars <- c("Age", paste0("Q32_", 1:6))
model_data[scale_vars] <- lapply(model_data[scale_vars], function(x) as.numeric(scale(x)))


model_1 <- "
# --------------------------------------------------
# Measurement: LGBTQ-LF predictors
# --------------------------------------------------
Belonging =~ Q14 + Q15 + Q21 + Q17 + Q18 + Q19 + Q20
Utility   =~ Q22 + Q23 + Q24 + Q25 + Q26

# --------------------------------------------------
# Measurement: Connectedness to LGBT Community (CC)
# Frost & Meyer's single-order model
# --------------------------------------------------
CC =~ Q28_1 + Q28_2 + Q28_3 + Q28_4 + Q28_5 + Q28_6 + Q28_7

# --------------------------------------------------
# Phantom residual factors for controls
#   Each observed control X -> E_X with Var(E_X) = 1, Var(X|E_X)=0
# --------------------------------------------------
E_Age   =~ l_Age*Age;             Age         ~~ 0*Age;         E_Age   ~~ 1*E_Age
E_Educ  =~ l_Educ*Educ;           Educ        ~~ 0*Educ;        E_Educ  ~~ 1*E_Educ
E_Ideo  =~ l_Ideo*Q11;            Q11         ~~ 0*Q11;         E_Ideo  ~~ 1*E_Ideo
E_Cis   =~ l_Cis*Gender2Cis;      Gender2Cis  ~~ 0*Gender2Cis;  E_Cis   ~~ 1*E_Cis
E_Trans =~ l_Trans*Gender2Trans;  Gender2Trans~~ 0*Gender2Trans;E_Trans~~ 1*E_Trans
E_White =~ l_White*RaceWhite;     RaceWhite   ~~ 0*RaceWhite;   E_White ~~ 1*E_White
E_Black =~ l_Black*RaceBlack;     RaceBlack   ~~ 0*RaceBlack;   E_Black ~~ 1*E_Black

# --------------------------------------------------
# Ordering via latent residuals (Blocks)
#   Block 1: control residuals (E_*)
#   Block 2: CC residual given controls (E_CC)
#   Block 3: Belonging residual given controls+CC (E_Bel)
#   Block 4: Utility residual given controls+CC+Bel (E_Util)
# --------------------------------------------------

# CC as function of control residuals
CC ~ E_Age + E_Educ + E_Ideo + E_Cis + E_Trans + E_White + E_Black

# Belonging as function of controls + CC
Belonging ~ E_Age + E_Educ + E_Ideo + E_Cis + E_Trans + E_White + E_Black + CC

# Utility as function of controls + CC + Belonging
Utility ~ E_Age + E_Educ + E_Ideo + E_Cis + E_Trans + E_White + E_Black + CC + Belonging

# --------------------------------------------------
# Phantom residuals for CC, Belonging, Utility 
# --------------------------------------------------
E_CC   =~ l_CC*CC;         CC        ~~ 0*CC;         E_CC   ~~ 1*E_CC
E_Bel  =~ l_Bel*Belonging; Belonging ~~ 0*Belonging;  E_Bel  ~~ 1*E_Bel
E_Util =~ l_Util*Utility;  Utility   ~~ 0*Utility;    E_Util ~~ 1*E_Util

# --------------------------------------------------
# Orthogonalization of residual factors
# --------------------------------------------------

# controls mutually orthogonal
E_Age  ~~ 0*E_Educ + 0*E_Ideo + 0*E_Cis + 0*E_Trans + 0*E_White + 0*E_Black
E_Educ ~~ 0*E_Ideo + 0*E_Cis + 0*E_Trans + 0*E_White + 0*E_Black
E_Ideo ~~ 0*E_Cis  + 0*E_Trans + 0*E_White + 0*E_Black
E_Cis  ~~ 0*E_Trans + 0*E_White + 0*E_Black
E_Trans~~ 0*E_White + 0*E_Black
E_White~~ 0*E_Black

# later blocks orthogonal to earlier
E_Age + E_Educ + E_Ideo + E_Cis + E_Trans + E_White + E_Black ~~ 0*E_CC + 0*E_Bel + 0*E_Util
E_CC   ~~ 0*E_Bel + 0*E_Util
E_Bel  ~~ 0*E_Util

# --------------------------------------------------
# FT single-indicator latent outcomes 
# --------------------------------------------------
FT_Lesb =~ lambda_FTL*Q32_1
Q32_1   ~~ 0*Q32_1
FT_Lesb ~~ zeta_FTL*FT_Lesb

FT_GayM =~ lambda_FTGM*Q32_2
Q32_2   ~~ 0*Q32_2
FT_GayM ~~ zeta_FTGM*FT_GayM

FT_BiM  =~ lambda_FTBM*Q32_3
Q32_3   ~~ 0*Q32_3
FT_BiM  ~~ zeta_FTBM*FT_BiM

FT_BiW  =~ lambda_FTBW*Q32_4
Q32_4   ~~ 0*Q32_4
FT_BiW  ~~ zeta_FTBW*FT_BiW

FT_TrM  =~ lambda_FTTM*Q32_5
Q32_5   ~~ 0*Q32_5
FT_TrM  ~~ zeta_FTTM*FT_TrM

FT_TrW  =~ lambda_FTTW*Q32_6
Q32_6   ~~ 0*Q32_6
FT_TrW  ~~ zeta_FTTW*FT_TrW

# --------------------------------------------------
# Structural γs to FT outcomes
# --------------------------------------------------
FT_Lesb ~ gl_age*E_Age + gl_edu*E_Educ + gl_ide*E_Ideo + gl_cis*E_Cis + gl_tr*E_Trans +
          gl_w*E_White + gl_bk*E_Black + gl_c*E_CC + gl_b*E_Bel + gl_u*E_Util

FT_GayM ~ gg_age*E_Age + gg_edu*E_Educ + gg_ide*E_Ideo + gg_cis*E_Cis + gg_tr*E_Trans +
          gg_w*E_White + gg_bk*E_Black + gg_c*E_CC + gg_b*E_Bel + gg_u*E_Util

FT_BiM  ~ gbm_age*E_Age + gbm_edu*E_Educ + gbm_ide*E_Ideo + gbm_cis*E_Cis + gbm_tr*E_Trans +
          gbm_w*E_White + gbm_bk*E_Black + gbm_c*E_CC + gbm_b*E_Bel + gbm_u*E_Util

FT_BiW  ~ gbw_age*E_Age + gbw_edu*E_Educ + gbw_ide*E_Ideo + gbw_cis*E_Cis + gbw_tr*E_Trans +
          gbw_w*E_White + gbw_bk*E_Black + gbw_c*E_CC + gbw_b*E_Bel + gbw_u*E_Util

FT_TrM  ~ gtm_age*E_Age + gtm_edu*E_Educ + gtm_ide*E_Ideo + gtm_cis*E_Cis + gtm_tr*E_Trans +
          gtm_w*E_White + gtm_bk*E_Black + gtm_c*E_CC + gtm_b*E_Bel + gtm_u*E_Util

FT_TrW  ~ gtw_age*E_Age + gtw_edu*E_Educ + gtw_ide*E_Ideo + gtw_cis*E_Cis + gtw_tr*E_Trans +
          gtw_w*E_White + gtw_bk*E_Black + gtw_c*E_CC + gtw_b*E_Bel + gtw_u*E_Util

# --------------------------------------------------
# Ordinal/binary outcomes 
# --------------------------------------------------
Q31 ~ q31_age*E_Age + q31_edu*E_Educ + q31_ide*E_Ideo + q31_cis*E_Cis + q31_tr*E_Trans +
      q31_w*E_White + q31_bk*E_Black + q31_c*E_CC + q31_b*E_Bel + q31_u*E_Util

Q33 ~ q33_age*E_Age + q33_edu*E_Educ + q33_ide*E_Ideo + q33_cis*E_Cis + q33_tr*E_Trans +
      q33_w*E_White + q33_bk*E_Black + q33_c*E_CC + q33_b*E_Bel + q33_u*E_Util

Q34 ~ q34_age*E_Age + q34_edu*E_Educ + q34_ide*E_Ideo + q34_cis*E_Cis + q34_tr*E_Trans +
      q34_w*E_White + q34_bk*E_Black + q34_c*E_CC + q34_b*E_Bel + q34_u*E_Util

Q37 ~ q37_age*E_Age + q37_edu*E_Educ + q37_ide*E_Ideo + q37_cis*E_Cis + q37_tr*E_Trans +
      q37_w*E_White + q37_bk*E_Black + q37_c*E_CC + q37_b*E_Bel + q37_u*E_Util

Q38 ~ q38_age*E_Age + q38_edu*E_Educ + q38_ide*E_Ideo + q38_cis*E_Cis + q38_tr*E_Trans +
      q38_w*E_White + q38_bk*E_Black + q38_c*E_CC + q38_b*E_Bel + q38_u*E_Util

Q40 ~ q40_age*E_Age + q40_edu*E_Educ + q40_ide*E_Ideo + q40_cis*E_Cis + q40_tr*E_Trans +
      q40_w*E_White + q40_bk*E_Black + q40_c*E_CC + q40_b*E_Bel + q40_u*E_Util

# --------------------------------------------------
# Unit-variance identities for FT latent outcomes (F&H)
#   Var(FT_*) = 1 = zeta_FT* + sum of squared γs
# --------------------------------------------------
1 == zeta_FTL  + gl_age^2  + gl_edu^2 + gl_ide^2 + gl_cis^2 + gl_tr^2 +
                gl_w^2    + gl_bk^2 + gl_c^2   + gl_b^2    + gl_u^2

1 == zeta_FTGM + gg_age^2  + gg_edu^2 + gg_ide^2 + gg_cis^2 + gg_tr^2 +
                 gg_w^2    + gg_bk^2 + gg_c^2   + gg_b^2    + gg_u^2

1 == zeta_FTBM + gbm_age^2 + gbm_edu^2+ gbm_ide^2+ gbm_cis^2+ gbm_tr^2 +
                 gbm_w^2   + gbm_bk^2+ gbm_c^2  + gbm_b^2   + gbm_u^2

1 == zeta_FTBW + gbw_age^2 + gbw_edu^2+ gbw_ide^2+ gbw_cis^2+ gbw_tr^2 +
                 gbw_w^2   + gbw_bk^2+ gbw_c^2  + gbw_b^2   + gbw_u^2

1 == zeta_FTTM + gtm_age^2 + gtm_edu^2+ gtm_ide^2+ gtm_cis^2+ gtm_tr^2 +
                 gtm_w^2   + gtm_bk^2+ gtm_c^2  + gtm_b^2   + gtm_u^2

1 == zeta_FTTW + gtw_age^2 + gtw_edu^2+ gtw_ide^2+ gtw_cis^2+ gtw_tr^2 +
                 gtw_w^2   + gtw_bk^2+ gtw_c^2  + gtw_b^2   + gtw_u^2

# --------------------------------------------------
# ΔR² definitions (FT outcomes) — γ²
# --------------------------------------------------
DeltaR2_Bel_FT_Lesb  := gl_b^2
DeltaR2_Util_FT_Lesb := gl_u^2
DeltaR2_CC_FT_Lesb   := gl_c^2
DeltaR2_BU_FT_Lesb   := gl_b^2 + gl_u^2

DeltaR2_Bel_FT_GayM  := gg_b^2
DeltaR2_Util_FT_GayM := gg_u^2
DeltaR2_CC_FT_GayM   := gg_c^2
DeltaR2_BU_FT_GayM   := gg_b^2 + gg_u^2

DeltaR2_Bel_FT_BiM   := gbm_b^2
DeltaR2_Util_FT_BiM  := gbm_u^2
DeltaR2_CC_FT_BiM    := gbm_c^2
DeltaR2_BU_FT_BiM    := gbm_b^2 + gbm_u^2

DeltaR2_Bel_FT_BiW   := gbw_b^2
DeltaR2_Util_FT_BiW  := gbw_u^2
DeltaR2_CC_FT_BiW    := gbw_c^2
DeltaR2_BU_FT_BiW    := gbw_b^2 + gbw_u^2

DeltaR2_Bel_FT_TrM   := gtm_b^2
DeltaR2_Util_FT_TrM  := gtm_u^2
DeltaR2_CC_FT_TrM    := gtm_c^2
DeltaR2_BU_FT_TrM    := gtm_b^2 + gtm_u^2

DeltaR2_Bel_FT_TrW   := gtw_b^2
DeltaR2_Util_FT_TrW  := gtw_u^2
DeltaR2_CC_FT_TrW    := gtw_c^2
DeltaR2_BU_FT_TrW    := gtw_b^2 + gtw_u^2

# --------------------------------------------------
# Latent-response ΔR² for categorical outcomes (probit)
# --------------------------------------------------
Den_Q31 := (1 + q31_age^2 + q31_edu^2 + q31_ide^2 + q31_cis^2 + q31_tr^2 +
               q31_w^2 + q31_bk^2 + q31_c^2 + q31_b^2 + q31_u^2)
DeltaR2_Bel_Q31  := q31_b^2 / Den_Q31
DeltaR2_Util_Q31 := q31_u^2 / Den_Q31
DeltaR2_CC_Q31   := q31_c^2 / Den_Q31
DeltaR2_BU_Q31   := (q31_b^2 + q31_u^2) / Den_Q31

Den_Q33 := (1 + q33_age^2 + q33_edu^2 + q33_ide^2 + q33_cis^2 + q33_tr^2 +
               q33_w^2 + q33_bk^2 + q33_c^2 + q33_b^2 + q33_u^2)
DeltaR2_Bel_Q33  := q33_b^2 / Den_Q33
DeltaR2_Util_Q33 := q33_u^2 / Den_Q33
DeltaR2_CC_Q33   := q33_c^2 / Den_Q33
DeltaR2_BU_Q33   := (q33_b^2 + q33_u^2) / Den_Q33

Den_Q34 := (1 + q34_age^2 + q34_edu^2 + q34_ide^2 + q34_cis^2 + q34_tr^2 +
               q34_w^2 + q34_bk^2 + q34_c^2 + q34_b^2 + q34_u^2)
DeltaR2_Bel_Q34  := q34_b^2 / Den_Q34
DeltaR2_Util_Q34 := q34_u^2 / Den_Q34
DeltaR2_CC_Q34   := q34_c^2 / Den_Q34
DeltaR2_BU_Q34   := (q34_b^2 + q34_u^2) / Den_Q34

Den_Q37 := (1 + q37_age^2 + q37_edu^2 + q37_ide^2 + q37_cis^2 + q37_tr^2 +
               q37_w^2 + q37_bk^2 + q37_c^2 + q37_b^2 + q37_u^2)
DeltaR2_Bel_Q37  := q37_b^2 / Den_Q37
DeltaR2_Util_Q37 := q37_u^2 / Den_Q37
DeltaR2_CC_Q37   := q37_c^2 / Den_Q37
DeltaR2_BU_Q37   := (q37_b^2 + q37_u^2) / Den_Q37

Den_Q38 := (1 + q38_age^2 + q38_edu^2 + q38_ide^2 + q38_cis^2 + q38_tr^2 +
               q38_w^2 + q38_bk^2 + q38_c^2 + q38_b^2 + q38_u^2)
DeltaR2_Bel_Q38  := q38_b^2 / Den_Q38
DeltaR2_Util_Q38 := q38_u^2 / Den_Q38
DeltaR2_CC_Q38   := q38_c^2 / Den_Q38
DeltaR2_BU_Q38   := (q38_b^2 + q38_u^2) / Den_Q38

Den_Q40 := (1 + q40_age^2 + q40_edu^2 + q40_ide^2 + q40_cis^2 + q40_tr^2 +
               q40_w^2 + q40_bk^2 + q40_c^2 + q40_b^2 + q40_u^2)
DeltaR2_Bel_Q40  := q40_b^2 / Den_Q40
DeltaR2_Util_Q40 := q40_u^2 / Den_Q40
DeltaR2_CC_Q40   := q40_c^2 / Den_Q40
DeltaR2_BU_Q40   := (q40_b^2 + q40_u^2) / Den_Q40

# --------------------------------------------------
# Sign constraints for identification (F&H-style)
# --------------------------------------------------
# phantom control loadings
l_Age   > 0
l_Educ  > 0
l_Ideo  > 0
l_Cis   > 0
l_Trans > 0
l_White > 0
l_Black > 0

# phantom residuals for CC, Belonging, Utility
l_CC   > 0
l_Bel  > 0
l_Util > 0

# FT latent outcome loadings
lambda_FTL  > 0
lambda_FTGM > 0
lambda_FTBM > 0
lambda_FTBW > 0
lambda_FTTM > 0
lambda_FTTW > 0
"



# Model 1 Fit
fit_1 <- sem(
  model_1,
  data             = model_data,
  estimator        = "ULSMV",
  std.lv           = FALSE,
  parameterization = "theta",
  ordered = c(
    paste0("Q30_", 1:6),
    "Q31","Q33","Q34","Q37","Q38","Q40",
    "Q14","Q15","Q21","Q17","Q18","Q19","Q20",
    "Q22","Q23","Q24","Q25","Q26",
    "Q28_1","Q28_2","Q28_3","Q28_4","Q28_5","Q28_6","Q28_7"
  )
)

# Fit of model 
fitMeasures(fit_1, c(
  "chisq.scaled","df","pvalue.scaled",
  "cfi.scaled","tli.scaled",
  "rmsea.scaled","rmsea.ci.lower","rmsea.ci.upper",
  "srmr"
))

summary(fit_1, fit.measures = TRUE, standardized = TRUE)


# Results for manuscript 
pe       <- parameterEstimates(fit_1, ci = TRUE, standardized = TRUE)

# Betas (γ) you care about:
betas_BU <- subset(
  pe,
  op == "~" &
    lhs %in% c("FT_Lesb","FT_GayM","FT_BiM","FT_BiW","FT_TrM","FT_TrW",
               "Q31","Q33","Q34","Q37","Q38","Q40") &
    rhs %in% c("E_Bel","E_Util"),
  select = c("lhs","rhs","est","se","pvalue","ci.lower","ci.upper")
)

betas_BU



