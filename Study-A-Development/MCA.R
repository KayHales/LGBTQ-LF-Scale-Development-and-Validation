# ---------- Justifying FIML with MCA ----------
# ---------- Packages ----------
suppressPackageStartupMessages({
  library(FactoMineR)
  library(dplyr)
  library(forcats)
  library(factoextra)
})

# ---- Step 1: Use lgbt_dat and use labels for clarity ----

item_cols <- paste0("Q", 14:72)
first_item <- intersect(item_cols, names(raw))[1]
likert_labels <- levels(raw[[first_item]])

mca_dat <- lgbt_dat %>%
  dplyr::mutate(dplyr::across(
    dplyr::everything(),
    ~ factor(as.integer(.), levels = 1:8, labels = likert_labels)
  ))

# --- Step 2. Run MCA and Plot ---
stopifnot(nrow(lgbt_dat) > 1)

# MCA
res.mca <- FactoMineR::MCA(mca_dat, graph = FALSE)

plot(
  res.mca,
  invisible = "ind",
  selectMod = list(name = grep("Don't know$", rownames(res.mca$var$coord), value = TRUE))
)
# --- Inspection: contributions of only IDK values ---

idk_rows <- grep("Don't know$", rownames(res.mca$var$contrib))
res.mca$var$contrib[idk_rows, , drop = FALSE]

idk_contrib <- idk_contrib[order(rowSums(idk_contrib), decreasing = TRUE), ]

print(round(head(idk_contrib, 20), 2))
