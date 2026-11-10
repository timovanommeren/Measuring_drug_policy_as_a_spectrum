library(tidyverse)

source(here::here("analysis", "R", "00_load_and_clean.R"))
source(here::here("analysis", "R", "utils", "report_helpers.R"))

# Interactive version of respondent_quadrants.R: every respondent by mean preferred policy (1 = ban ... 9 = free
# sale) over a set of drugs on each axis, with dashed lines at the weighted means of all respondents. The axes
# are chosen in the browser: x cannabis, MDMA, soft drugs (cannabis, MDMA, psychedelics), hard drugs (cocaine,
# speed, heroin) or all illicit drugs; y alcohol, tobacco or both. Scores, means and quadrants are computed
# there, per respondent over the drugs answered, as in utils/party_map_scores.R.
# In the browser the map can be split into panels by the descriptive report's split variables (plus party),
# and the points coloured by any attitude item: belief and behaviour items on a 1-7 gradient, emotion boxes
# ticked or not, involvement on a 0-100 gradient, own use on 1-4, and the latent class and profile solutions
# (LCA, LPA) as colours or panels. Dragging a box over dots lists those
# respondents in a table under the map (demographics, vote, own use, involvement, their nine policy answers),
# with the group's weighted profile against everyone on the map.
# The data are inlined into the HTML, so the output stays in analysis/output/ (not tracked) and must not be
# shared or published. Output: analysis/output/respondent_quadrants_interactive.html

d <- df
excluded <- c("Niet-stemmers", "Blanco", "Mocht (nog) niet stemmen", "DENK", "Anders")  # as in the party map
drugs <- c("alcohol", "tabak_en_vapes", "cannabis", "psychedelica", "mdma", "speed", "cocaine", "heroine", "pijnstillers")

# the report's split buttons, plus party (every party on the party map; DENK, Anders, blank, non-voters NA)
splits <- build_splits(d)
party <- factor(replace(as.character(d$vote_recall_rec), as.character(d$vote_recall_rec) %in% excluded, NA))
splits$party <- list(label = "Partij (stemherinnering)", groups = levels(party), assign = as.integer(party))
splits <- map(splits, \(s) s[c("label", "groups", "assign")])

# the latent class and profile solutions from exploratory/latent/ (lca.R, lpa_profiles.R), read from their saved
# fits in analysis/output/latent_models/ (run those scripts first; skipped if the fits are missing). Every
# respondent's modal class, with its posterior probability, as a colour and split option. Each class is labelled
# with its weighted share and the weighted mean answer (1-9) of its members on the legal, soft and hard drugs
policy <- \(v) as.numeric(d[[paste0("beleid_voorkeur_", v)]])
dims <- list(legal = c("alcohol", "tabak_en_vapes"), soft = c("cannabis", "psychedelica", "mdma"),
             hard = c("cocaine", "speed", "heroine"))
dim_scores <- map(dims, \(v) { s <- rowMeans(sapply(v, policy), na.rm = TRUE); replace(s, is.nan(s), NA) })

latent_entry <- function(cls, prob, label, block, noun = "Class") {
  w <- as.numeric(d$w)
  groups <- map_chr(sort(unique(na.omit(cls))), \(k) {
    m <- !is.na(cls) & cls == k
    means <- map_dbl(dim_scores, \(s) weighted.mean(s[m], w[m], na.rm = TRUE))
    sprintf("%s %d (%.0f%%): legal %.1f, soft %.1f, hard %.1f", noun, k,
            100 * sum(w[m]) / sum(w[!is.na(cls)]), means[["legal"]], means[["soft"]], means[["hard"]])
  })
  list(label = label, block = block, groups = groups, assign = match(cls, sort(unique(na.omit(cls)))),
       prob = round(prob, 3))
}

latent <- list()
lca_file <- here::here("analysis", "output", "latent_models", "lca", "lca_fits.rds")
if (file.exists(lca_file)) {
  lca <- read_rds(lca_file)   # poLCA fits for 1-8 classes, one row per respondent of df
  for (k in 2:8) {
    f <- lca[[as.character(k)]]
    stopifnot(length(f$predclass) == nrow(d))
    latent[[paste0("lca_", k)]] <- latent_entry(f$predclass, apply(f$posterior, 1, max),
                                                sprintf("LCA: %d classes", k), "Latent classes (LCA, eight drugs)")
  }
} else message("no LCA fits at ", lca_file, ": run exploratory/latent/lca.R first")

lpa_file <- here::here("analysis", "output", "latent_models", "lpa", "lpa_fits.rds")
if (file.exists(lpa_file)) {
  lpa <- read_rds(lpa_file)   # tidyLPA fits; rows are the respondents with all three indicators, as in lpa_profiles.R
  in_lpa <- reduce(dim_scores, \(a, b) a & !is.na(b), .init = rep(TRUE, nrow(d)))
  model_names <- c(`1` = "equal variances", `2` = "varying variances", `6` = "varying variances and covariances")
  for (m in names(model_names)) for (k in 2:6) {
    dff <- lpa[[sprintf("model_%s_class_%d", m, k)]]$dff
    stopifnot(nrow(dff) == sum(in_lpa))
    cls <- prob <- rep(NA_real_, nrow(d))
    cls[in_lpa] <- dff$Class
    prob[in_lpa] <- apply(as.matrix(dff[grep("^CPROB", names(dff))]), 1, max)
    latent[[sprintf("lpa_%s_%d", m, k)]] <- latent_entry(
      cls, prob, sprintf("LPA model %s: %d profiles%s", m, k, if (m == "1" && k == 4) " (provisional choice)" else ""),
      sprintf("Latent profiles (LPA model %s, %s)", m, model_names[[m]]), noun = "Profile")
  }
} else message("no LPA fits at ", lpa_file, ": run exploratory/latent/lpa_profiles.R first")

# the respondent table under the map (rows for the dots selected with a box, and their profile against the sample)
# education and urbanisation at their finest level here (the splits use the 3 and 2 groups): edu_clean's
# ten categories and urb's five address-density classes, both lowest first, labels shortened from the codebook.
# ordered = TRUE keeps them in that order, all categories shown, in the profile above the table
edu_detail <- factor(as.numeric(d$edu_clean), levels = 10:1,
                     labels = c("Geen onderwijs", "Basisonderwijs", "Praktijk/VSO", "MBO 1/VMBO-b/k", "MAVO/VMBO-t",
                                "MBO 2-4", "HAVO/VWO", "HBO/WO propedeuse", "HBO/WO bachelor", "HBO/WO master"))
urb_detail <- factor(as.numeric(d$urb), levels = 5:1,
                     labels = c("Niet (<500)", "Weinig (500-1000)", "Matig (1000-1500)", "Sterk (1500-2500)",
                                "Zeer sterk (2500+)"))
as_group <- \(label, column, ordered = FALSE) {
  column <- droplevels(as.factor(column))
  list(label = label, groups = levels(column), assign = as.integer(column), ordered = ordered)
}
info <- list(
  sex  = as_group("Sex", d$sex_rec),
  age  = as_group("Age", d$age_group),
  edu  = as_group("Education", edu_detail, ordered = TRUE),
  urb  = as_group("Urbanisation (addresses/km2)", urb_detail, ordered = TRUE),
  prov = as_group("Province", d$geo_prov_rec),
  vote = as_group("Vote (TK 2025)", d$vote_recall_rec),
  use  = as_group("Own use (heaviest ever)", d$substance_ladder)
)

# the variables to colour by
item <- function(col, block, type, label = var_label(col, d)) {
  list(id = col, block = block, type = type, label = label, anti = col %in% anti_items,
       values = as.numeric(d[[col]]))
}
blocks <- c(society = "Beliefs: society", addiction = "Beliefs: addiction", regulate = "Beliefs: regulation",
            crime = "Beliefs: crime")
beliefs <- grep("^beliefs_[a-z]+_[0-9]$", names(d), value = TRUE)
emo_cols <- paste0("emo_", rep(emo_topics, each = 4), "_", emo_feelings)
emo_label <- \(col) str_remove(var_label(col, d), "^Kruis voor elk onderwerp aan welk gevoel u erbij hebt\\. ")
emo_topic <- \(col) str_remove(emo_label(col), " (Boos|Bang / bezorgd|Blij|Geen van deze)$")

colour_vars <- c(
  map(beliefs, \(v) item(v, blocks[[str_extract(v, "(?<=beliefs_)[a-z]+")]], "likert")),
  map(paste0("gedrag_", 1:3), \(v) item(v, "Behaviour", "likert")),
  map(emo_cols, \(v) item(v, paste("Emotions:", emo_topic(v)), "binary", emo_label(v))),
  list(item("betrokkenheid_mean", "Involvement", "slider", "Involvement: mean of the five sliders")),
  map(c("betrokkenheid_1", "betrokkenheid_1b", "betrokkenheid_2", "betrokkenheid_2b", "betrokkenheid_3"),
      \(v) item(v, "Involvement", "slider")),
  map(paste0("gebruik_", 1:4), \(v) item(v, "Own use", "use",
                                         str_remove(var_label(v, d), "^.*hoe vaak u het heeft gebruikt\\. ")))
)

set.seed(1)
data <- list(
  answers = map(set_names(drugs), \(v) as.numeric(d[[paste0("beleid_voorkeur_", v)]])),
  w = as.numeric(d$w),
  jx = runif(nrow(d), -1, 1), jy = runif(nrow(d), -1, 1),  # jitter, scaled in the browser to the axis's step
  splits = splits,
  latent = latent,
  info = info,
  involvement = as.numeric(d$betrokkenheid_mean),
  vars = colour_vars,
  use_labels = names(attr(d$gebruik_1, "labels"))[1:4]
)

template <- read_file(here::here("analysis", "R", "exploratory", "respondent_quadrants_template.html"))
json <- as.character(jsonlite::toJSON(data, auto_unbox = TRUE, na = "null", digits = 4))
dir.create(here::here("analysis", "output"), showWarnings = FALSE)
write_file(str_replace(template, fixed("__DATA__"), json),
           here::here("analysis", "output", "respondent_quadrants_interactive.html"))
