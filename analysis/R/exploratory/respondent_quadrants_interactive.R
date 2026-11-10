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
# ticked or not, involvement on a 0-100 gradient, own use on 1-4. Dragging a box over dots lists those
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

# the respondent table under the map (rows for the dots selected with a box, and their profile against the sample)
as_group <- \(label, column) { column <- droplevels(as.factor(column)); list(label = label, groups = levels(column), assign = as.integer(column)) }
info <- list(
  sex  = as_group("Sex", d$sex_rec),
  age  = as_group("Age", d$age_group),
  edu  = as_group("Education", fct_rev(d$edu_group)),
  urb  = as_group("Urbanisation", d$urb_group),
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
