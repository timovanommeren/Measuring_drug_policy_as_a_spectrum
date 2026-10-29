library(tidyverse)

source(here::here("analysis", "R", "load_and_clean.R"))

# exploratory: weighted % per answer for each drug, with all categories of a demographic side by
# side (one page per drug x demographic). Two PDFs:
# - the four grouped regimes, with detailed demographics (many categories)
# - the nine answer options, with coarse demographics (few categories)
# plus the party split by involvement (grouped regimes)

drug_labels <- c(alcohol = "Alcohol", tabak_en_vapes = "Tobacco and vapes", cannabis = "Cannabis",
                 psychedelica = "Psychedelics", mdma = "MDMA", cocaine = "Cocaine / crack",
                 speed = "Speed", heroine = "Heroin")

option_labels <- c("Ban", "Research only", "Treatment only", "Prescription only", "Specialist shop, non-profit",
                   "Specialist shop, for-profit", "Sold everywhere, no advertising",
                   "Sold everywhere, with advertising", "Free sale")

# answer schemes: the four grouped regimes (regime_labels from load_and_clean.R) or the nine options,
# with the colours of figure 2
schemes <- list(
  regime = list(levels = str_to_sentence(regime_labels), code = \(x) cut(x, c(0, 1, 4, 6, 9), labels = FALSE),
                colours = viridisLite::viridis(9, end = 0.9)[c(1, 3, 6, 8)],
                note = "grouped regime: ban (1), medical/research (2–4), specialist shop (5–6), general market (7–9)"),
  option = list(levels = option_labels, code = \(x) x, colours = viridisLite::viridis(9, end = 0.9),
                note = "answer option (1 = ban … 9 = free sale)")
)

# vote per party; small parties (n < 30, except Volt) and "Anders" together, non-voters and blank
# together (involvement pages)
party_n <- count(df, vote_recall_rec)
small <- setdiff(as.character(party_n$vote_recall_rec[party_n$n < 30]), "Volt")
no_vote <- c("Niet-stemmers", "Blanco", "Mocht (nog) niet stemmen")

edu_labels <- c("Master/doctorate", "Bachelor", "Propaedeutic year", "HAVO/VWO", "MBO 2–4", "MAVO/VMBO-t",
                "MBO 1/VMBO-b/k or lower")
urb_labels <- c("Very high", "High", "Moderate", "Low", "Not urban")
nielsen_labels <- c("Three largest cities and suburbs", "Rest of the west (NH, ZH, UT)", "North (DR, FR, GR)",
                    "East (OV, GE, FL)", "South (ZE, NB, LI)")

df <- df |>
  mutate(
    sex = fct_recode(sex_rec, Man = "Man", Woman = "Vrouw"),
    age_7 = fct_relabel(haven::as_factor(age_7_rec), \(x) str_replace(x, "-", "–")),
    age_3 = fct_collapse(age_4_rec, `18–34` = "18-34", `35–64` = c("35-49", "50-64"), `65+` = "65+"),
    # MBO 1/VMBO-b/k (7), practical/special education (8) and primary school (9) together;
    # "geen onderwijs" (10) has no respondents
    edu_7 = factor(edu_labels[pmin(as.integer(edu_clean), 7)], levels = edu_labels),
    edu_3 = fct_recode(edu_3_rec, Practical = "Praktisch-opgeleid", Middle = "Middel-opgeleid",
                       Theoretical = "Theoretisch-opgeleid") |> fct_rev(),
    urb_5 = factor(urb_labels[as.integer(urb)], levels = urb_labels),
    urb_3 = factor(c("Urban", "Urban", "Moderate", "Rural", "Rural")[as.integer(urb)],
                   levels = c("Urban", "Moderate", "Rural")),
    province = haven::as_factor(geo_prov_rec) |> fct_drop(),
    region = factor(nielsen_labels[as.integer(geo_nielsen5_rec)], levels = nielsen_labels),
    party = if_else(vote_recall_rec %in% c(no_vote, "Anders", "DENK"), NA_character_, as.character(vote_recall_rec)),
    vote_2 = fct_recode(vote_lr, Left = "left", Right = "right"),
    vote = as.character(vote_recall_rec) |>
      case_match(no_vote ~ "Did not vote / blank",
                 c(small, "Anders") ~ "Other party",
                 .default = as.character(vote_recall_rec))
  )

stopifnot(sum(as.integer(df$edu_clean) == 10) == 0, !anyNA(df$edu_7), !anyNA(df$age_7))

# per scheme the demographics shown; ordinal ones keep their order, nominal ones (ordered = FALSE)
# are ordered by the share choosing a ban
demographics <- list(
  regime = list(
    Sex = list(var = "sex"), Age = list(var = "age_7"), Education = list(var = "edu_7"),
    Urbanisation = list(var = "urb_5"), Province = list(var = "province", ordered = FALSE),
    `Region (Nielsen)` = list(var = "region", ordered = FALSE),
    `Vote (TK 2025, party)` = list(var = "party", ordered = FALSE)
  ),
  option = list(
    Sex = list(var = "sex"), Age = list(var = "age_3"), Education = list(var = "edu_3"),
    Urbanisation = list(var = "urb_3"), `Vote (TK 2025, left/right)` = list(var = "vote_2")
  )
)

demo_notes <- c(
  Education = "Highest completed education; the lowest group includes practical, special and primary education.",
  Urbanisation = "Urbanisation by address density; urban = very high or high, rural = low or not urban.",
  `Region (Nielsen)` = "Nielsen regions; the three largest cities are Amsterdam, Rotterdam and The Hague.",
  `Vote (TK 2025, party)` = "DENK (n = 1), other parties, blank and non-voters not shown.",
  `Vote (TK 2025, left/right)` = paste("Left = GroenLinks-PvdA, D66, SP, PvdD, Volt, DENK, CU; right = PVV, VVD, CDA,",
                                       "JA21, FvD, BBB, 50PLUS, SGP.")
)

# involvement: "Hoe een partij omgaat met drugs en drugscriminaliteit, weegt mee bij mijn
# stemkeuze" (0-100), split at the weighted median into a bottom and a top half
inv <- as.numeric(df$betrokkenheid_3)
inv_order <- order(inv)
inv_median <- inv[inv_order][which(cumsum(df$w[inv_order]) >= sum(df$w[!is.na(inv)]) / 2)[1]]
df <- df |> mutate(involvement = factor(if_else(inv <= inv_median, "bottom", "top"),
                                         levels = c("bottom", "top")))

group_vars <- unique(c(unlist(map(demographics, \(d) map_chr(d, "var"))), "vote"))

# one row per respondent x drug with an answer; "no opinion" (99) is already NA
long <- df |>
  select(w, involvement, all_of(group_vars), all_of(paste0("beleid_voorkeur_", names(drug_labels)))) |>
  mutate(across(all_of(group_vars), as.character)) |>
  pivot_longer(starts_with("beleid_voorkeur_"), names_to = "drug", names_prefix = "beleid_voorkeur_",
               values_to = "x") |>
  filter(!is.na(x)) |>
  mutate(x = as.numeric(x))

stopifnot(all(long$x %in% 1:9))

# weighted % per answer category in each group plus "Total" (first). `order`: the group order;
# NULL keeps the factor levels of `group_var` (ordinal) or, with `by_ban`, orders by the share choosing a ban
answer_dist <- function(drug, group_var, scheme, data = long, order = NULL, by_ban = FALSE) {
  s <- schemes[[scheme]]
  d <- filter(data, drug == .env$drug) |>
    mutate(answer = factor(s$levels[s$code(x)], levels = s$levels))
  d <- bind_rows(mutate(d, category = "Total"), mutate(d, category = .data[[group_var]])) |>
    filter(!is.na(category))
  shares <- d |>
    summarise(w = sum(w), n = n(), .by = c(category, answer)) |>
    complete(category, answer, fill = list(w = 0, n = 0)) |>
    mutate(p = w / sum(w), n = sum(n), .by = category)
  if (is.null(order)) {
    order <- if (by_ban) {
      shares |> filter(answer == s$levels[1]) |> arrange(p) |> pull(category)
    } else {
      levels(factor(df[[group_var]]))
    }
  }
  order <- c("Total", setdiff(intersect(order, shares$category), "Total"))
  n_of <- setNames(shares$n, shares$category)
  # long category names over two lines
  lab <- \(cat, n) sprintf("%s\n(n = %d)", str_wrap(cat, 18), n)
  shares |>
    mutate(label = factor(lab(category, n), levels = lab(order, n_of[order])))
}

`%|%` <- function(x, y) if (is.na(x)) y else x

# `note`: the demographic's note under the title (demo_notes); "" for none
plot_dist <- function(drug, scheme, demo, d, data = long, order = NULL, title_extra = "", order_note = NULL,
                      note = demo_notes[demo] %|% "") {
  s <- schemes[[scheme]]
  nominal <- isFALSE(d$ordered)
  data <- answer_dist(drug, d$var, scheme, data, order, by_ban = nominal)
  n_groups <- n_distinct(data$category)
  # fewer, larger labels when the bars are narrow
  label_min <- if (scheme == "option") 0.02 else 0.01
  label_size <- if (n_groups > 10) 2.3 else if (scheme == "option") 2.7 else 3
  if (is.null(order_note)) {
    order_note <- if (nominal) "Groups ordered by the share choosing a ban, lowest first." else ""
  }
  ggplot(data, aes(label, p, fill = answer)) +
    geom_col(position = position_dodge(width = 0.88), width = 0.84) +
    geom_text(aes(label = if_else(p >= label_min, as.character(round(100 * p)), "")),
              position = position_dodge(width = 0.88), vjust = -0.4, size = label_size) +
    scale_fill_manual(values = setNames(s$colours, s$levels)) +
    scale_y_continuous(labels = scales::label_percent(), limits = c(0, max(data$p) + 0.06),
                       expand = expansion(0)) +
    guides(fill = guide_legend(nrow = if (scheme == "option") 2 else 1, byrow = TRUE)) +
    labs(title = sprintf("%s by %s%s", drug_labels[[drug]],
                         paste0(str_to_lower(str_sub(demo, 1, 1)), str_sub(demo, 2)), title_extra),
         subtitle = str_wrap(str_squish(paste0("Weighted % per ", s$note, "; numbers above the bars in %; no opinion ",
                                               "excluded. ", order_note, " ", note)),
                             width = 175),
         x = NULL, y = "Respondents (weighted %)", fill = NULL) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(size = 8.5),
          axis.text.x = element_text(size = if (n_groups > 10) 7.5 else 9),
          legend.position = "bottom")
}

# each group sums to 100%
for (scheme in names(schemes)) for (d in demographics[[scheme]]) {
  stopifnot(all(abs(summarise(answer_dist("mdma", d$var, scheme), s = sum(p), .by = category)$s - 1) < 1e-9))
}

out_dir <- here::here("analysis", "output", "raw_distributions")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

files <- c(regime = "policy_distributions_regimes.pdf", option = "policy_distributions_options.pdf")
for (scheme in names(schemes)) {
  cairo_pdf(file.path(out_dir, files[[scheme]]), width = 11, height = 7, onefile = TRUE)
  iwalk(demographics[[scheme]], \(d, demo) walk(names(drug_labels), \(drug) print(plot_dist(drug, scheme, demo, d))))
  dev.off()
}


## Party split by involvement: per drug, the bottom half and then the top half (grouped regimes)

vote_grouped <- list(var = "vote", ordered = FALSE)
half_titles <- c(bottom = sprintf(" — involvement bottom half (≤ %s)", inv_median),
                 top    = sprintf(" — involvement top half (> %s)", inv_median))

involvement_pages <- map(names(drug_labels), \(drug) {
  # same party order on both pages: the full-sample order
  order <- answer_dist(drug, "vote", "regime", by_ban = TRUE) |> distinct(category, label) |> arrange(label) |>
    pull(category)
  map(c("bottom", "top"), \(half) {
    plot_dist(drug, "regime", "Vote (TK 2025, party)", vote_grouped, data = filter(long, involvement == half),
              order = order, title_extra = half_titles[[half]], note = "",
              order_note = paste("Involvement: “Hoe een partij omgaat met drugs en drugscriminaliteit, weegt mee",
                                 "bij mijn stemkeuze” (0–100), split at the weighted median. Parties in the",
                                 "full-sample order (share choosing a ban, lowest first); small parties grouped."))
  })
}) |>
  list_flatten()

cairo_pdf(file.path(out_dir, "policy_distributions_involvement.pdf"), width = 11, height = 7, onefile = TRUE)
walk(involvement_pages, print)
dev.off()
