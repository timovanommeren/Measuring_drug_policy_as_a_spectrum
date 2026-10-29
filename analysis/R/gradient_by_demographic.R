library(tidyverse)

source(here::here("analysis", "R", "load_and_clean.R"))

# exploratory: the gradient figure (figure 3a, results_table.R) for each demographic separately.
# Per drug, weighted 100% stacked bars of the nine answer options, one row per category; one page
# per demographic, the categories that go with less support for a ban on top

drug_labels <- c(alcohol = "Alcohol", tabak_en_vapes = "Tobacco and vapes", cannabis = "Cannabis",
                 psychedelica = "Psychedelics", mdma = "MDMA", speed = "Speed", cocaine = "Cocaine",
                 heroine = "Heroin")

# answer options as in descriptives.R (figure 2), with the same colours
option_labels <- c("Ban", "Research only", "Treatment only", "Prescription only", "Specialist shop, non-profit",
                   "Specialist shop, for-profit", "Sold everywhere, no advertising",
                   "Sold everywhere, with advertising", "Free sale")
option_colours <- setNames(viridisLite::viridis(9, end = 0.9), option_labels)

# English labels per category, top row first
party_out <- c("DENK", "Blanco", "Mocht (nog) niet stemmen", "Anders")
demographics <- list(
  Sex = list(var = "sex", labels = c(Vrouw = "Woman", Man = "Man")),
  Age = list(var = "age", labels = c(`18-34` = "18–34", `35-49` = "35–49", `50-64` = "50–64", `65+` = "65+")),
  # highest completed education (edu_clean), highest first; code 10 "geen onderwijs" has no respondents
  Education = list(var = "edu", labels = c(`1` = "Master/doctorate (hbo/wo)", `2` = "Bachelor (hbo/wo)",
                                           `3` = "Propaedeutic year (hbo/wo)", `4` = "HAVO/VWO",
                                           `5` = "MBO 2–4", `6` = "MAVO/VMBO-t, first years HAVO/VWO",
                                           `7` = "MBO 1/VMBO-b/k", `8` = "Practical/special education",
                                           `9` = "Primary school")),
  Urbanisation = list(var = "urb", labels = c(`1` = "Very high", `2` = "High", `3` = "Moderate", `4` = "Low",
                                              `5` = "Not urban")),
  # parties ordered by their weighted share choosing a ban, pooled over the drugs (set below)
  `Vote (TK 2025, party)` = list(var = "party", labels = NULL)
)

stopifnot(sum(as.integer(df$edu_clean) == 10) == 0)

long <- df |>
  transmute(w, sex = as.character(sex_rec), age = as.character(age_4_rec),
            edu = as.character(as.integer(edu_clean)), urb = as.character(as.integer(urb)),
            party = if_else(vote_recall_rec %in% party_out, NA_character_, as.character(vote_recall_rec)),
            across(all_of(paste0("beleid_voorkeur_", names(drug_labels))), as.numeric)) |>
  pivot_longer(starts_with("beleid_voorkeur_"), names_to = "drug", names_prefix = "beleid_voorkeur_",
               values_to = "x") |>
  # "no opinion" (99) is already NA
  filter(!is.na(x)) |>
  mutate(option = factor(option_labels[x], levels = option_labels))

stopifnot(!anyNA(long$option))

party_order <- long |>
  filter(!is.na(party)) |>
  summarise(ban = sum(w[x == 1]) / sum(w), .by = party) |>
  arrange(ban) |>
  pull(party)
demographics$`Vote (TK 2025, party)`$labels <- setNames(party_order, party_order)

gradient <- function(demo) {
  d <- demographics[[demo]]
  long |>
    mutate(row = d$labels[.data[[d$var]]]) |>
    filter(!is.na(row)) |>
    summarise(w = sum(w), .by = c(row, drug, option)) |>
    complete(row, drug, option, fill = list(w = 0)) |>
    mutate(p = w / sum(w), .by = c(row, drug),
           # bottom row first, so the first category is on top
           row = factor(row, levels = rev(unname(d$labels))),
           drug = factor(drug_labels[drug], levels = drug_labels),
           # white text on the dark half of the palette
           txt = if_else(as.integer(option) <= 5, "white", "black"))
}

# respondents per category (unweighted)
row_n <- function(demo) {
  d <- demographics[[demo]]
  v <- c(sex = "sex_rec", age = "age_4_rec", edu = "edu_clean", urb = "urb", party = "vote_recall_rec")[[d$var]]
  key <- if (d$var %in% c("edu", "urb")) as.character(as.integer(df[[v]])) else as.character(df[[v]])
  n <- table(d$labels[key])
  setNames(sprintf("%s (n = %d)", names(n), n), names(n))
}

# each row sums to 100% per drug
for (demo in names(demographics)) {
  stopifnot(all(abs(summarise(gradient(demo), s = sum(p), .by = c(row, drug))$s - 1) < 1e-9))
}

plot_gradient <- function(demo) {
  data <- gradient(demo)
  ggplot(data, aes(p, row, fill = option)) +
    geom_col(position = position_fill(reverse = TRUE), width = 0.8) +
    # group = option keeps the labels stacked like the bars (the colour mapping would regroup them)
    geom_text(aes(label = if_else(p >= 0.1, scales::percent(p, 1), ""), colour = txt, group = option),
              position = position_fill(reverse = TRUE, vjust = 0.5), size = 2.3) +
    facet_wrap(~drug, nrow = 2) +
    scale_fill_manual(values = option_colours, drop = FALSE) +
    scale_colour_identity() +
    scale_x_continuous(labels = c("0", "50%", "100%"), breaks = c(0, 0.5, 1), expand = expansion(0)) +
    scale_y_discrete(labels = row_n(demo)) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(title = sprintf("Preferred policy by %s", str_to_lower(demo) |> str_replace("tk", "TK")),
         subtitle = paste0("Weighted % per drug; labels shown from 10%; geen mening excluded.",
                           if (demo == "Vote (TK 2025, party)")
                             paste(" Parties ordered by their weighted share choosing a ban (all drugs pooled), lowest",
                                   "on top; DENK (n = 1), other parties and blank left out.")
                           else if (demo == "Education")
                             " Highest completed education (edu_clean)."
                           else ""),
         x = NULL, y = NULL, fill = NULL) +
    theme_minimal(base_size = 9) +
    theme(panel.grid = element_blank(),
          strip.text = element_text(face = "bold", size = 10),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(size = 7.5),
          legend.position = "bottom",
          panel.spacing.x = unit(1.4, "lines"),
          plot.margin = margin(10, 15, 10, 10))
}

out_dir <- here::here("analysis", "output", "gradient")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
cairo_pdf(file.path(out_dir, "gradient_by_demographic.pdf"), width = 13, height = 6.5, onefile = TRUE)
walk(names(demographics), \(demo) print(plot_gradient(demo)))
dev.off()
