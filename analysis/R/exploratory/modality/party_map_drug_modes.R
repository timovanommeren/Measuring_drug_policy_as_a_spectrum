library(tidyverse)

source(here::here("analysis", "R", "00_load_and_clean.R"))
source(here::here("analysis", "R", "utils", "party_logos.R"))
source(here::here("analysis", "R", "utils", "party_map_scores.R"))

# exploratory: per drug, each party's camps, built from the local maxima marked by hand per party and
# drug (drug_mode_rater.R). Each respondent belongs to the nearest marked maximum of their party (an
# answer exactly between two maxima half to each); a camp is drawn at the weighted mean answer of its
# respondents when it holds at least 10% of the party (weighted); the largest camp is always drawn.
# Output: analysis/output/party_maps/party_map_drug_modes.pdf (one page per rated drug) and
# party_map_drug_modes.csv (every camp, drawn or not)

out_dir <- here::here("analysis", "output", "party_maps")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
ratings_path <- Sys.getenv("DRUG_MODES_CSV", here::here("analysis", "ratings", "drug_mode_ratings.csv"))
min_camp_share <- 0.10

option_ticks <- c("Ban (1)", "Research only (2)", "Treatment only (3)", "Prescription only (4)",
                  "Specialist shop,\nnon-profit (5)", "Specialist shop,\nfor-profit (6)",
                  "Sold everywhere,\nno advertising (7)", "Sold everywhere,\nwith advertising (8)", "Free sale (9)")

# one band per answer option, in the colours of figure 2
option_bands <- tibble(xmin = 1:9 - 0.5, xmax = 1:9 + 0.5, fill = viridisLite::viridis(9, end = 0.9))

stopifnot(file.exists(ratings_path))
ratings <- read_csv(ratings_path, col_types = cols(.default = "c")) |>
  slice_tail(n = 1, by = c(party, drug)) |>
  transmute(party, drug, maxima = map(str_split(maxima, ";"), as.integer))

party_answers <- answers |>
  filter(!is.na(vote), !vote %in% excluded) |>
  rename(party = vote) |>
  pivot_longer(all_of(names(drug_names)), names_to = "drug", values_to = "a") |>
  filter(!is.na(a))

# membership of each answer in the nearest maxima (ties shared equally)
membership <- function(a, maxima) {
  d <- outer(a, maxima, \(x, m) abs(x - m))
  b <- d <= apply(d, 1, min) + 1e-9
  b / rowSums(b)
}

camps <- party_answers |>
  inner_join(ratings, by = c("party", "drug")) |>
  nest(.by = c(party, drug, maxima)) |>
  mutate(camps = map2(data, maxima, \(g, m) {
    M <- membership(g$a, m)
    tibble(maximum = m,
           share = colSums(M * g$w) / sum(g$w),
           centre = colSums(M * g$w * g$a) / colSums(M * g$w),
           n_frac = colSums(M))
  })) |>
  select(party, drug, camps) |>
  unnest(camps) |>
  filter(share > 0) |>
  mutate(drawn = share >= min_camp_share | share == max(share), .by = c(party, drug))

stopifnot(all(abs(summarise(camps, s = sum(share), .by = c(party, drug))$s - 1) < 1e-9))

party_drug_means <- party_answers |>
  summarise(mean = weighted.mean(a, w), n = n(), .by = c(party, drug))

# one maximum: the camp is the whole party, drawn at its mean
single <- camps |> filter(n() == 1, .by = c(party, drug)) |> left_join(party_drug_means, by = c("party", "drug"))
stopifnot(all(abs(single$centre - single$mean) < 1e-9))

plot_drug <- function(dr) {
  cp <- filter(camps, drug == dr, drawn)
  means <- filter(party_drug_means, drug == dr, party %in% cp$party) |> arrange(mean)
  rows <- tibble(party = means$party, row = seq_along(means$party))
  cp <- left_join(cp, rows, by = "party") |> mutate(n = round(n_frac))
  means <- left_join(means, rows, by = "party")
  spans <- cp |> summarise(lo = min(centre), hi = max(centre), row = first(row), .by = party) |> filter(hi > lo)
  xlim <- c(0.5, 9.5)
  ylim <- c(0.4, nrow(rows) + 0.7)
  n_rated <- n_distinct(cp$party)
  ggplot() +
    geom_rect(data = option_bands, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = option_bands$fill,
              alpha = 0.1) +
    geom_hline(data = rows, aes(yintercept = row), colour = "grey90", linewidth = 0.3) +
    geom_segment(data = spans, aes(lo, row, xend = hi, yend = row), colour = "grey45", linewidth = 0.4,
                 linetype = "dotted") +
    geom_point(data = means, aes(mean, row), colour = "grey40", size = 1.3) +
    annotation_custom(logo_grob(transmute(cp, party, x = centre, y = row, n), xlim, ylim, scale = 0.42)) +
    geom_text(data = cp, aes(centre, row + 0.38, label = scales::percent(share, 1)), size = 2.4, colour = "grey25") +
    scale_x_continuous(breaks = 1:9, labels = option_ticks) +
    scale_y_continuous(breaks = rows$row, labels = rows$party) +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(title = sprintf("Camps per party: %s", drug_names[[dr]]),
         subtitle = paste0(
           "Local maxima marked by hand per party; each respondent belongs to the nearest maximum.\n",
           sprintf("A camp (logo, share above it) holds ≥ %d%% of the party and is drawn at the weighted mean answer of its respondents. ",
                   round(100 * min_camp_share)),
           "Grey dot = party mean,\ndotted line between a party's camps. Logo size and n = respondents per camp.\n",
           sprintf("Parties ordered by their mean answer; %d of %d parties rated. Bands: the 9 answer options, coloured as in figure 2.",
                   n_rated, n_distinct(party_answers$party))),
         x = "Preferred policy", y = NULL) +
    theme_minimal(base_size = 10) +
    theme(panel.grid = element_blank(), plot.title = element_text(face = "bold"),
          axis.text.x = element_text(lineheight = 0.9), plot.margin = margin(10, 20, 10, 10))
}

write_csv(camps, file.path(out_dir, "party_map_drug_modes.csv"))

drugs_rated <- intersect(names(drug_names), unique(camps$drug))
cairo_pdf(file.path(out_dir, "party_map_drug_modes.pdf"), width = 11, height = 8.5, onefile = TRUE)
walk(drugs_rated, \(dr) print(plot_drug(dr)))
dev.off()
