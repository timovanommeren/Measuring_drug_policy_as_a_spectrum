library(tidyverse)

source(here::here("analysis", "R", "00_load_and_clean.R"))
source(here::here("analysis", "R", "utils", "party_logos.R"))
source(here::here("analysis", "R", "utils", "party_map_scores.R"))

# exploratory: party maps for every pair of an illicit drug (x) and a legal drug (y), with each party's
# joint camps. Per drug the camps of party_map_drug_modes.R: the local maxima marked by hand
# (drug_mode_rater.R) whose camp holds at least 10% of the party. On a pair, each respondent belongs to
# the nearest of those maxima on both drugs (an answer exactly between two maxima half to each); a joint
# camp is a combination of an x and a y maximum holding at least 10% of the party (weighted; the largest
# is always drawn), drawn at the weighted mean answers of its respondents.
# Output: analysis/output/party_maps/party_map_pair_modes.pdf (12 pages: all parties per pair),
# party_map_pair_modes_by_party.pdf and _by_pair.pdf (one page per party per pair, ordered by party or by
# pair) and party_map_pair_modes.csv

out_dir <- here::here("analysis", "output", "party_maps")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
ratings_path <- Sys.getenv("DRUG_MODES_CSV", here::here("analysis", "ratings", "drug_mode_ratings.csv"))
min_share <- 0.10

illicit <- c("cannabis", "psychedelica", "mdma", "cocaine", "speed", "heroine")
legal <- c("alcohol", "tabak_en_vapes")

option_ticks <- c("Ban (1)", "Research only (2)", "Treatment only (3)", "Prescription only (4)",
                  "Specialist shop,\nnon-profit (5)", "Specialist shop,\nfor-profit (6)",
                  "Sold everywhere,\nno advertising (7)", "Sold everywhere,\nwith advertising (8)", "Free sale (9)")

# as in 03_fig_party_map.R: quadrants around the sample means
quadrants <- tibble(
  qx = c("left", "right", "left", "right"), qy = c("top", "top", "bottom", "bottom"),
  label = c("Double standards (status quo)", "Liberals", "Abstainers", "Reformers"),
  fill = c("#E69F00", "#009E73", "#56B4E9", "#CC79A7")
)

ratings <- read_csv(ratings_path, col_types = cols(.default = "c")) |>
  slice_tail(n = 1, by = c(party, drug)) |>
  transmute(party, drug, maxima = map(str_split(maxima, ";"), as.integer))

party_resp <- answers |>
  filter(!is.na(vote), !vote %in% excluded) |>
  rename(party = vote)

# membership of each value in the nearest maxima (ties shared equally)
membership <- function(a, maxima) {
  d <- outer(a, maxima, \(x, m) abs(x - m))
  b <- d <= apply(d, 1, min) + 1e-9
  b / rowSums(b)
}

# per party and drug: the marked maxima whose camp holds >= 10% (or is the largest), as on the per-drug maps
drawn_maxima <- party_resp |>
  pivot_longer(all_of(names(drug_names)), names_to = "drug", values_to = "a") |>
  filter(!is.na(a)) |>
  inner_join(ratings, by = c("party", "drug")) |>
  nest(.by = c(party, drug, maxima)) |>
  mutate(maxima = map2(data, maxima, \(g, m) {
    share <- colSums(membership(g$a, m) * g$w) / sum(g$w)
    m[share >= min_share | share == max(share)]
  })) |>
  select(party, drug, maxima)

stopifnot(nrow(drawn_maxima) == n_distinct(party_resp$party) * length(drug_names))

pair_cells <- function(xd, yd) {
  party_resp |>
    filter(!is.na(.data[[xd]]), !is.na(.data[[yd]])) |>
    transmute(party, w, a_x = .data[[xd]], a_y = .data[[yd]]) |>
    nest(.by = party) |>
    left_join(filter(drawn_maxima, drug == xd) |> select(party, mx = maxima), by = "party") |>
    left_join(filter(drawn_maxima, drug == yd) |> select(party, my = maxima), by = "party") |>
    mutate(cells = pmap(list(data, mx, my), \(g, mx, my) {
      Mx <- membership(g$a_x, mx)
      My <- membership(g$a_y, my)
      expand_grid(i = seq_along(mx), j = seq_along(my)) |>
        pmap(\(i, j) {
          cw <- g$w * Mx[, i] * My[, j]
          tibble(max_x = mx[i], max_y = my[j], share = sum(cw) / sum(g$w), x = sum(cw * g$a_x) / sum(cw),
                 y = sum(cw * g$a_y) / sum(cw), n_frac = sum(Mx[, i] * My[, j]))
        }) |>
        list_rbind()
    })) |>
    select(party, cells) |>
    unnest(cells) |>
    filter(share > 0) |>
    mutate(drawn = share >= min_share | share == max(share), .by = party) |>
    mutate(x_drug = xd, y_drug = yd)
}

pairs <- expand_grid(y_drug = legal, x_drug = illicit)
cells <- pmap(pairs, \(y_drug, x_drug) pair_cells(x_drug, y_drug)) |> list_rbind()
stopifnot(all(abs(summarise(cells, s = sum(share), .by = c(party, x_drug, y_drug))$s - 1) < 1e-9))

write_csv(cells, file.path(out_dir, "party_map_pair_modes.csv"))

panel_mm <- c(9.2, 5.9) * 25.4

plot_pair <- function(xd, yd) {
  cl <- filter(cells, x_drug == xd, y_drug == yd)
  camps <- filter(cl, drawn) |> mutate(n = round(n_frac))
  # party means and the quadrant borders (sample means) over the respondents who answered both drugs
  both <- party_resp |> filter(!is.na(.data[[xd]]), !is.na(.data[[yd]]))
  means <- both |> summarise(mx = weighted.mean(.data[[xd]], w), my = weighted.mean(.data[[yd]], w), .by = party)
  ref <- c(x = weighted.mean(both[[xd]], both$w), y = weighted.mean(both[[yd]], both$w))
  multi <- camps |> filter(n() > 1, .by = party) |> distinct(party) |> pull(party)
  spokes <- camps |> filter(party %in% multi) |> left_join(means, by = "party")
  xlim <- c(0.5, 9.5)
  ylim <- c(0.5, 9.5)
  pts <- repel_logos(select(camps, party, x, y, n), xlim, ylim, panel_mm, scale = 0.42)
  moved <- filter(pts, abs(x - x0) / diff(xlim) * panel_mm[1] > 1 | abs(y - y0) / diff(ylim) * panel_mm[2] > 1)
  quad <- quadrants |>
    mutate(xmin = if_else(qx == "left", xlim[1], ref[["x"]]), xmax = if_else(qx == "left", ref[["x"]], xlim[2]),
           ymin = if_else(qy == "bottom", ylim[1], ref[["y"]]), ymax = if_else(qy == "bottom", ref[["y"]], ylim[2]),
           lx = if_else(qx == "left", xlim[1] + 0.1, xlim[2] - 0.1), ly = if_else(qy == "bottom", ylim[1] + 0.1, ylim[2] - 0.1),
           hjust = if_else(qx == "left", 0, 1), vjust = if_else(qy == "bottom", 0, 1))
  counts <- count(camps, party) |> count(n, name = "parties")
  ggplot() +
    geom_rect(data = quad, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = quad$fill, alpha = 0.1) +
    geom_text(data = quad, aes(lx, ly, label = label, hjust = hjust, vjust = vjust),
              colour = colorspace::darken(quad$fill, 0.35), fontface = "bold", size = 3.2) +
    geom_hline(yintercept = ref[["y"]], linetype = "dashed", linewidth = 0.3, colour = "grey50") +
    geom_vline(xintercept = ref[["x"]], linetype = "dashed", linewidth = 0.3, colour = "grey50") +
    geom_segment(data = spokes, aes(mx, my, xend = x, yend = y), colour = "grey45", linewidth = 0.3, linetype = "dotted") +
    geom_point(data = filter(means, party %in% multi), aes(mx, my), colour = "grey40", size = 1.1) +
    geom_segment(data = moved, aes(x0, y0, xend = x, yend = y), colour = "grey60", linewidth = 0.25) +
    annotation_custom(logo_grob(pts, xlim, ylim, scale = 0.42)) +
    scale_x_continuous(breaks = 1:9, labels = option_ticks) +
    scale_y_continuous(breaks = 1:9, labels = option_ticks) +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(title = sprintf("Party camps: %s (x) vs %s (y)", drug_names[[xd]], drug_names[[yd]]),
         subtitle = paste0(
           "Per drug the local maxima marked by hand whose camp holds ≥ 10% of the party; each respondent belongs to the nearest one on both drugs.\n",
           "A joint camp (logo) = an x and a y maximum together holding ≥ 10% of the party, at the weighted mean answers of its respondents. ",
           "Grey dot = party mean,\ndotted lines to its camps. Logo size and n = respondents per camp. Dashed lines = sample means.\n",
           "Camps per party: ", paste(sprintf("%d (%d parties)", counts$n, counts$parties), collapse = ", "), "."),
         x = sprintf("Preferred policy: %s", drug_names[[xd]]), y = sprintf("Preferred policy: %s", drug_names[[yd]])) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"),
          axis.text = element_text(lineheight = 0.9, size = 7.5), plot.margin = margin(10, 20, 10, 10))
}

cairo_pdf(file.path(out_dir, "party_map_pair_modes.pdf"), width = 11, height = 8.5, onefile = TRUE)
pwalk(pairs, \(y_drug, x_drug) print(plot_pair(x_drug, y_drug)))
dev.off()

# one page per party per pair: the party's joint camps on the full scale, with its respondents as faint
# jittered points behind them; parties by size, then the pairs in the order above
plot_party_pair <- function(pr, xd, yd) {
  camps <- filter(cells, party == pr, x_drug == xd, y_drug == yd, drawn) |> mutate(n = round(n_frac))
  pts_resp <- party_resp |> filter(party == pr, !is.na(.data[[xd]]), !is.na(.data[[yd]])) |>
    transmute(x = .data[[xd]], y = .data[[yd]])
  both <- party_resp |> filter(!is.na(.data[[xd]]), !is.na(.data[[yd]]))
  ref <- c(x = weighted.mean(both[[xd]], both$w), y = weighted.mean(both[[yd]], both$w))
  pm <- party_resp |> filter(party == pr, !is.na(.data[[xd]]), !is.na(.data[[yd]])) |>
    summarise(mx = weighted.mean(.data[[xd]], w), my = weighted.mean(.data[[yd]], w))
  xlim <- c(0.5, 9.5)
  ylim <- c(0.5, 9.5)
  pts <- repel_logos(select(camps, party, x, y, n), xlim, ylim, panel_mm, scale = 0.9)
  quad <- quadrants |>
    mutate(xmin = if_else(qx == "left", xlim[1], ref[["x"]]), xmax = if_else(qx == "left", ref[["x"]], xlim[2]),
           ymin = if_else(qy == "bottom", ylim[1], ref[["y"]]), ymax = if_else(qy == "bottom", ref[["y"]], ylim[2]),
           lx = if_else(qx == "left", xlim[1] + 0.1, xlim[2] - 0.1), ly = if_else(qy == "bottom", ylim[1] + 0.1, ylim[2] - 0.1),
           hjust = if_else(qx == "left", 0, 1), vjust = if_else(qy == "bottom", 0, 1))
  ggplot() +
    geom_rect(data = quad, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = quad$fill, alpha = 0.1) +
    geom_text(data = quad, aes(lx, ly, label = label, hjust = hjust, vjust = vjust),
              colour = colorspace::darken(quad$fill, 0.35), fontface = "bold", size = 3.2) +
    geom_hline(yintercept = ref[["y"]], linetype = "dashed", linewidth = 0.3, colour = "grey50") +
    geom_vline(xintercept = ref[["x"]], linetype = "dashed", linewidth = 0.3, colour = "grey50") +
    geom_point(data = pts_resp, aes(x, y), position = position_jitter(width = 0.25, height = 0.25, seed = 1),
               size = 0.7, alpha = 0.25, colour = "grey20") +
    geom_segment(data = if (nrow(camps) > 1) camps else camps[0, ], aes(pm$mx, pm$my, xend = x, yend = y),
                 colour = "grey35", linewidth = 0.35, linetype = "dotted") +
    geom_point(data = pm, aes(mx, my), colour = "grey30", size = 1.6) +
    annotation_custom(logo_grob(pts, xlim, ylim, scale = 0.9)) +
    geom_text(data = pts, aes(x, y + 0.75, label = sprintf("%s (max %d / %d)", scales::percent(camps$share, 1),
                                                           camps$max_x, camps$max_y)),
              size = 3, colour = "grey15") +
    scale_x_continuous(breaks = 1:9, labels = option_ticks) +
    scale_y_continuous(breaks = 1:9, labels = option_ticks) +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(title = sprintf("%s: %s (x) vs %s (y)", pr, drug_names[[xd]], drug_names[[yd]]),
         subtitle = paste0(
           sprintf("%d joint camp%s (share of the party and the maxima on x / y).\n", nrow(camps), if_else(nrow(camps) == 1, "", "s")),
           "Camps as on the overview pages: per drug the hand-marked maxima holding ≥ 10%, joint camps ≥ 10%.",
           sprintf("\nGrey points = the party's %d respondents (jittered); grey dot = party mean; dashed lines = sample means.",
                   nrow(pts_resp))),
         x = sprintf("Preferred policy: %s", drug_names[[xd]]), y = sprintf("Preferred policy: %s", drug_names[[yd]])) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"),
          axis.text = element_text(lineheight = 0.9, size = 7.5), plot.margin = margin(10, 20, 10, 10))
}

party_order <- party_means |> arrange(desc(n)) |> pull(party)
cairo_pdf(file.path(out_dir, "party_map_pair_modes_by_party.pdf"), width = 11, height = 8.5, onefile = TRUE)
for (pr in party_order) pwalk(pairs, \(y_drug, x_drug) print(plot_party_pair(pr, x_drug, y_drug)))
dev.off()

# the same pages ordered by pair, then the parties by size
cairo_pdf(file.path(out_dir, "party_map_pair_modes_by_pair.pdf"), width = 11, height = 8.5, onefile = TRUE)
pwalk(pairs, \(y_drug, x_drug) for (pr in party_order) print(plot_party_pair(pr, x_drug, y_drug)))
dev.off()

# camps per party per pair
print(cells |> filter(drawn) |> count(x_drug, y_drug, party) |> count(n, name = "party_pairs"))
