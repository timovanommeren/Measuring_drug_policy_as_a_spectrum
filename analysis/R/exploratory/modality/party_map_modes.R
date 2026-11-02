# mclust first: it masks purrr::map and dplyr::count, which must win (Mclust needs mclust attached)
library(mclust)
library(tidyverse)
library(patchwork)

source(here::here("analysis", "R", "00_load_and_clean.R"))
source(here::here("analysis", "R", "utils", "party_logos.R"))

# exploratory: alternatives to the party map of figure 4a (party means on illicit vs legal drugs) that
# respect multimodality, and reproducible modality checks with remode. Output in analysis/output/party_maps:
# - party_map_joint_modes.pdf: per party a 2D mixture on the respondents' (x, y); two logos where the data
#   strongly support two camps
# - party_map_quadrant_shares.pdf: each party's respondents by quadrant
# - party_map_density.pdf: weighted 2D density of all respondents, then per party
# - modality_remode.pdf / .csv: remode modes per party x drug and per party on the aggregate axes,
#   compared with the hand ratings (analysis/ratings/modality_ratings.csv)

out_dir <- here::here("analysis", "output", "party_maps")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
set.seed(2026)

source(here::here("analysis", "R", "utils", "party_map_scores.R"))

option_ticks <- c("Ban (1)", "Research only (2)", "Treatment only (3)", "Prescription only (4)",
                  "Specialist shop,\nnon-profit (5)", "Specialist shop,\nfor-profit (6)",
                  "Sold everywhere,\nno advertising (7)", "Sold everywhere,\nwith advertising (8)", "Free sale (9)")

# as in 03_fig_party_map.R: quadrants around the sample means, liberal on the legal drugs (top) or not,
# on the illicit drugs (right) or not
quadrants <- tibble(
  qx = c("left", "right", "left", "right"), qy = c("top", "top", "bottom", "bottom"),
  label = c("Double standards (status quo)", "Liberals", "Abstainers", "Reformers"),
  fill = c("#E69F00", "#009E73", "#56B4E9", "#CC79A7")
)


## Map frame shared by the pages: tinted quadrants split at the sample means, corner labels, dashed lines

quad_frame <- function(xlim, ylim, label_size = 3.2) {
  quad <- quadrants |>
    mutate(xmin = if_else(qx == "left", xlim[1], ref[["x"]]), xmax = if_else(qx == "left", ref[["x"]], xlim[2]),
           ymin = if_else(qy == "bottom", ylim[1], ref[["y"]]), ymax = if_else(qy == "bottom", ref[["y"]], ylim[2]),
           lx = if_else(qx == "left", xlim[1] + 0.01 * diff(xlim), xlim[2] - 0.01 * diff(xlim)),
           ly = if_else(qy == "bottom", ylim[1] + 0.015 * diff(ylim), ylim[2] - 0.015 * diff(ylim)),
           hjust = if_else(qx == "left", 0, 1), vjust = if_else(qy == "bottom", 0, 1))
  list(
    geom_rect(data = quad, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = quad$fill,
              alpha = 0.1, inherit.aes = FALSE),
    geom_text(data = quad, aes(lx, ly, label = label, hjust = hjust, vjust = vjust),
              colour = colorspace::darken(quad$fill, 0.35), fontface = "bold", size = label_size, inherit.aes = FALSE),
    geom_hline(yintercept = ref[["y"]], linetype = "dashed", linewidth = 0.3, colour = "grey50"),
    geom_vline(xintercept = ref[["x"]], linetype = "dashed", linewidth = 0.3, colour = "grey50")
  )
}

map_scales <- function(xlim, ylim) list(
  scale_x_continuous(breaks = 1:9, labels = option_ticks, limits = xlim, oob = scales::oob_keep),
  scale_y_continuous(breaks = 1:9, labels = option_ticks, limits = ylim, oob = scales::oob_keep),
  coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE)
)

map_theme <- theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"),
        axis.text = element_text(lineheight = 0.9), plot.margin = margin(10, 45, 10, 10))

axis_labels <- labs(x = "Mean preferred policy: illicit drugs (cannabis, psychedelics, MDMA, speed, cocaine, heroin)",
                    y = "Mean preferred policy:\nalcohol and tobacco")

pad <- \(r, f = 0.16) r + c(-1, 1) * f * diff(r)
panel_mm <- c(9.3, 4.6) * 25.4

# logos at the given points (columns party, x, y, n), moved apart where they overlap, with a line from
# a point to its logo when it moved
logo_layers <- function(points, xlim, ylim, scale) {
  points <- repel_logos(points, xlim, ylim, panel_mm, scale)
  moved <- filter(points, abs(x - x0) / diff(xlim) * panel_mm[1] > 1 | abs(y - y0) / diff(ylim) * panel_mm[2] > 1)
  list(geom_segment(data = moved, aes(x0, y0, xend = x, yend = y), colour = "grey55", linewidth = 0.25),
       annotation_custom(logo_grob(points, xlim, ylim, scale)))
}


## 1. Joint modes: a 2D Gaussian mixture per party on (x, y)

# a party is drawn twice only with strong BIC support for two components and a smaller component of
# at least 15% (fixed in advance)
bic_threshold <- 10
min_share <- 0.15

fit_party <- function(g) {
  X <- cbind(g$x, g$y)
  wt <- as.vector(as.numeric(g$w) / max(g$w))
  m <- Mclust(X, G = 1:2, modelNames = "VVV", prior = priorControl(), verbose = FALSE)
  bic <- m$BIC[, "VVV"]
  m2 <- Mclust(X, G = 2, modelNames = "VVV", prior = priorControl(), verbose = FALSE)
  # weighted refit from the unweighted classification: weighted centroids and shares
  mw <- me.weighted(X, modelName = "VVV", z = m2$z, weights = wt, prior = priorControl())
  two <- tibble(comp = 1:2, x = mw$parameters$mean[1, ], y = mw$parameters$mean[2, ], share = mw$parameters$pro)
  dbic <- unname(bic[["2"]] - bic[["1"]])
  tibble(n = nrow(g), dbic = dbic,
         split = !is.na(dbic) && dbic > bic_threshold && min(two$share) >= min_share,
         one = list(tibble(comp = 1L, x = weighted.mean(g$x, g$w), y = weighted.mean(g$y, g$w), share = 1)),
         two = list(two))
}

mixtures <- resp |>
  nest(.by = party) |>
  mutate(fit = map(data, fit_party)) |>
  select(party, fit) |>
  unnest(fit)

two_comp <- mixtures |> select(party, n, dbic, split, two) |> unnest(two)
stopifnot(all(abs(summarise(two_comp, s = sum(share), .by = party)$s - 1) < 1e-6))
one_comp <- mixtures |> select(party, one) |> unnest(one) |> left_join(party_means, by = "party", suffix = c("", "_mean"))
stopifnot(all(abs(one_comp$x - one_comp$x_mean) < 1e-6), all(abs(one_comp$y - one_comp$y_mean) < 1e-6))

# drawn: two components for split parties, the mean otherwise; n per component = share of the party
drawn <- bind_rows(
  two_comp |> filter(split) |> transmute(party, comp, x, y, share, n_party = n),
  one_comp |> filter(party %in% mixtures$party[!mixtures$split]) |>
    transmute(party, comp, x, y, share, n_party = n)
) |>
  mutate(n = round(share * n_party))

xlim_modes <- pad(range(c(drawn$x, ref[["x"]]))) |> pmax(0.6) |> pmin(9.4)
ylim_modes <- pad(range(c(drawn$y, ref[["y"]]))) |> pmax(0.6) |> pmin(9.4)
split_parties <- mixtures$party[mixtures$split]

p_modes <- ggplot(drawn, aes(x, y)) +
  quad_frame(xlim_modes, ylim_modes) +
  geom_line(data = filter(drawn, party %in% split_parties), aes(group = party), colour = "grey35",
            linewidth = 0.35, linetype = "dotted") +
  geom_point(data = filter(party_means, party %in% split_parties), colour = "grey40", size = 1.2) +
  logo_layers(select(drawn, party, x, y, n), xlim_modes, ylim_modes, scale = 0.6) +
  map_scales(xlim_modes, ylim_modes) +
  axis_labels +
  labs(title = "Parties by their camps: joint modes of the illicit and legal drug scores",
       subtitle = paste0(
         "Per party a 2D Gaussian mixture (mclust, VVV with prior) on the respondents' mean scores; weighted centroids.\n",
         sprintf("Two logos (dotted line between them) when BIC prefers two components by > %d and the smaller one holds ≥ %d%%; ",
                 bic_threshold, round(100 * min_share)),
         "grey dot = party mean.\nLogo size and n = respondents per component (share × party n). ",
         sprintf("Split: %s. Dashed lines = sample means.", if (length(split_parties)) paste(split_parties, collapse = ", ") else "none")
       )) +
  map_theme

# page 2: every party's two-component solution, faded where the rule does not draw it
xlim_all <- pad(range(c(two_comp$x, ref[["x"]])), 0.08) |> pmax(0.6) |> pmin(9.4)
ylim_all <- pad(range(c(two_comp$y, ref[["y"]])), 0.08) |> pmax(0.6) |> pmin(9.4)
p_forced <- ggplot(two_comp, aes(x, y)) +
  quad_frame(xlim_all, ylim_all) +
  geom_line(aes(group = party, alpha = split), colour = "grey30", linewidth = 0.4) +
  geom_point(aes(size = share * n, alpha = split, colour = split)) +
  ggrepel::geom_text_repel(aes(label = sprintf("%s %d%%", party, round(100 * share)), alpha = split),
                           size = 2.6, min.segment.length = 0.2, seed = 1, max.overlaps = Inf) +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.35), guide = "none") +
  scale_colour_manual(values = c(`TRUE` = "#3b528b", `FALSE` = "grey50"), labels = c(`TRUE` = "drawn twice", `FALSE` = "not drawn twice"),
                      name = NULL) +
  scale_size_area(max_size = 9, guide = "none") +
  map_scales(xlim_all, ylim_all) +
  axis_labels +
  labs(title = "Every party's two-component solution",
       subtitle = paste("Forced two components per party; point size = respondents in the component, label = share.",
                        "\nFaded: BIC or share rule not met, so page 1 shows the party mean.")) +
  map_theme +
  theme(legend.position = "top")


## 2. Quadrant shares

corner_of <- function(x, y) {
  case_when(x >= ref[["x"]] & y >= ref[["y"]] ~ "Liberals",
            x >= ref[["x"]] ~ "Reformers",
            y >= ref[["y"]] ~ "Double standards (status quo)",
            .default = "Abstainers")
}

corner_levels <- c("Reformers", "Liberals", "Double standards (status quo)", "Abstainers")
shares <- bind_rows(mutate(resp, party = "All respondents"), resp) |>
  mutate(corner = factor(corner_of(x, y), levels = corner_levels)) |>
  summarise(w = sum(w), .by = c(party, corner)) |>
  complete(party, corner, fill = list(w = 0)) |>
  mutate(share = w / sum(w), .by = party) |>
  left_join(bind_rows(tibble(party = "All respondents", n = nrow(resp)), select(party_means, party, n)), by = "party")

stopifnot(all(abs(summarise(shares, s = sum(share), .by = party)$s - 1) < 1e-9))

party_order <- shares |>
  filter(corner == "Reformers", party != "All respondents") |>
  arrange(share) |>
  pull(party)
shares <- shares |>
  mutate(label = factor(sprintf("%s (n = %d)", party, n),
                        levels = sprintf("%s (n = %d)", c(party_order, "All respondents"),
                                         n[match(c(party_order, "All respondents"), party)])))

p_shares <- ggplot(shares, aes(share, label, fill = corner)) +
  geom_col(width = 0.75) +
  geom_text(aes(label = if_else(share >= 0.06, scales::percent(share, 1), "")),
            position = position_stack(vjust = 0.5), size = 2.6, colour = "white", fontface = "bold") +
  geom_hline(yintercept = length(party_order) + 0.5, colour = "grey30", linewidth = 0.3) +
  scale_fill_manual(values = setNames(quadrants$fill, quadrants$label)[corner_levels], name = NULL) +
  scale_x_continuous(labels = scales::label_percent(), expand = expansion(0)) +
  labs(title = "Where each party's voters are: share per quadrant",
       subtitle = paste0(sprintf("Respondents classified by their own scores against the sample means (illicit %.2f, legal %.2f), ",
                                 ref[["x"]], ref[["y"]]),
                         "as in figure 4a; weighted.\nOrdered by % Reformers."),
       x = "Respondents (weighted %)", y = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "top", panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

# page 2: the mean map (as figure 4a) next to the bars
xlim_mean <- pad(range(c(party_means$x, ref[["x"]])))
ylim_mean <- pad(range(c(party_means$y, ref[["y"]])))
p_mean <- ggplot(party_means, aes(x, y)) +
  quad_frame(xlim_mean, ylim_mean, label_size = 2.4) +
  logo_layers(party_means, xlim_mean, ylim_mean, scale = 0.45) +
  map_scales(xlim_mean, ylim_mean) +
  labs(x = "Mean preferred policy: illicit drugs", y = "Mean preferred policy:\nalcohol and tobacco") +
  map_theme +
  theme(text = element_text(size = 8), axis.text = element_text(size = 6.5), plot.margin = margin(4, 8, 4, 4))
p_mean_shares <- (p_mean | (p_shares + labs(title = NULL, subtitle = NULL) + theme(text = element_text(size = 8)))) +
  plot_layout(widths = c(1.15, 1)) +
  plot_annotation(tag_levels = "a",
                  title = "Party means (a) and the composition behind them (b)",
                  theme = theme(plot.title = element_text(face = "bold")))


## 3. Weighted 2D density per party

# fixed bandwidth from the full sample, so smoothness does not depend on a party's n
bw <- c(MASS::bandwidth.nrd(resp$x), MASS::bandwidth.nrd(resp$y)) / 4
grid_x <- seq(1, 9, length.out = 120)
grid_y <- seq(1, 9, length.out = 120)

kde_w <- function(x, y, w) {
  kx <- outer(grid_x, x, \(g, v) dnorm(g, v, bw[1]))
  ky <- outer(grid_y, y, \(g, v) dnorm(g, v, bw[2]))
  z <- kx %*% (t(ky) * w / sum(w))
  # z has x in rows; expand_grid varies gy fastest, so read z row by row
  expand_grid(gx = grid_x, gy = grid_y) |> mutate(dens = as.vector(t(z)))
}

facets <- party_means |>
  arrange(desc(n)) |>
  mutate(facet = fct_inorder(sprintf("%s (n = %d)%s", party, n, if_else(n < 40, "\nn < 40, read with care", ""))))

density <- resp |>
  nest(.by = party) |>
  mutate(grid = map(data, \(g) kde_w(g$x, g$y, g$w))) |>
  select(party, grid) |>
  unnest(grid) |>
  mutate(rel = dens / max(dens), .by = party) |>
  left_join(select(facets, party, facet), by = "party")

p_density <- ggplot(density, aes(gx, gy, z = rel)) +
  geom_contour_filled(breaks = c(0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 1.01)) +
  geom_hline(yintercept = ref[["y"]], linetype = "dashed", linewidth = 0.3, colour = "grey30") +
  geom_vline(xintercept = ref[["x"]], linetype = "dashed", linewidth = 0.3, colour = "grey30") +
  geom_point(data = facets, aes(x, y), inherit.aes = FALSE, shape = 4, size = 2, colour = "red3") +
  facet_wrap(~facet, ncol = 4) +
  scale_fill_viridis_d(option = "mako", direction = -1, name = "Density\n(share of the\nparty's peak)",
                       labels = c("5–20%", "20–35%", "35–50%", "50–65%", "65–80%", "80–100%")) +
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9), expand = expansion(0)) +
  scale_y_continuous(breaks = c(1, 3, 5, 7, 9), expand = expansion(0)) +
  coord_fixed() +
  labs(title = "Where each party's voters are: weighted 2D density",
       subtitle = paste("Kernel density of the respondents' mean scores (same bandwidth for every party), scaled to each",
                        "party's peak.\nDashed lines = sample means (quadrant borders); red cross = party mean."),
       x = "Mean preferred policy: illicit drugs (1 = ban … 9 = free sale)",
       y = "Mean preferred policy: alcohol and tobacco") +
  theme_minimal(base_size = 9) +
  theme(panel.grid = element_blank(), strip.text = element_text(face = "bold", size = 7.5),
        panel.spacing = unit(0.9, "lines"),
        plot.title = element_text(face = "bold"), panel.border = element_rect(fill = NA, colour = "grey80"))


# all respondents with both scores (non-voters and other parties included), same bandwidth
density_all <- kde_w(scores$x, scores$y, scores$w) |> mutate(rel = dens / max(dens))
p_density_all <- ggplot(density_all, aes(gx, gy, z = rel)) +
  geom_contour_filled(breaks = c(0.05, 0.2, 0.35, 0.5, 0.65, 0.8, 1.01)) +
  geom_hline(yintercept = ref[["y"]], linetype = "dashed", linewidth = 0.3, colour = "grey30") +
  geom_vline(xintercept = ref[["x"]], linetype = "dashed", linewidth = 0.3, colour = "grey30") +
  geom_text(data = quadrants |> mutate(gx = if_else(qx == "left", 1.1, 8.9), gy = if_else(qy == "bottom", 1.1, 8.9),
                                       hjust = if_else(qx == "left", 0, 1), vjust = if_else(qy == "bottom", 0, 1)),
            aes(gx, gy, label = label, hjust = hjust, vjust = vjust), inherit.aes = FALSE,
            colour = colorspace::darken(quadrants$fill, 0.35), fontface = "bold", size = 3.4) +
  annotate("point", weighted.mean(scores$x, scores$w), weighted.mean(scores$y, scores$w), shape = 4, size = 3,
           colour = "red3") +
  scale_fill_viridis_d(option = "mako", direction = -1, name = "Density\n(share of the peak)",
                       labels = c("5–20%", "20–35%", "35–50%", "50–65%", "65–80%", "80–100%")) +
  scale_x_continuous(breaks = 1:9, labels = option_ticks, expand = expansion(0)) +
  scale_y_continuous(breaks = 1:9, labels = option_ticks, expand = expansion(0)) +
  coord_fixed() +
  labs(title = sprintf("All respondents (n = %d): weighted 2D density", nrow(scores)),
       subtitle = paste("Kernel density of the respondents' mean scores (same bandwidth as the party pages), scaled to the peak;",
                        "all respondents with both scores,\nnon-voters and other parties included.",
                        "Dashed lines = sample means of the map parties (quadrant borders); red cross = mean of all respondents."),
       x = "Mean preferred policy: illicit drugs", y = "Mean preferred policy:\nalcohol and tobacco") +
  theme_minimal(base_size = 9) +
  theme(panel.grid = element_blank(), plot.title = element_text(face = "bold"),
        axis.text = element_text(lineheight = 0.9), panel.border = element_rect(fill = NA, colour = "grey80"))


## 4. Reproducible modality checks with remode

# party groups as in the hand ratings (raw_distributions.R): small parties (n < 30, except Volt) and
# "Anders" together, non-voters and blank together
party_n <- count(df, vote_recall_rec)
small <- setdiff(as.character(party_n$vote_recall_rec[party_n$n < 30]), "Volt")
no_vote <- c("Niet-stemmers", "Blanco", "Mocht (nog) niet stemmen")
answers <- answers |>
  mutate(group = case_match(vote, no_vote ~ "Did not vote / blank", c(small, "Anders") ~ "Other party",
                            .default = vote))

run_remode <- function(counts, test) remode::remode(counts, f_sign_test = test, n_boot = 10000)

# weighted counts rescaled to the unweighted n
weighted_counts <- function(cat, w, k) {
  wc <- tapply(w, factor(cat, levels = seq_len(k)), sum, default = 0)
  round(as.numeric(wc) / sum(wc) * length(cat))
}

# per party group x drug
per_drug <- answers |>
  filter(!is.na(group)) |>
  select(group, w, all_of(names(drug_names))) |>
  pivot_longer(all_of(names(drug_names)), names_to = "drug", values_to = "a") |>
  filter(!is.na(a)) |>
  summarise(counts = list(tabulate(a, 9)), wcounts = list(weighted_counts(a, w, 9)), n = n(), .by = c(group, drug)) |>
  mutate(res_boot = map(counts, \(ct) run_remode(ct, "bootstrap")),
         res_binom = map(counts, \(ct) run_remode(ct, "binomial")),
         res_weighted = map(wcounts, \(ct) run_remode(ct, "bootstrap")),
         modes = map_int(res_boot, "nr_of_modes"),
         modes_binom = map_int(res_binom, "nr_of_modes"),
         modes_weighted = map_int(res_weighted, "nr_of_modes"),
         locations = map_chr(res_boot, \(r) paste(r$mode_indeces, collapse = " ")),
         p_values = map_chr(res_boot, \(r) paste(signif(r$p_values, 2), collapse = " ")),
         bayes_factors = map_chr(res_boot, \(r) paste(signif(r$approx_bayes_factors, 2), collapse = " ")),
         # stability of the number of modes (jackknife; binomial test for speed): % of data that can be
         # removed while most resamples keep the same number of modes
         stable_until = map_dbl(res_binom, \(r) remode::remode_stability(r, iterations = 100, plot = FALSE)$stable_until))

ratings <- read_csv(here::here("analysis", "ratings", "modality_ratings.csv"), show_col_types = FALSE) |>
  transmute(group = party, drug, rated = match(modality, c("unimodal", "bimodal", "trimodal")))
per_drug <- left_join(per_drug, ratings, by = c("group", "drug"))
stopifnot(!anyNA(per_drug$rated), nrow(per_drug) == 104)

agreement <- list(
  confusion = table(remode = per_drug$modes, rated = per_drug$rated),
  confusion_binom = table(remode_binomial = per_drug$modes_binom, rated = per_drug$rated),
  kappa = irr::kappa2(cbind(per_drug$modes, per_drug$rated)),
  kappa_linear = irr::kappa2(cbind(per_drug$modes, per_drug$rated), weight = "equal"),
  boot_vs_binom = mean(per_drug$modes == per_drug$modes_binom),
  boot_vs_weighted = mean(per_drug$modes == per_drug$modes_weighted)
)
print(agreement)

# per party on the aggregate axes (what the map plots), scores binned to whole units [k, k + 1): 8
# categories. Not half steps: y is the mean of 2 answers, and equal answers (whole numbers) are much more
# common than answers one apart (half steps), which makes half-step histograms spiky. A whole-unit bin
# holds both, and for x (sixths) the same number of possible values
unit_bin <- \(s) as.integer(pmin(floor(s), 8))
axis_remode <- function(d, by) {
  d |>
    filter(!is.na(.data[[by]])) |>
    pivot_longer(c(x, y), names_to = "axis", values_to = "s") |>
    summarise(counts = list(tabulate(unit_bin(s), 8)), wcounts = list(weighted_counts(unit_bin(s), w, 8)),
              n = n(), .by = c(all_of(by), axis)) |>
    mutate(res = map(counts, \(ct) run_remode(ct, "bootstrap")),
           modes = map_int(res, "nr_of_modes"),
           modes_weighted = map_int(wcounts, \(ct) run_remode(ct, "bootstrap")$nr_of_modes),
           locations = map_chr(res, \(r) paste0(r$mode_indeces, "–", r$mode_indeces + 1, collapse = " "))) |>
    rename(party = all_of(by))
}

axis_groups <- axis_remode(scores |> left_join(distinct(answers, vote, group), by = "vote"), "group")
axis_parties <- axis_remode(resp, "party")

# averaging check: per party group, modes on the aggregate axis vs the mean over its drugs
averaging <- per_drug |>
  mutate(axis = if_else(drug %in% x_drugs, "x", "y")) |>
  summarise(mean_drug_modes = mean(modes), multimodal_drugs = sum(modes > 1), drugs = n(), .by = c(group, axis)) |>
  left_join(select(axis_groups, group = party, axis, axis_modes = modes, axis_modes_weighted = modes_weighted),
            by = c("group", "axis")) |>
  mutate(pattern = case_when(axis_modes > 1 & mean_drug_modes < 1.5 ~ "axis multimodal, drugs mostly not",
                             axis_modes == 1 & mean_drug_modes >= 1.5 ~ "drugs multimodal, axis not",
                             .default = "consistent"))

# group-level tests with the remode counts (as modality_by_party.R with the hand ratings)
g_stat <- function(x, y) {
  t <- table(x, y)
  e <- outer(rowSums(t), colSums(t)) / sum(t)
  2 * sum(ifelse(t > 0, t * log(t / e), 0))
}
stratified_test <- function(x, y, strata, B = 1e5) {
  obs <- g_stat(x, y)
  null <- replicate(B, g_stat(unsplit(lapply(split(x, strata), sample), strata), y))
  (1 + sum(null >= obs - 1e-9)) / (B + 1)
}
modality_f <- factor(per_drug$modes)
tests <- tibble(
  test = c("Party vs modality (Fisher)", "Drug vs modality (Fisher)",
           "Party given drug (permutation within drug)", "Drug given party (permutation within party)"),
  p = c(fisher.test(table(per_drug$group, modality_f), simulate.p.value = TRUE, B = 1e5)$p.value,
        fisher.test(table(per_drug$drug, modality_f), simulate.p.value = TRUE, B = 1e5)$p.value,
        stratified_test(per_drug$group, modality_f, per_drug$drug),
        stratified_test(per_drug$drug, modality_f, per_drug$group))
)
print(tests)

write_csv(select(per_drug, group, drug, n, rated, modes, modes_binom, modes_weighted, locations, p_values,
                 bayes_factors, stable_until),
          file.path(out_dir, "modality_remode.csv"))
write_csv(select(axis_parties, party, axis, n, modes, modes_weighted, locations), file.path(out_dir, "modality_remode_axes.csv"))
write_csv(averaging, file.path(out_dir, "modality_remode_averaging.csv"))

# figures for the remode checks
table_plot <- function(d, title, subtitle = NULL) {
  d <- mutate(d, across(everything(), as.character))
  long <- d |>
    mutate(row = row_number()) |>
    pivot_longer(-row, names_to = "col", values_to = "value") |>
    mutate(col = factor(col, levels = names(d)))
  ggplot(long, aes(col, -row, label = value)) +
    geom_text(size = 2.8) +
    geom_text(data = tibble(col = factor(names(d), levels = names(d)), row = 0, value = names(d)),
              fontface = "bold", size = 2.8) +
    scale_x_discrete(position = "top") +
    labs(title = title, subtitle = subtitle) +
    theme_void(base_size = 10) +
    theme(plot.title = element_text(face = "bold"), plot.margin = margin(10, 10, 10, 10))
}

drug_summary <- per_drug |>
  summarise(remode = mean(modes), rated = mean(rated), .by = drug) |>
  pivot_longer(c(remode, rated), names_to = "source", values_to = "mean_modes") |>
  mutate(drug = factor(drug_names[drug], levels = drug_names[names(sort(tapply(per_drug$modes, per_drug$drug, mean)))]))

p_agree <- ggplot(drug_summary, aes(mean_modes, drug, colour = source)) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = c(remode = "#0072B2", rated = "#E69F00"), labels = c(rated = "Hand ratings", remode = "remode"),
                      name = NULL) +
  labs(title = "Mean number of modes per drug: remode vs hand ratings",
       subtitle = sprintf(paste("remode (bootstrap test, 10,000 draws, alpha = .05, shape-based) on the raw counts of the 13 party groups.",
                                "\nAgreement with the hand ratings %.0f%%; Cohen's kappa %.2f (linear weighted %.2f).",
                                "\nremode with the binomial test agrees with the bootstrap in %.0f%%, with weighted counts in %.0f%%."),
                          100 * mean(per_drug$modes == per_drug$rated), agreement$kappa$value, agreement$kappa_linear$value,
                          100 * agreement$boot_vs_binom, 100 * agreement$boot_vs_weighted),
       x = "Mean number of modes over the party groups", y = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "top", plot.title = element_text(face = "bold"))

conf_df <- as.data.frame.matrix(agreement$confusion) |>
  rownames_to_column("remode modes") |>
  rename_with(\(x) paste("rated", x), -1)
p_conf <- table_plot(conf_df, "Confusion table: remode (rows) vs hand ratings (columns)") /
  table_plot(mutate(tests, p = signif(p, 2)), "Group-level tests with the remode mode counts",
             "Fisher's exact test (simulated p, 1e5) and stratified permutation tests (G statistic, 1e5 permutations)")

hist_pages <- function(d, title) {
  d <- d |>
    mutate(facet = sprintf("%s — %s (n = %d)\nremode %d, rated %d", group, drug_names[drug], n, modes, rated),
           hist = map2(counts, res_boot, \(ct, r) tibble(option = 1:9, p = ct / sum(ct), mode = option %in% r$mode_indeces))) |>
    select(facet, hist) |>
    unnest(hist)
  facets <- unique(d$facet)
  map(split(facets, ceiling(seq_along(facets) / 20)), \(fs) {
    ggplot(filter(d, facet %in% fs), aes(option, p, fill = mode)) +
      geom_col(width = 0.8) +
      facet_wrap(~facet, ncol = 5) +
      scale_fill_manual(values = c(`TRUE` = "#0072B2", `FALSE` = "grey70"), labels = c(`TRUE` = "remode mode", `FALSE` = "other"),
                        name = NULL) +
      scale_x_continuous(breaks = 1:9) +
      scale_y_continuous(labels = scales::label_percent()) +
      labs(title = title, x = "Preferred policy (1 = ban … 9 = free sale)", y = "Respondents (unweighted %)") +
      theme_minimal(base_size = 8) +
      theme(legend.position = "top", panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))
  })
}

disagree_pages <- hist_pages(filter(per_drug, modes != rated),
                             "Cases where remode and the hand rating disagree (remode modes in blue)")

axis_pages <- map(c(x = "Illicit drugs (x)", y = "Alcohol and tobacco (y)"), \(lab) {
  ax <- if (str_detect(lab, "x\\)")) "x" else "y"
  d <- axis_parties |>
    filter(axis == ax) |>
    mutate(hist = map2(counts, res, \(ct, r) tibble(score = 1:8 + 0.5, p = ct / sum(ct), mode = 1:8 %in% r$mode_indeces)),
    facet = sprintf("%s (n = %d): %d mode%s", party, n, modes, if_else(modes == 1, "", "s"))) |>
    arrange(desc(n)) |>
    mutate(facet = fct_inorder(facet)) |>
    select(facet, hist) |>
    unnest(hist)
  ggplot(d, aes(score, p, fill = mode)) +
    geom_col(width = 0.85) +
    geom_vline(xintercept = ref[[ax]], linetype = "dashed", linewidth = 0.3) +
    facet_wrap(~facet, ncol = 4, scales = "free_y") +
    scale_fill_manual(values = c(`TRUE` = "#0072B2", `FALSE` = "grey70"), labels = c(`TRUE` = "remode mode", `FALSE` = "other"),
                      name = NULL) +
    scale_x_continuous(breaks = 1:9) +
    scale_y_continuous(labels = scales::label_percent()) +
    labs(title = sprintf("Modes of the party distributions on the map axis: %s", lab),
         subtitle = paste("Mean score per respondent binned to whole units (1–2, 2–3, …, 8–9); remode (bootstrap test)",
                          "on the raw counts.\nDashed line = sample mean."),
         x = "Mean preferred policy (1 = ban … 9 = free sale)", y = "Respondents (unweighted %)") +
    theme_minimal(base_size = 8) +
    theme(legend.position = "top", panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))
})

averaging_table <- averaging |>
  arrange(axis, desc(mean_drug_modes)) |>
  transmute(`party group` = group, axis = if_else(axis == "x", "illicit (x)", "legal (y)"),
            `multimodal drugs` = sprintf("%d of %d", multimodal_drugs, drugs),
            `mean modes per drug` = sprintf("%.2f", mean_drug_modes), `modes on axis` = axis_modes,
            `axis, weighted` = axis_modes_weighted, pattern)
p_averaging <- table_plot(averaging_table, "Averaging check: modality per drug vs on the aggregate axis",
                          "remode (bootstrap) on the separate drugs and on the mean score over the drugs of an axis")

# link to the joint-mode map: mixture decision next to the remode modes on both axes
link_table <- mixtures |>
  select(party, n, dbic, split) |>
  left_join(axis_parties |> select(party, axis, modes) |> pivot_wider(names_from = axis, values_from = modes,
                                                                      names_prefix = "remode modes "),
            by = "party") |>
  arrange(desc(n)) |>
  transmute(party, n, `BIC 2 vs 1 component` = sprintf("%.1f", dbic), `drawn twice` = if_else(split, "yes", ""),
            `remode modes, illicit (x)` = `remode modes x`, `remode modes, legal (y)` = `remode modes y`)
p_link <- table_plot(link_table, "Mixture decision per party next to the remode modes on the map axes",
                     sprintf("Drawn twice: BIC difference > %d and smaller component ≥ %d%%", bic_threshold, round(100 * min_share)))


## Write PDFs

pdf_pages <- function(file, plots, width = 11, height = 7) {
  cairo_pdf(file.path(out_dir, file), width = width, height = height, onefile = TRUE)
  walk(plots, print)
  dev.off()
}

pdf_pages("party_map_joint_modes.pdf", list(p_modes, p_forced, p_link))
pdf_pages("party_map_quadrant_shares.pdf", list(p_shares, p_mean_shares))
pdf_pages("party_map_density.pdf", list(p_density_all, p_density), width = 11, height = 10)
pdf_pages("modality_remode.pdf", c(list(p_agree, p_conf), disagree_pages, unname(axis_pages), list(p_averaging)),
          width = 11, height = 8)
