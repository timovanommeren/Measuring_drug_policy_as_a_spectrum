library(tidyverse)

# Tests, tables and figures for the involvement moderation run (moderation_involvement.R).
# Usage: Rscript moderation_involvement_summary.R [out_subdir]

args <- commandArgs(trailingOnly = TRUE)
out_dir <- here::here("analysis", "output", if (length(args) >= 1) args[1] else "involvement_moderation")
r <- readRDS(file.path(out_dir, "moderation_involvement.rds"))
meta <- r$meta

parse_keys <- function(x) {
  tibble(key = names(x), value = unname(x)) |>
    separate_wider_delim(key, "|", names = c("drug", "item", "family", "group", "stat"), cols_remove = FALSE)
}

ok <- \(m) m[!is.na(m[, 1]), , drop = FALSE]
boot <- ok(r$boot)
perm <- ok(r$perm)
sim <- ok(r$sim)
failed <- c(boot = nrow(r$boot) - nrow(boot), perm = nrow(r$perm) - nrow(perm), sim = nrow(r$sim) - nrow(sim))

z_mde <- \(alpha) qnorm(1 - alpha / 2) + qnorm(0.8)

# estimate, bootstrap CI and SE, and the null distribution (permutation or simulation)
summarise_stats <- function(keys, est, boot, null) {
  tibble(key = keys, estimate = est[keys],
         lo = apply(boot[, keys, drop = FALSE], 2, quantile, 0.025),
         hi = apply(boot[, keys, drop = FALSE], 2, quantile, 0.975),
         se = apply(boot[, keys, drop = FALSE], 2, sd),
         null_mean = colMeans(null[, keys, drop = FALSE]),
         null_sd = apply(null[, keys, drop = FALSE], 2, sd),
         p_empirical = map_dbl(keys, \(k) {
           dev <- abs(null[, k] - mean(null[, k]))
           (1 + sum(dev >= abs(est[[k]] - mean(null[, k])))) / (nrow(null) + 1)
         })) |>
    mutate(z = (estimate - null_mean) / null_sd,
           p = 2 * pnorm(-abs(z)))
}

all_keys <- parse_keys(r$point)


## Primary: per item, change in D averaged over the 8 groups of the 4 primary contrasts and 8 drugs

primary_groups <- meta$contrasts |>
  filter(primary) |>
  pivot_longer(c(first, second), values_to = "level") |>
  transmute(var, group = paste0(var, ":", level))

primary_weights <- map(names(meta$item_labels), \(it) {
  sel <- all_keys |>
    filter(item == it, family == "group", stat == "dD", group %in% primary_groups$group,
           !(it == "betrokkenheid_3" & str_starts(group, "vote:")))
  setNames(rep(1 / nrow(sel), nrow(sel)), sel$key)
}) |>
  set_names(names(meta$item_labels))

combine <- \(x, wts) if (is.null(dim(x))) sum(x[names(wts)] * wts) else as.vector(x[, names(wts)] %*% wts)

primary <- imap(primary_weights, \(wts, it) {
  est <- combine(r$point, wts)
  b <- combine(boot, wts)
  n0 <- combine(perm, wts)
  tibble(item = it, item_label = meta$item_labels[[it]], n_terms = length(wts), estimate = est,
         lo = quantile(b, 0.025), hi = quantile(b, 0.975), se = sd(b),
         null_mean = mean(n0), null_sd = sd(n0),
         p_empirical = (1 + sum(abs(n0 - mean(n0)) >= abs(est - mean(n0)))) / (length(n0) + 1))
}) |>
  list_rbind() |>
  mutate(z = (estimate - null_mean) / null_sd,
         p = 2 * pnorm(-abs(z)),
         p_holm = p.adjust(p, "holm"),
         mde_05 = z_mde(0.05) * se,
         mde_holm = z_mde(0.05 / n()) * se)


## Secondary families (BH within each family)

families <- list(
  "Within-group polarisation (dD)"   = list(family = "group",    stat = "dD",      null = perm),
  "Within-group bimodality (dA)"     = list(family = "group",    stat = "dA",      null = perm),
  "Unequal polarisation (dD B - A)"  = list(family = "contrast", stat = "dD_diff", null = sim),
  "Gap in mean position (dgap)"      = list(family = "contrast", stat = "dgap",    null = sim),
  "Age x vote cells (dD)"            = list(family = "cell",     stat = "dD",      null = perm)
)

secondary <- imap(families, \(f, nm) {
  keys <- all_keys |> filter(family == f$family, stat == f$stat) |> pull(key)
  summarise_stats(keys, r$point, boot, f$null) |>
    mutate(test_family = nm,
           q = p.adjust(p, "BH"),
           mde_05 = z_mde(0.05) * se,
           mde_bonf = z_mde(0.05 / n()) * se)
}) |>
  list_rbind() |>
  left_join(select(all_keys, -value), by = "key")

# the additive model's implied change (simulation null mean) vs the interaction part
secondary <- secondary |>
  mutate(interaction_part = if_else(family == "contrast", estimate - null_mean, NA_real_))

# sign flips of the gap between low and high involvement
gaps <- all_keys |>
  filter(family == "contrast", stat %in% c("gap_lo", "gap_hi")) |>
  select(drug, item, group, stat, value) |>
  pivot_wider(names_from = stat, values_from = value) |>
  mutate(sign_flip = sign(gap_lo) != sign(gap_hi))

whole <- all_keys |>
  filter(family == "whole") |>
  select(drug, item, stat, value) |>
  pivot_wider(names_from = stat, values_from = value)


## Robustness: primary per variant and agreement of the 400 group dD with the main run

robust <- imap(r$robust, \(x, variant) {
  k <- parse_keys(x)
  main_dD <- all_keys |> filter(family == "group", stat == "dD") |> select(key, main = value)
  rob_dD <- k |> filter(family == "group", stat == "dD") |> select(key, variant_value = value)
  cmp <- inner_join(main_dD, rob_dD, by = "key")
  conv <- k |> filter(family == "converged")
  tibble(variant = variant, item = names(primary_weights),
         primary = map_dbl(primary_weights, \(w) combine(x, w)),
         cor_group_dD = cor(cmp$main, cmp$variant_value),
         same_sign_group_dD = mean(sign(cmp$main) == sign(cmp$variant_value)),
         not_converged = if (nrow(conv) > 0) sum(conv$value == 0) else NA_integer_)
}) |>
  list_rbind()


## Write tables

write_csv(primary, file.path(out_dir, "primary.csv"))
write_csv(secondary, file.path(out_dir, "secondary.csv"))
write_csv(gaps, file.path(out_dir, "gaps_sign_flips.csv"))
write_csv(whole, file.path(out_dir, "whole_sample.csv"))
write_csv(robust, file.path(out_dir, "robustness.csv"))


## Report

fmt <- \(x, d = 3) formatC(x, format = "f", digits = d)
sig_counts <- secondary |>
  # hypothesised: more dispersion, less agreement (more bimodal), larger differences between groups
  mutate(hyp = if_else(stat == "dA", estimate < null_mean, estimate > null_mean)) |>
  summarise(tests = n(), q_05 = sum(q < 0.05), positive = sum(q < 0.05 & hyp),
            median_mde_05 = median(mde_05), .by = test_family)

report <- c(
  "# Involvement moderation: summary",
  "",
  sprintf("Draws: %d bootstrap, %d permutations, %d simulations (failed: %d / %d / %d). Runtime %.0f min.",
          nrow(boot), nrow(perm), nrow(sim), failed[["boot"]], failed[["perm"]], failed[["sim"]], meta$runtime_min),
  sprintf("Respondents: %d. Low/high involvement = weighted P25/P75 of each item.", meta$n_respondents),
  "",
  "Merged answer categories per drug:",
  imap_chr(meta$merges, \(m, dr) sprintf("- %s: %s", meta$drug_labels[[dr]], paste(m, collapse = ", "))),
  "",
  "## Primary (Holm over 5 items)",
  "Change in Leik's D from low to high involvement, averaged over the 8 groups of age, education,",
  "urbanisation and vote (item 3: vote groups excluded, already seen) and 8 drugs. Null: involvement",
  "permuted within demographic cells.",
  "",
  "| Item | dD | 95% CI | null mean | p | p (Holm) | MDE (.05) | MDE (Holm) |",
  "|---|---|---|---|---|---|---|---|",
  pmap_chr(primary, \(item_label, estimate, lo, hi, null_mean, p, p_holm, mde_05, mde_holm, ...) {
    sprintf("| %s | %s | [%s, %s] | %s | %s | %s | %s | %s |", item_label, fmt(estimate), fmt(lo), fmt(hi),
            fmt(null_mean), fmt(p, 4), fmt(p_holm, 4), fmt(mde_05), fmt(mde_holm))
  }),
  "",
  "## Secondary families (BH, q < .05)",
  "",
  "| Family | tests | q < .05 | in hypothesised direction | median MDE (.05) |",
  "|---|---|---|---|---|",
  pmap_chr(sig_counts, \(test_family, tests, q_05, positive, median_mde_05) {
    sprintf("| %s | %d | %d | %d | %s |", test_family, tests, q_05, positive, fmt(median_mde_05))
  }),
  "",
  sprintf("Gap sign flips between low and high involvement: %d of %d contrasts.", sum(gaps$sign_flip), nrow(gaps)),
  "",
  "## Robustness (primary estimate per variant)",
  "",
  "| Variant | Item | primary dD | cor. with main (group dD) | same sign | not converged |",
  "|---|---|---|---|---|---|",
  pmap_chr(robust, \(variant, item, primary, cor_group_dD, same_sign_group_dD, not_converged) {
    sprintf("| %s | %s | %s | %s | %s | %s |", variant, meta$item_labels[[item]], fmt(primary), fmt(cor_group_dD, 2),
            fmt(same_sign_group_dD, 2), not_converged)
  })
)
writeLines(report, file.path(out_dir, "report.md"))


## Figures

item_f <- \(x) factor(meta$item_labels[x], levels = meta$item_labels)
drug_f <- \(x) factor(meta$drug_labels[x], levels = meta$drug_labels)
group_levels <- meta$contrasts |>
  pivot_longer(c(first, second), values_to = "level") |>
  transmute(group = paste0(var, ":", level)) |>
  pull(group)

pdf_pages <- function(file, plots, width = 11, height = 7.5) {
  cairo_pdf(file.path(out_dir, file), width = width, height = height, onefile = TRUE)
  walk(plots, print)
  dev.off()
}

base_theme <- theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

# 1. primary
p_primary <- ggplot(primary, aes(estimate, fct_rev(item_f(item)))) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_errorbarh(aes(xmin = null_mean - 1.96 * null_sd, xmax = null_mean + 1.96 * null_sd), height = 0.3,
                 colour = "grey70", linewidth = 2) +
  geom_pointrange(aes(xmin = lo, xmax = hi)) +
  geom_text(aes(label = sprintf("p(Holm) = %s", fmt(p_holm))), nudge_y = 0.3, size = 2.8) +
  labs(title = "Primary: does involvement polarise opinion within demographic groups?",
       subtitle = paste("Change in Leik's ordinal dispersion D from low (P25) to high (P75) involvement, averaged over",
                        "8 demographic groups and 8 drugs.\nBlack: estimate with 95% bootstrap CI. Grey: 95% range",
                        "under the null (involvement permuted within demographic cells)."),
       x = "Change in D (0 = consensus, 1 = split between the extremes)", y = NULL) +
  base_theme

# 2. heatmap of within-group dD
heat <- secondary |>
  filter(test_family == "Within-group polarisation (dD)") |>
  mutate(group = factor(group, levels = rev(group_levels)), drug = drug_f(drug), item = item_f(item),
         mark = case_when(q < 0.05 ~ "*", .default = ""))
lim <- max(abs(heat$estimate - heat$null_mean))
p_heat <- ggplot(heat, aes(drug, group, fill = estimate - null_mean)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = mark), size = 4, vjust = 0.75) +
  facet_wrap(~item, ncol = 3) +
  scale_fill_gradient2(low = "#2166AC", high = "#B2182B", limits = c(-lim, lim), name = "dD minus\nnull mean") +
  labs(title = "Within-group polarisation: change in D from low to high involvement",
       subtitle = "Relative to the permutation null; * = q < .05 (Benjamini–Hochberg over all 400 tests)",
       x = NULL, y = NULL) +
  base_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. spline curves of D per item
spline <- all_keys |>
  filter(family == "spline", str_starts(stat, "D_")) |>
  mutate(pct = as.integer(str_remove(stat, "D_p")),
         lo = apply(boot[, key, drop = FALSE], 2, quantile, 0.025),
         hi = apply(boot[, key, drop = FALSE], 2, quantile, 0.975),
         var = str_extract(group, "^[^:]+"), level = str_remove(group, "^[^:]+:"))
spline_pages <- map(names(meta$item_labels), \(it) {
  ggplot(filter(spline, item == it), aes(pct, value, colour = level, fill = level, group = level)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA) +
    geom_line() +
    facet_grid(drug_f(drug) ~ var) +
    scale_x_continuous(breaks = c(10, 25, 50, 75, 90)) +
    labs(title = sprintf("Within-group dispersion D across involvement: %s", meta$item_labels[[it]]),
         subtitle = "Spline model; involvement percentiles 10–90; 95% bootstrap bands",
         x = "Involvement (weighted percentile)", y = "Leik's D", colour = NULL, fill = NULL) +
    base_theme +
    theme(legend.position = "top", strip.text.y = element_text(angle = 0, hjust = 0))
})

# 4. profile histograms at low vs high involvement
prof <- all_keys |>
  filter(family == "profile", str_starts(stat, "p_")) |>
  mutate(level = if_else(str_starts(stat, "p_lo"), "Low (P25)", "High (P75)") |> fct_inorder(),
         category = str_remove(stat, "^p_(lo|hi)_"),
         lo = apply(boot[, key, drop = FALSE], 2, quantile, 0.025),
         hi = apply(boot[, key, drop = FALSE], 2, quantile, 0.975))
prof_pages <- map(names(meta$item_labels), \(it) {
  dd <- filter(prof, item == it) |>
    mutate(category = fct_inorder(category), drug = drug_f(drug))
  ggplot(dd, aes(category, value, fill = level)) +
    geom_col(position = position_dodge(0.8), width = 0.75) +
    geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(0.8), width = 0.25, linewidth = 0.25) +
    facet_grid(drug ~ group, scales = "free_x", switch = "y") +
    scale_fill_manual(values = c("Low (P25)" = "grey65", "High (P75)" = "#B2182B"), name = "Involvement") +
    scale_y_continuous(labels = scales::label_percent(), breaks = c(0, 0.4, 0.8)) +
    labs(title = sprintf("Predicted answers for two profiles, low vs high involvement: %s", meta$item_labels[[it]]),
         subtitle = "Merged answer categories (1 = ban … 9 = free sale); 95% bootstrap CI",
         x = "Preferred policy", y = NULL) +
    base_theme +
    theme(legend.position = "top", strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0, hjust = 1))
})

# 5. raw cells: change in D from bottom to top half, 16 cells
raw <- r$raw_cells |>
  select(drug, item, cell, half, n, D) |>
  pivot_wider(names_from = half, values_from = c(n, D)) |>
  mutate(dD = if_else(n_bottom >= 30 & n_top >= 30, D_top - D_bottom, NA_real_))
raw_pages <- map(names(meta$item_labels), \(it) {
  ggplot(filter(raw, item == it), aes(drug_f(drug), cell, fill = dD)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = sprintf("%d/%d", n_bottom, n_top)), size = 1.8, colour = "grey30") +
    scale_fill_gradient2(low = "#2166AC", high = "#B2182B", na.value = "grey92", name = "dD (raw)") +
    labs(title = sprintf("Raw change in D, bottom vs top half of involvement: %s", meta$item_labels[[it]]),
         subtitle = "Cells of age × education × urbanisation × vote; grey = fewer than 30 respondents in a half (n bottom/top shown)",
         x = NULL, y = NULL) +
    base_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

pdf_pages("fig_primary.pdf", list(p_primary), width = 8, height = 4.5)
pdf_pages("fig_within_group_heatmap.pdf", list(p_heat), width = 11, height = 8)
pdf_pages("fig_spline_curves.pdf", spline_pages, width = 9, height = 11)
pdf_pages("fig_profiles_involvement.pdf", prof_pages, width = 8, height = 11)
pdf_pages("fig_cells_raw.pdf", raw_pages, width = 9, height = 7)

message("summary written to ", out_dir)
