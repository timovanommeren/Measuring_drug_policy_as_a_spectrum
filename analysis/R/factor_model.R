library(tidyverse)
library(lavaan)

source(here::here("analysis", "R", "load_and_clean.R"))

# ordinal factor models of the policy preferences for the eight drugs (painkillers left out), on the
# nine answer options (1 = ban ... 9 = free sale; "geen mening" is NA, pairwise deletion). WLSMV with
# the survey weights. Does a licit factor (alcohol, tobacco) next to an illicit factor fit better than
# one factor, and how strongly do the two correlate? A correlation clearly below 1 means the split
# is there at the respondent level, not only in the party averages.
# Each step ends with the plot the next decision is based on, in the usual psychometric forms (scree
# plot with parallel analysis, fit indices, modification indices, path diagrams); all pages go to
# one pdf, in order.
# Output: workflow plots (pdf), fit statistics and factor correlations (csv), the fitted models (rds)

out_dir <- here::here("analysis", "output", "latent_models", "factor_model")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

drug_labels <- c(alcohol = "Alcohol", tabak_en_vapes = "Tobacco and vapes", cannabis = "Cannabis",
                 psychedelica = "Psychedelics", mdma = "MDMA", cocaine = "Cocaine / crack",
                 speed = "Speed", heroine = "Heroin")

option_labels <- c("Ban", "Research only", "Treatment only", "Prescription only", "Specialist shop, non-profit",
                   "Specialist shop, for-profit", "Sold everywhere, no advertising",
                   "Sold everywhere, with advertising", "Free sale")

items <- paste0("beleid_voorkeur_", names(drug_labels))
item_labels <- setNames(drug_labels, items)
licit_items <- items[1:2]
illicit_items <- items[3:8]

model_labels <- c(one = "One factor", two_free = "Two factors, free loadings", two = "Two factors",
                  two_mi = "Two factors + cannabis on licit", three = "Three factors (licit, soft, hard)")
factor_labels <- c(drugs = "Drugs", licit = "Licit", illicit = "Illicit", soft = "Soft", hard = "Hard")

d <- df |>
  # ordered() keeps only the observed answers, so the empty "with advertising" category of MDMA and
  # the hard drugs drops out by itself
  mutate(across(all_of(items), \(x) ordered(as.numeric(x))))

fit_cfa <- \(model) cfa(model, d, ordered = items, estimator = "WLSMV", missing = "pairwise",
                        sampling.weights = "w", std.lv = TRUE)

# pages of the workflow pdf: ggplots, or functions that draw a base-graphics page
plots <- list()
step_theme <- theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"), plot.subtitle = element_text(colour = "grey30"),
        plot.title.position = "plot", panel.grid.minor = element_blank())

fit_table <- function(fits) {
  map_dfr(fits, \(fit) {
    as_tibble_row(fitMeasures(fit, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "tli.scaled",
                                     "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr")))
  }, .id = "model") |>
    mutate(across(-model, as.numeric))
}

# fit indices per model, with the usual cut-offs (Hu & Bentler 1999) and the 90% CI of the RMSEA
fit_plot <- function(stats) {
  cutoffs <- tibble(index = factor(c("CFI", "TLI", "RMSEA", "RMSEA", "SRMR"), levels = c("CFI", "TLI", "RMSEA", "SRMR")),
                    cut = c(0.95, 0.95, 0.06, 0.08, 0.08))
  stats |>
    transmute(model = factor(model_labels[model], levels = model_labels), CFI = cfi.scaled, TLI = tli.scaled,
              RMSEA = rmsea.scaled, SRMR = srmr, lo = rmsea.ci.lower.scaled, hi = rmsea.ci.upper.scaled) |>
    pivot_longer(c(CFI, TLI, RMSEA, SRMR), names_to = "index") |>
    mutate(index = factor(index, levels = levels(cutoffs$index)),
           across(c(lo, hi), \(x) if_else(index == "RMSEA", x, NA))) |>
    ggplot(aes(model, value, group = index)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#3b528b", alpha = 0.15, na.rm = TRUE) +
    geom_line(colour = "#3b528b", linewidth = 0.8) +
    geom_hline(data = cutoffs, aes(yintercept = cut), linetype = "dashed", colour = "#D55E00", linewidth = 0.4) +
    facet_wrap(~index, nrow = 1, scales = "free_y") +
    scale_x_discrete(labels = \(x) str_wrap(x, 12)) +
    labs(x = NULL, y = NULL, caption = "Dashed: conventional cut-offs (Hu & Bentler 1999). Band: 90% CI of the RMSEA.") +
    step_theme
}

# path diagram with standardised estimates: factors with their correlations on the left, items on
# the right; thresholds and residual variances left out
path_page <- function(fit, title) {
  force(fit)
  \() {
    lv <- lavNames(fit, "lv")
    semPlot::semPaths(fit, what = "std", whatLabels = "std", layout = "tree2", rotation = 2,
                      style = "lisrel", residuals = FALSE, thresholds = FALSE, intercepts = FALSE,
                      nodeLabels = c(item_labels[lavNames(fit, "ov")], factor_labels[lv]),
                      sizeMan = 14, sizeMan2 = 4, sizeLat = 9, label.scale = FALSE, label.cex = 0.8,
                      edge.label.cex = 0.8, edge.label.position = 0.65, edge.color = "grey20", fade = FALSE,
                      curve = 2, nCharNodes = 0, mar = c(4, 6, 8, 8))
    title(title, adj = 0, line = 1.5, cex.main = 1)
  }
}

# ---- step 1: answer distributions ----
plots$distributions <- df |>
  dplyr::select(all_of(items)) |>
  pivot_longer(everything(), names_to = "item", values_to = "x") |>
  mutate(drug = factor(item_labels[item], levels = rev(drug_labels)),
         answer = factor(if_else(is.na(x), "No opinion", option_labels[as.numeric(x)]),
                         levels = c(option_labels, "No opinion"))) |>
  count(drug, answer) |>
  mutate(p = n / sum(n), .by = drug) |>
  ggplot(aes(p, drug, fill = answer)) +
  geom_col(position = position_stack(reverse = TRUE), width = 0.8) +
  scale_fill_manual(values = c(viridisLite::viridis(9, end = 0.9), "grey80"), drop = FALSE) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Step 1. Answer distributions (unweighted)",
       subtitle = paste("Nine ordered, skewed options with sparse categories (and empty ones for the hard drugs):",
                        "treat the items as ordinal, with polychoric correlations and WLSMV")) +
  step_theme +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))

# ---- step 2: number of factors ----
fit1 <- fit_cfa(paste("drugs =~", paste(items, collapse = " + ")))

# scree plot of the (weighted) polychoric correlations, with Horn's parallel analysis: the 95th
# percentile of the eigenvalues of 1000 random normal data sets of the same size
poly <- unclass(lavInspect(fit1, "sampstat")$cov)
set.seed(20261029)
random_eigen <- replicate(1000, eigen(cor(matrix(rnorm(nobs(fit1) * length(items)), ncol = length(items))))$values)

scree <- tibble(component = seq_along(items), Observed = eigen(poly)$values,
                `Parallel analysis (95th percentile)` = apply(random_eigen, 1, quantile, 0.95))
print(scree)

plots$scree <- scree |>
  pivot_longer(-component, names_to = "series", values_to = "eigenvalue") |>
  ggplot(aes(component, eigenvalue, linetype = series)) +
  geom_line(linewidth = 0.8, colour = "#3b528b") +
  scale_x_continuous(breaks = seq_along(items)) +
  scale_linetype_manual(values = c(Observed = "solid", `Parallel analysis (95th percentile)` = "dashed")) +
  labs(x = "Component", y = "Eigenvalue", linetype = NULL, title = "Step 2. Scree plot with parallel analysis",
       subtitle = paste("Eigenvalues of the weighted polychoric correlations. Factors to retain: observed eigenvalues",
                        "above the parallel-analysis line.\nOne dominant factor and a clear second one:",
                        "test a licit and an illicit factor against one factor")) +
  step_theme +
  theme(legend.position = "bottom")

# ---- step 3: one factor or two ----
illicit <- paste("illicit =~", paste(illicit_items, collapse = " + "))
fit2_free <- fit_cfa(paste0("licit =~ ", paste(licit_items, collapse = " + "), "\n", illicit))
# with only two indicators the free licit factor gives tobacco a (slightly) negative residual
# variance, so the main model has equal licit loadings
m2 <- paste0("licit =~ a*", paste(licit_items, collapse = " + a*"), "\n", illicit)
fit2 <- fit_cfa(m2)

# the one-factor model is the free two-factor model with r = 1 (the equal-loadings model has the same
# df as one factor, so is not nested in it)
print(lavTestLRT(fit1, fit2_free))

# negative residual variances (Heywood cases)
heywood <- map(list(one = fit1, two_free = fit2_free, two = fit2),
               \(fit) filter(parameterEstimates(fit), op == "~~", lhs == rhs, lhs %in% items, est < 0))
print(heywood)

plots$fit_one_two <- fit_table(list(one = fit1, two_free = fit2_free, two = fit2)) |>
  fit_plot() +
  labs(title = "Step 3. One factor or two",
       subtitle = paste("Two factors fit clearly better (scaled chi-square difference test). The free licit factor",
                        "gives tobacco a negative residual variance,\nso its two loadings are set equal;",
                        "the fit stays poor (RMSEA), so look at where the misfit is"))

# ---- step 4: where is the misfit ----
mi_table <- function(fit, n = 10) {
  modindices(fit) |>
    as_tibble() |>
    filter(op %in% c("=~", "~~"), lhs != rhs) |>
    slice_max(mi, n = n) |>
    mutate(label = if_else(op == "=~",
                           paste(item_labels[rhs], "on", str_to_lower(factor_labels[lhs]), "factor"),
                           paste(item_labels[lhs], "with", item_labels[rhs], "(residual)")),
           type = if_else(op == "=~", "Cross-loading", "Residual correlation"))
}
mi <- mi_table(fit2)
print(mi)

plots$modindices <- ggplot(mi, aes(mi, fct_reorder(label, mi), fill = type)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("Cross-loading" = "#D55E00", "Residual correlation" = "grey60")) +
  labs(x = "Modification index", y = NULL, fill = NULL, title = "Step 4. Modification indices, two-factor model",
       subtitle = paste("Largest misfit: cannabis also loads on the licit factor. Plausible, as cannabis is",
                        "tolerated and sold in coffeeshops: free that loading.\nThe residual correlations",
                        "among cocaine, speed and heroin point to a soft/hard split within the illicit drugs")) +
  step_theme +
  theme(legend.position = "bottom")

# ---- step 5: cannabis cross-loading and the soft/hard split ----
fit2_mi <- fit_cfa(paste0(m2, "\nlicit =~ ", items[3]))
fit3 <- fit_cfa(paste0("licit =~ a*", paste(licit_items, collapse = " + a*"), "\n",
                       "soft =~ ", paste(illicit_items[1:3], collapse = " + "), "\n",
                       "hard =~ ", paste(illicit_items[4:6], collapse = " + ")))

fits <- list(one = fit1, two_free = fit2_free, two = fit2, two_mi = fit2_mi, three = fit3)
write_rds(fits, file.path(out_dir, "factor_fits.rds"))

# freeing cannabis on licit (1 df); the two-factor model is the three-factor model with r(soft, hard) = 1
print(lavTestLRT(fit2, fit2_mi))
print(lavTestLRT(fit2, fit3))

fit_stats <- fit_table(fits)
print(fit_stats, width = Inf)
write_csv(fit_stats, file.path(out_dir, "factor_fit_stats.csv"))

plots$fit_all <- fit_plot(fit_stats) +
  labs(title = "Step 5a. Fit of all models",
       subtitle = paste("Both the cannabis cross-loading and the soft/hard split improve the fit;",
                        "the licit-illicit correlation is what matters, so compare it across models"))

# the largest remaining modification indices per model: what the changes fixed and what is left
plots$modindices_after <- map_dfr(list(two = fit2, two_mi = fit2_mi, three = fit3), \(fit) mi_table(fit, 6),
                                  .id = "model") |>
  mutate(model = factor(model_labels[model], levels = model_labels),
         # unique per panel, so each panel is sorted on its own
         label = fct_reorder(paste(label, model, sep = "___"), mi)) |>
  ggplot(aes(mi, label, fill = type)) +
  geom_col(width = 0.7) +
  scale_y_discrete(labels = \(x) str_remove(x, "___.*")) +
  facet_wrap(~model, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c("Cross-loading" = "#D55E00", "Residual correlation" = "grey60")) +
  labs(x = "Modification index", y = NULL, fill = NULL, title = "Step 5b. Largest remaining modification indices",
       subtitle = paste("In the two-factor model the cross-loading takes out the cannabis misfit. The soft/hard",
                        "split takes out the hard-drug residuals,\nbut there cannabis still wants to load on licit:",
                        "cannabis sits between the licit and the illicit drugs")) +
  step_theme +
  theme(legend.position = "bottom")

# ---- step 6: results ----
std <- map_dfr(fits[-1], \(fit) as_tibble(standardizedSolution(fit)), .id = "model")
print(filter(std, model %in% c("two", "two_mi", "three"), op == "=~") |>
        dplyr::select(model, lhs, rhs, est.std, ci.lower, ci.upper), n = Inf)

r <- filter(std, op == "~~", lhs != rhs, lhs %in% names(factor_labels)) |>
  dplyr::select(model, lhs, rhs, est.std, se, ci.lower, ci.upper)
print(r)
write_csv(r, file.path(out_dir, "factor_correlations.csv"))

plots$path_two <- path_page(fit2, "Step 6a. Two-factor model, standardised estimates")
plots$path_two_mi <- path_page(fit2_mi, "Step 6b. Two-factor model with cannabis on licit, standardised estimates")
plots$path_three <- path_page(fit3, "Step 6c. Three-factor model, standardised estimates")

cairo_pdf(file.path(out_dir, "factor_model_workflow.pdf"), width = 11, height = 7, onefile = TRUE)
walk(plots, \(p) if (is.function(p)) p() else print(p))
dev.off()
