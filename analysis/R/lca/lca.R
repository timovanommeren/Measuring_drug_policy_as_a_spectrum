library(tidyverse)
library(poLCA)

source(here::here("analysis", "R", "load_and_clean.R"))

# exploratory latent class analysis of the policy preferences for the eight drugs (painkillers
# left out), on the nine answer options (1 = ban ... 9 = free sale; "geen mening" is NA and kept in
# with na.rm = FALSE). 1 to 8 classes, 50 random starts each. Unweighted: poLCA has no survey weights.
# Output: fit statistics (csv), the fitted models (rds) and the class profiles per solution (pdf)

out_dir <- here::here("analysis", "output", "latent_models", "lca")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

drug_labels <- c(alcohol = "Alcohol", tabak_en_vapes = "Tobacco and vapes", cannabis = "Cannabis",
                 psychedelica = "Psychedelics", mdma = "MDMA", cocaine = "Cocaine / crack",
                 speed = "Speed", heroine = "Heroin")

option_labels <- c("Ban", "Research only", "Treatment only", "Prescription only", "Specialist shop, non-profit",
                   "Specialist shop, for-profit", "Sold everywhere, no advertising",
                   "Sold everywhere, with advertising", "Free sale")

items <- paste0("beleid_voorkeur_", names(drug_labels))

lca_df <- df |>
  dplyr::select(all_of(items)) |>
  mutate(across(everything(), as.integer)) |>
  as.data.frame()

f <- as.formula(paste0("cbind(", paste(items, collapse = ", "), ") ~ 1"))

n_classes <- 1:8

set.seed(20261029)
fits <- map(n_classes, \(k) {
  message("fitting ", k, " classes")
  poLCA(f, lca_df, nclass = k, nrep = 50, maxiter = 5000, na.rm = FALSE, verbose = FALSE, calc.se = FALSE)
})
names(fits) <- n_classes
write_rds(fits, file.path(out_dir, "lca_fits.rds"))

# relative entropy (1 = perfect classification); undefined for one class
rel_entropy <- function(fit) {
  k <- ncol(fit$posterior)
  if (k == 1) return(NA_real_)
  p <- pmax(fit$posterior, 1e-300)
  1 - sum(-p * log(p)) / (nrow(p) * log(k))
}

fit_stats <- map_dfr(fits, \(fit) {
  tibble(
    classes = length(fit$P),
    loglik = fit$llik,
    npar = fit$npar,
    aic = fit$aic,
    bic = fit$bic,
    # sample-size adjusted BIC
    sabic = -2 * fit$llik + fit$npar * log((fit$N + 2) / 24),
    entropy = rel_entropy(fit),
    smallest_class = min(fit$P),
    # how many of the 50 starts reached the best log-likelihood (a check on local maxima)
    best_ll_replicated = sum(abs(fit$attempts - max(fit$attempts)) < 0.01),
    converged = fit$numiter < fit$maxiter,
    resid_df = fit$resid.df
  )
})

write_csv(fit_stats, file.path(out_dir, "lca_fit_stats.csv"))
print(fit_stats, width = Inf)

# information criteria by number of classes
p_ic <- fit_stats |>
  pivot_longer(c(aic, bic, sabic), names_to = "criterion") |>
  mutate(criterion = toupper(criterion)) |>
  ggplot(aes(classes, value, colour = criterion)) +
  geom_line() +
  scale_x_continuous(breaks = n_classes) +
  labs(x = "Number of classes", y = NULL, colour = NULL) +
  theme_minimal()

# class profiles: expected answer (1 = ban ... 9 = free sale) per drug within each class, one line
# per class, one page per solution
profile_plot <- function(fit) {
  shares <- fit$P
  imap_dfr(fit$probs, \(m, item) {
    tibble(class_n = seq_len(nrow(m)), drug = drug_labels[str_remove(item, "beleid_voorkeur_")],
           mean = drop(m %*% as.integer(str_extract(colnames(m), "\\d+"))))
  }) |>
    mutate(
      class = factor(sprintf("Class %d (%.0f%%)", class_n, 100 * shares[class_n])) |> fct_reorder(class_n),
      drug = factor(drug, levels = drug_labels)
    ) |>
    ggplot(aes(drug, mean, colour = class, group = class)) +
    geom_line(linewidth = 1) +
    scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#0072B2", "#D55E00",
                                   "#999999", "#000000")) +
    scale_y_continuous(limits = c(1, 9), breaks = 1:9, labels = option_labels) +
    labs(x = NULL, y = "Mean response", colour = NULL, title = sprintf("%d classes", length(shares))) +
    theme_minimal() +
    theme(legend.position = "right", panel.grid.minor = element_blank())
}

pdf(file.path(out_dir, "lca_profiles.pdf"), width = 11, height = 8)
print(p_ic)
walk(fits[-1], \(fit) print(profile_plot(fit)))
dev.off()
