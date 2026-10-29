library(tidyverse)
library(tidyLPA)

source(here::here("analysis", "R", "load_and_clean.R"))

# latent profile analysis following the design of Schäfer (2025, Political Studies Review,
# doi:10.1177/14789299241311760), with as indicators the dimensions the party map contrasts: mean
# policy preference for the licit drugs (alcohol, tobacco), the soft drugs (cannabis, psychedelics,
# MDMA) and the hard drugs (cocaine, speed, heroin), each on a 0 (ban) to 1 (free sale) scale.
# A "double standard" profile (high licit, low illicit) or a "cannabis yes, cocaine no" profile can
# show up as a profile of its own. Step 1 is unweighted (mclust has no weights); step 3 relates the
# profiles to the vote with BCH weights (bias-adjusted for misclassification) times the survey weights
# Output: fit statistics (csv), profile plots of all solutions (pdf), profile shares by bloc with
# bootstrap CIs (csv), figure of the chosen solution (pdf)

out_dir <- here::here("analysis", "output", "latent_models", "lpa")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 0–1 scale, as in Schäfer; geen mening is already NA
d <- df |>
  mutate(across(starts_with("beleid_voorkeur_"), \(x) (as.numeric(x) - 1) / 8),
         licit = rowMeans(pick(beleid_voorkeur_alcohol, beleid_voorkeur_tabak_en_vapes), na.rm = TRUE),
         soft = rowMeans(pick(beleid_voorkeur_cannabis, beleid_voorkeur_psychedelica, beleid_voorkeur_mdma),
                         na.rm = TRUE),
         hard = rowMeans(pick(beleid_voorkeur_cocaine, beleid_voorkeur_speed, beleid_voorkeur_heroine), na.rm = TRUE)) |>
  filter(if_all(c(licit, soft, hard), is.finite))

# models: 1 = equal variances, 2 = varying variances, 6 = varying variances + covariances.
# Half the respondents ban all three hard drugs (hard = 0), and without a prior mclust cannot fit
# models 2 and 6 (a profile with zero variance on hard); the default conjugate prior regularises that
set.seed(20261029)
fits <- estimate_profiles(dplyr::select(d, licit, soft, hard), 1:6, models = c(1, 2, 6),
                          prior = mclust::priorControl())
write_rds(fits, file.path(out_dir, "lpa_fits.rds"))

fit_stats <- get_fit(fits)
print(dplyr::select(fit_stats, Model, Classes, LogLik, AIC, BIC, SABIC, Entropy, prob_min, n_min, BLRT_p), n = Inf)
write_csv(fit_stats, file.path(out_dir, "lpa_fit_stats.csv"))

pdf(file.path(out_dir, "lpa_profiles.pdf"), width = 11, height = 8)
print(plot_profiles(fits, add_line = TRUE, rawdata = FALSE))
dev.off()

# step 3 on the chosen solution (provisional). In models 2 and 6 the first profile is the floor at
# hard = 0 (sd ~.005), so they mostly separate "bans all hard drugs" from the rest; model 1 gives
# interpretable types
fit <- fits[["model_1_class_4"]]
post <- get_data(fit) |> dplyr::select(starts_with("CPROB")) |> as.matrix()

profile_means <- get_estimates(fit) |>
  filter(Category == "Means") |>
  dplyr::select(Class, Parameter, Estimate) |>
  pivot_wider(names_from = Parameter, values_from = Estimate)
print(profile_means)

colnames(post) <- paste("Profile", seq_len(ncol(post)))
profile_labels <- with(profile_means, sprintf("Profile %d: licit %.2f, soft %.2f, hard %.2f", Class, licit, soft, hard))
vote <- fct_na_value_to_level(d$vote_3, "none")

# BCH correction (Vermunt 2010; Bakk, Tekle & Vermunt 2013): assign each respondent to their modal
# profile, then weight with the inverse of the classification-error matrix, which undoes the
# misclassification that attenuates differences between groups. D: rows = true profile, columns =
# assigned profile. Each respondent's BCH weights sum to 1 but can be negative, so the shares are
# computed directly instead of with a multinomial logit
modal <- max.col(post)
D <- t(rowsum(post, modal))
D <- D / rowSums(D)
print(round(D, 3))
bch <- solve(D)[modal, ]
colnames(bch) <- colnames(post)

# weighted profile shares within each bloc, with posterior-probability weights (for comparison) and
# with BCH weights
shares <- \(i, wts) {
  s <- rowsum(wts[i, ] * d$w[i], vote[i])
  as_tibble(s / rowSums(s), rownames = "vote") |>
    pivot_longer(-vote, names_to = "profile", values_to = "share")
}
methods <- list(posterior = post, bch = bch)

# bootstrap CIs: resample respondents, keep step 1 (posteriors and D) fixed
set.seed(20261029)
boot <- map_dfr(1:1000, \(b) {
  i <- sample(nrow(post), replace = TRUE)
  map_dfr(methods, \(wts) shares(i, wts), .id = "method")
})

share_tab <- map_dfr(methods, \(wts) shares(seq_len(nrow(post)), wts), .id = "method") |>
  left_join(summarise(boot, ci_lower = quantile(share, 0.025), ci_upper = quantile(share, 0.975),
                      .by = c(method, vote, profile)), by = c("method", "vote", "profile"))
print(pivot_wider(share_tab, id_cols = c(vote, profile), names_from = method,
                  values_from = c(share, ci_lower, ci_upper)), n = Inf)
write_csv(share_tab, file.path(out_dir, "lpa_shares_by_vote.csv"))

# figure: profile means, and the profile composition of each bloc (BCH)
fig_theme <- theme_minimal(base_size = 9) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(), plot.title.position = "plot",
        plot.title = element_text(face = "bold"))
profile_colours <- setNames(c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#0072B2", "#D55E00")[seq_along(profile_labels)],
                            profile_labels)
overall <- colSums(post * d$w) / sum(d$w)

# profile plot (Spurk et al. 2020): mean of each indicator per profile, one line per profile
p_means <- profile_means |>
  pivot_longer(c(licit, soft, hard), names_to = "indicator", values_to = "mean") |>
  mutate(indicator = factor(indicator, levels = c("licit", "soft", "hard"),
                            labels = c("Licit\n(alcohol, tobacco)", "Soft\n(cannabis, psychedelics, MDMA)",
                                       "Hard\n(cocaine, speed, heroin)")),
         profile = sprintf("%s (%.0f%%)", profile_labels[Class], 100 * overall[Class])) |>
  ggplot(aes(indicator, mean, colour = profile, group = profile)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = setNames(profile_colours, NULL)) +
  scale_x_discrete(expand = expansion(add = 0.4)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("0\nban", "0.25", "0.5", "0.75", "1\nfree sale")) +
  labs(x = NULL, y = "Mean policy preference", colour = NULL, title = "Latent profiles") +
  fig_theme +
  guides(colour = guide_legend(ncol = 1))

# composition of each bloc, BCH-corrected (CIs in lpa_shares_by_vote.csv)
p_shares <- share_tab |>
  filter(method == "bch") |>
  mutate(vote = factor(vote, levels = c("left", "middle", "right", "none"),
                       labels = c("Left", "Centre", "Right", "Did not vote / other")),
         profile = factor(profile_labels[as.integer(str_extract(profile, "\\d+"))], levels = profile_labels)) |>
  ggplot(aes(share, fct_rev(vote), fill = profile)) +
  geom_col(position = position_stack(reverse = TRUE), width = 0.7) +
  geom_text(aes(label = if_else(share >= 0.05, scales::percent(share, 1), "")),
            position = position_stack(reverse = TRUE, vjust = 0.5), size = 2.8) +
  scale_fill_manual(values = profile_colours) +
  scale_x_continuous(labels = scales::percent, expand = expansion()) +
  labs(x = "Share of the bloc (BCH-corrected for classification error)", y = NULL, fill = NULL,
       title = "Profile composition by bloc") +
  fig_theme +
  theme(plot.margin = margin(5.5, 15, 5.5, 5.5)) +
  guides(fill = guide_legend(ncol = 1))

cairo_pdf(file.path(out_dir, "fig_lpa.pdf"), width = 6.5, height = 4.5, onefile = TRUE)
print(p_means)
print(p_shares)
dev.off()
