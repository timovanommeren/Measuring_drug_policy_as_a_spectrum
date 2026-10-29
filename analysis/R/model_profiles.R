library(tidyverse)

source(here::here("analysis", "R", "load_and_clean.R"))

# one multinomial model per drug: grouped regime ~ all demographics; then what the model
# predicts for two contrasting profiles, with 95% CIs from a respondent bootstrap

set.seed(2026)
n_boot <- 500

drug_labels <- c(alcohol = "Alcohol", tabak_en_vapes = "Tobacco and vapes", cannabis = "Cannabis",
                 psychedelica = "Psychedelics", mdma = "MDMA", cocaine = "Cocaine / crack",
                 speed = "Speed", heroine = "Heroin")

# predictors; unknown urbanisation and other/no vote kept as own levels so nobody drops out
d <- df |>
  mutate(
    age = fct_collapse(age_4_rec, "18–49" = c("18-34", "35-49"), "50+" = c("50-64", "65+")),
    edu = fct_recode(haven::as_factor(edu_2_rec), practical = "Praktisch-opgeleid",
                     theoretical = "Theoretisch-opgeleid"),
    urb3 = factor(case_when(as.integer(urb) <= 2 ~ "urban", as.integer(urb) >= 3 ~ "rural",
                            .default = "unknown"),
                  levels = c("urban", "rural", "unknown")),
    vote = fct_na_value_to_level(vote_lr, "other/none"),
    sex = sex_rec,
    # "Hoe een partij omgaat met drugs en drugscriminaliteit, weegt mee bij mijn stemkeuze" (0-100)
    inv = as.numeric(betrokkenheid_3) / 100
  )

weighted_quantile <- function(x, w, p) {
  keep <- !is.na(x)
  o <- order(x[keep])
  x[keep][o][which(cumsum(w[keep][o]) >= p * sum(w[keep]))[1]]
}

# low and high involvement for the moderation model: weighted 25th and 75th percentile
inv_lo <- weighted_quantile(d$inv, d$w, 0.25)
inv_hi <- weighted_quantile(d$inv, d$w, 0.75)

predictors <- c("age", "edu", "urb3", "vote")

profiles <- list(
  "Young, theoretical, urban, left" = c(age = "18–49", edu = "theoretical", urb3 = "urban", vote = "left"),
  "Old, practical, rural, right"    = c(age = "50+", edu = "practical", urb3 = "rural", vote = "right")
)

# adjusted effect of one demographic: second level minus first, others as observed
contrasts <- list(
  "Age: 50+ vs 18–49"                  = c(var = "age",  first = "18–49",       second = "50+"),
  "Education: practical vs theoretical" = c(var = "edu",  first = "theoretical", second = "practical"),
  "Urbanisation: rural vs urban"        = c(var = "urb3", first = "urban",       second = "rural"),
  "Vote: right vs left"                 = c(var = "vote", first = "left",        second = "right")
)

# weighted mean predicted probability per regime, with the given variables set to fixed levels
predict_set <- function(fit, data, set = NULL) {
  for (v in names(set)) {
    data[[v]] <- if (is.factor(data[[v]])) factor(set[[v]], levels = levels(data[[v]])) else set[[v]]
  }
  pr <- predict(fit, data, type = "probs")
  colSums(pr * data$w) / sum(data$w)
}

fit_model <- function(dd) {
  nnet::multinom(y ~ age + edu + urb3 + vote + sex, data = dd, weights = w, trace = FALSE, maxit = 500)
}

# moderation: does the effect of vote depend on involvement?
fit_moderation <- function(dd) {
  nnet::multinom(y ~ age + edu + urb3 + sex + vote * inv, data = dd, weights = w, trace = FALSE,
                 maxit = 500)
}

# predictions for all categories, 0 for a category absent from a (resampled) dataset
all_categories <- function(p, categories) {
  out <- setNames(rep(0, length(categories)), categories)
  out[names(p)] <- p
  out
}

# all quantities for one (resampled) dataset, per drug:
# - 9 answer options: observed and the two profiles (for the histograms)
# - 4 grouped regimes: observed and the adjusted effect of each demographic
estimate <- function(data) {
  map(names(drug_labels), \(drug) {
    options <- data |>
      mutate(y = droplevels(factor(as.numeric(.data[[paste0("beleid_voorkeur_", drug)]]), levels = 1:9))) |>
      filter(!is.na(y))
    regimes <- data |> mutate(y = .data[[paste0("regime_", drug)]]) |> filter(!is.na(y))
    fit_o <- fit_model(options)
    fit_r <- fit_model(regimes)
    regimes_m <- filter(regimes, !is.na(inv))
    fit_m <- fit_moderation(regimes_m)
    opt <- \(set = NULL) all_categories(predict_set(fit_o, options, set), as.character(1:9))
    reg <- \(set = NULL) all_categories(predict_set(fit_r, regimes, set), regime_labels)
    mod <- \(vote, inv) all_categories(predict_set(fit_m, regimes_m, list(vote = vote, inv = inv)),
                                       regime_labels)
    left_lo <- mod("left", inv_lo)
    left_hi <- mod("left", inv_hi)
    right_lo <- mod("right", inv_lo)
    right_hi <- mod("right", inv_hi)
    moderation <- list("left, low" = left_lo, "left, high" = left_hi,
                       "right, low" = right_lo, "right, high" = right_hi,
                       "gap low" = right_lo - left_lo, "gap high" = right_hi - left_hi,
                       "gap change" = (right_hi - left_hi) - (right_lo - left_lo))
    bind_rows(
      tibble(type = "observed", outcome = "option", group = "Observed", category = as.character(1:9),
             value = opt()),
      imap(profiles, \(p, nm) tibble(type = "profile", outcome = "option", group = nm,
                                     category = as.character(1:9), value = opt(as.list(p)))) |>
        list_rbind(),
      tibble(type = "observed", outcome = "regime", group = "Observed", category = regime_labels,
             value = reg()),
      imap(contrasts, \(cn, nm) {
        tibble(type = "ame", outcome = "regime", group = nm, category = regime_labels,
               value = reg(setNames(list(cn[["second"]]), cn[["var"]])) -
                 reg(setNames(list(cn[["first"]]), cn[["var"]])))
      }) |>
        list_rbind(),
      imap(moderation, \(v, nm) tibble(type = "moderation", outcome = "regime", group = nm,
                                       category = regime_labels, value = v)) |>
        list_rbind()
    ) |>
      mutate(drug = drug)
  }) |>
    list_rbind()
}

point <- estimate(d)

boot <- map(seq_len(n_boot), \(b) estimate(d[sample(nrow(d), replace = TRUE), ]) |> mutate(b = b),
            .progress = "bootstrap") |>
  list_rbind()

keys <- c("type", "outcome", "group", "category", "drug")
results <- boot |>
  summarise(lo = quantile(value, 0.025), hi = quantile(value, 0.975), .by = all_of(keys)) |>
  right_join(point, by = keys) |>
  mutate(drug_label = drug_labels[drug])

# checks: probabilities sum to 1; observed predictions match the weighted sample shares
stopifnot(all(abs(summarise(filter(results, type %in% c("observed", "profile") |
                                     (type == "moderation" & !str_starts(group, "gap"))),
                            s = sum(value), .by = c(type, outcome, group, drug))$s - 1) < 1e-6))
sample_shares <- map(names(drug_labels), \(drug) {
  x <- as.numeric(d[[paste0("beleid_voorkeur_", drug)]])
  r <- d[[paste0("regime_", drug)]]
  bind_rows(
    tibble(outcome = "option", category = as.character(1:9),
           share = map_dbl(1:9, \(k) sum(d$w[which(x == k)]) / sum(d$w[!is.na(x)]))),
    tibble(outcome = "regime", category = regime_labels,
           share = map_dbl(regime_labels, \(k) sum(d$w[which(r == k)]) / sum(d$w[!is.na(r)])))
  ) |>
    mutate(drug = drug)
}) |>
  list_rbind()
check <- results |> filter(type == "observed") |> left_join(sample_shares, by = c("outcome", "category", "drug"))
stopifnot(all(abs(check$value - check$share) < 0.01))

write_csv(results, here::here("analysis", "output", "model_profiles.csv"))

# respondents who match each profile exactly
profile_n <- map_int(profiles, \(p) sum(reduce(imap(p, \(lv, v) d[[v]] == lv), `&`)))

# drugs in the order of the main figure (by weighted % ban), first on top
drug_order <- sample_shares |> filter(outcome == "regime", category == "ban") |> arrange(share) |> pull(drug)


## Figure: predicted distribution over the 9 answer options for the two profiles

prof <- results |>
  filter(type == "profile") |>
  mutate(drug_label = factor(drug_label, levels = drug_labels[drug_order]),
         group = factor(group, levels = names(profiles)),
         x = as.integer(category))

p_profiles <- ggplot(prof, aes(x, value, fill = factor(x))) +
  geom_col(width = 0.8) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.25, linewidth = 0.25) +
  facet_grid(drug_label ~ group, switch = "y") +
  scale_fill_viridis_d(end = 0.9, guide = "none") +
  scale_x_continuous(breaks = 1:9, expand = expansion(add = 0.3)) +
  scale_y_continuous(labels = scales::label_percent(), breaks = c(0, 0.4, 0.8), expand = expansion(0)) +
  labs(x = "Preferred policy (1 = ban … 9 = free sale)", y = NULL) +
  theme_minimal(base_size = 8) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(face = "bold"),
        strip.text.y.left = element_text(angle = 0, hjust = 1),
        panel.spacing.y = unit(0.5, "lines"))

ggsave(here::here("analysis", "output", "fig_profiles.pdf"), p_profiles, width = 6.5, height = 8, device = cairo_pdf)

caption_profiles <- paste0(
  "Predicted preferred policy for two contrasting profiles, over the nine answer options ",
  r"((colours as in Figure~\ref{fig:policy_scale}). Predictions from one multinomial logit model )",
  "per drug, with age (18--49, 50+), education (practical incl.\\ middle, theoretical), urbanisation ",
  "(urban = high or very high, rural = moderate, low or none, unknown), vote (left, right, other or none) ",
  "and sex as predictors, ",
  "weighted. For each profile, all respondents were assigned the profile's age, education, urbanisation ",
  "and vote while keeping their own sex; bars show the weighted mean predicted shares. ",
  "Error bars: 95\\% bootstrap confidence intervals ",
  sprintf("(%d respondent resamples). ", n_boot),
  sprintf("Respondents matching each profile exactly: %d (young, theoretical, urban, left) and %d ",
          profile_n[[1]], profile_n[[2]]),
  "(old, practical, rural, right). Vote: left = GroenLinks-PvdA, D66, SP, PvdD, Volt, DENK, CU; ",
  "right = PVV, VVD, CDA, JA21, FvD, BBB, 50PLUS, SGP."
)

writeLines(c(
  r"(\begin{figure}[H])",
  r"(  \centering)",
  r"(  \includegraphics[width=\linewidth]{out/fig_profiles.pdf})",
  paste0(r"(  \caption{)", caption_profiles, "}"),
  r"(  \label{fig:profiles})",
  r"(\end{figure})"
), here::here("analysis", "output", "fig_profiles.tex"))


## Exploratory: adjusted effect of each demographic (others held as observed)

ame <- results |>
  filter(type == "ame") |>
  mutate(drug_label = factor(drug_label, levels = rev(drug_labels[drug_order])),
         group = factor(group, levels = names(contrasts)),
         regime = factor(category, levels = regime_labels))

p_ame <- ggplot(ame, aes(100 * value, drug_label, colour = group)) +
  geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_pointrange(aes(xmin = 100 * lo, xmax = 100 * hi), position = position_dodge(width = 0.7),
                  size = 0.2) +
  facet_grid(~regime, labeller = labeller(regime = str_to_sentence)) +
  scale_colour_manual(values = c("#E69F00", "#56B4E9", "#009E73", "#D55E00"), name = NULL) +
  labs(title = "Adjusted effect of each demographic on the predicted regime share",
       subtitle = "Average marginal effect (percentage points), other demographics as observed; 95% bootstrap CI",
       x = "Difference in predicted share (percentage points)", y = NULL) +
  theme_minimal(base_size = 9) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold")) +
  guides(colour = guide_legend(nrow = 2))

ggsave(here::here("analysis", "output", "model_profiles_ame.pdf"), p_ame, width = 11, height = 6.5,
       device = cairo_pdf)


## Exploratory: involvement as moderator of the left-right gap

# does one side simply score higher on involvement?
d |>
  filter(!is.na(inv)) |>
  summarise(mean_involvement = 100 * weighted.mean(inv, w), n = n(), .by = vote) |>
  print()

inv_levels <- c(low = sprintf("Low (P25 = %.0f)", 100 * inv_lo), high = sprintf("High (P75 = %.0f)", 100 * inv_hi))

mod_shares <- results |>
  filter(type == "moderation", !str_starts(group, "gap")) |>
  separate_wider_delim(group, ", ", names = c("vote", "involvement")) |>
  mutate(drug_label = factor(drug_label, levels = drug_labels[drug_order]),
         regime = factor(category, levels = regime_labels),
         vote = str_to_sentence(vote),
         involvement = factor(inv_levels[involvement], levels = inv_levels))

p_mod_shares <- ggplot(mod_shares, aes(involvement, 100 * value, colour = vote, group = vote)) +
  geom_line(linewidth = 0.4) +
  geom_pointrange(aes(ymin = 100 * lo, ymax = 100 * hi), size = 0.15) +
  facet_grid(drug_label ~ regime, labeller = labeller(regime = str_to_sentence)) +
  scale_colour_manual(values = c(Left = "#D55E00", Right = "#0072B2"), name = "Vote") +
  scale_y_continuous(breaks = c(0, 50, 100)) +
  labs(title = "Predicted regime share by vote and involvement",
       subtitle = paste("Involvement: “Hoe een partij omgaat met drugs en drugscriminaliteit, weegt mee bij mijn",
                        "stemkeuze” (0–100).\nMultinomial logit per drug with vote × involvement; age, education,",
                        "urbanisation and sex as observed. 95% bootstrap CI."),
       x = "Involvement", y = "Predicted share (%)") +
  theme_minimal(base_size = 8) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"),
        strip.text.y = element_text(angle = 0, hjust = 0))

gap_change <- results |>
  filter(type == "moderation", group == "gap change") |>
  mutate(drug_label = factor(drug_label, levels = rev(drug_labels[drug_order])),
         regime = factor(category, levels = regime_labels))

p_gap_change <- ggplot(gap_change, aes(100 * value, drug_label)) +
  geom_vline(xintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_pointrange(aes(xmin = 100 * lo, xmax = 100 * hi), size = 0.2) +
  facet_grid(~regime, labeller = labeller(regime = str_to_sentence)) +
  labs(title = "Does the left–right gap grow with involvement?",
       subtitle = paste0("Change in the gap (right − left, percentage points) from low (P25 = ", round(100 * inv_lo),
                         ") to high (P75 = ", round(100 * inv_hi), ") involvement; 95% bootstrap CI.\n",
                         "Positive: at high involvement right-wing voters are further ahead of left-wing voters ",
                         "on this regime."),
       x = "Change in right − left gap (percentage points)", y = NULL) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank(), strip.text = element_text(face = "bold"))

cairo_pdf(here::here("analysis", "output", "model_involvement.pdf"), width = 9, height = 7.5, onefile = TRUE)
print(p_mod_shares)
print(p_gap_change)
dev.off()
