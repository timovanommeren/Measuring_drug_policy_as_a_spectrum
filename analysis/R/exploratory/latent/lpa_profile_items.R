library(tidyverse)
library(patchwork)

source(here::here("analysis", "R", "00_load_and_clean.R"))

# The four profiles of the chosen LPA solution (model 1, equal variances, 4 profiles; see lpa_profiles.R) across
# every item of the questionnaire: one line per profile, the profile's mean answer per item. Items in the order
# of the survey: the blocks came in fixed order (policy, beliefs, behaviour, own use, feelings, involvement);
# within the policy, belief and behaviour blocks the order was randomised per respondent, so there the items
# follow the questionnaire's own numbering. The blocks have different answer scales, so each gets its own panel
# and y axis. Answers as given (not reverse-scored); belief and behaviour items where agreeing favours
# prohibition are marked with an asterisk. A profile's mean weights each respondent by their posterior probability
# of the profile times the survey weight, so uncertain respondents count partly in several profiles.
# Needs analysis/output/latent_models/lpa/lpa_fits.rds (run lpa_profiles.R first).
# Output: analysis/output/latent_models/lpa/fig_lpa_items.pdf

fit <- read_rds(here::here("analysis", "output", "latent_models", "lpa", "lpa_fits.rds"))[["model_1_class_4"]]

# the respondents the LPA was fitted on (all three indicators), as in lpa_profiles.R
policy <- \(v) as.numeric(df[[paste0("beleid_voorkeur_", v)]])
dim_mean <- \(v) { s <- rowMeans(sapply(v, policy), na.rm = TRUE); replace(s, is.nan(s), NA) }
in_lpa <- !is.na(dim_mean(c("alcohol", "tabak_en_vapes"))) & !is.na(dim_mean(c("cannabis", "psychedelica", "mdma"))) &
  !is.na(dim_mean(c("cocaine", "speed", "heroine")))
d <- df[in_lpa, ]
post <- as.matrix(fit$dff[grep("^CPROB", names(fit$dff))])
stopifnot(nrow(post) == nrow(d))
pw <- post * as.numeric(d$w)   # posterior x survey weight, one column per profile

# the items, block by block, with a short label
label_of <- \(v) str_squish(attr(d[[v]], "label") %||% v)
drugs <- c(alcohol = "Alcohol", tabak_en_vapes = "Tobacco and vapes", cannabis = "Cannabis", psychedelica = "Psychedelics",
           mdma = "MDMA", cocaine = "Cocaine", speed = "Speed", heroine = "Heroin", pijnstillers = "Painkillers")
beliefs <- paste0("beliefs_", rep(c("society", "addiction", "regulate", "crime"), c(5, 5, 4, 4)), "_",
                  c(1:5, 1:5, 1:4, 1:4))
emo_topic_labels <- c(alc = "Alcohol", tob = "Tobacco", soft = "Soft drugs", hard = "Hard drugs", wod = "War on drugs",
                      legal = "Legalisation")
emo_feeling_labels <- c(angry = "angry", worried = "worried", happy = "happy", none = "none of these")
emo <- expand_grid(t = names(emo_topic_labels), f = names(emo_feeling_labels))

items <- bind_rows(
  tibble(block = "Policy preference\n(1 = ban ... 9 = free sale)", var = paste0("beleid_voorkeur_", names(drugs)),
         label = unname(drugs)),
  tibble(block = "Beliefs\n(1 = strongly disagree ... 7 = strongly agree)", var = beliefs,
         label = map_chr(beliefs, label_of)),
  tibble(block = "Behaviour\n(1-7)", var = paste0("gedrag_", 1:3), label = map_chr(paste0("gedrag_", 1:3), label_of)),
  tibble(block = "Own use\n(1 = never ... 4 = often)", var = paste0("gebruik_", 1:4),
         label = str_remove(map_chr(paste0("gebruik_", 1:4), label_of), "^.*gebruikt\\.\\s*")),
  tibble(block = "Feelings\n(% ticked)", var = paste0("emo_", emo$t, "_", emo$f),
         label = paste0(emo_topic_labels[emo$t], ": ", emo_feeling_labels[emo$f])),
  tibble(block = "Involvement\n(0-100)", var = c("betrokkenheid_1", "betrokkenheid_1b", "betrokkenheid_2", "betrokkenheid_2b",
                                                 "betrokkenheid_3"),
         label = map_chr(c("betrokkenheid_1", "betrokkenheid_1b", "betrokkenheid_2", "betrokkenheid_2b", "betrokkenheid_3"),
                         label_of))
) |>
  mutate(block = fct_inorder(block),
         label = if_else(var %in% anti_items, paste0("* ", label), label),
         label = str_trunc(label, 62),
         pos = row_number())

# the profiles' weighted mean per item (feelings as % ticked)
profile_share <- colSums(pw) / sum(pw)
means <- fit$estimates |>
  filter(Category == "Means") |>
  select(Class, Parameter, Estimate) |>
  pivot_wider(names_from = Parameter, values_from = Estimate)
profile_names <- with(means, sprintf("Profile %d (%.0f%%): legal %.1f, soft %.1f, hard %.1f", Class,
                                     100 * profile_share[Class], 1 + 8 * licit, 1 + 8 * soft, 1 + 8 * hard))

item_means <- items |>
  mutate(m = map(var, \(v) {
    y <- as.numeric(d[[v]])
    ok <- !is.na(y)
    tibble(profile = profile_names, mean = colSums(pw[ok, ] * y[ok]) / colSums(pw[ok, ]))
  })) |>
  unnest(m) |>
  mutate(mean = if_else(str_detect(block, "Feelings"), 100 * mean, mean),
         profile = factor(profile, levels = profile_names))

profile_colours <- setNames(c("#E69F00", "#56B4E9", "#009E73", "#CC79A7"), profile_names)
y_limits <- list(c(1, 9), c(1, 7), c(1, 7), c(1, 4), c(0, 100), c(0, 100))

# one panel per block, its width proportional to its number of items
panels <- imap(levels(items$block), \(b, k) {
  dat <- filter(item_means, block == b)
  ggplot(dat, aes(fct_reorder(label, pos), mean, colour = profile, group = profile)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.3) +
    scale_colour_manual(values = profile_colours, drop = FALSE) +
    scale_y_continuous(limits = y_limits[[k]], breaks = scales::breaks_pretty(n = 5), expand = expansion(mult = 0.03)) +
    labs(x = NULL, y = if (k == 1) "Mean response" else NULL, colour = NULL, title = b) +
    theme_minimal(base_size = 9) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 6.5),
          panel.grid.minor = element_blank(), panel.grid.major.x = element_line(colour = "grey93"),
          plot.title = element_text(size = 8, face = "bold"), legend.position = "top")
})

p <- wrap_plots(panels, nrow = 1, widths = as.numeric(table(items$block)) + 1.5, guides = "collect") +
  plot_annotation(
    title = "The four latent profiles (LPA model 1) across the questionnaire",
    caption = paste("Items in survey order (blocks fixed; order within the policy, belief and behaviour blocks was",
                    "randomised, so these follow the questionnaire's numbering). Answers as given; * = agreeing",
                    "favours prohibition. Means weighted by posterior probability x survey weight;",
                    sprintf("n = %d respondents with all three LPA indicators.", nrow(d))),
    theme = theme(plot.title = element_text(face = "bold", size = 11), plot.caption = element_text(size = 7, hjust = 0))
  ) &
  theme(legend.position = "top")

out <- here::here("analysis", "output", "latent_models", "lpa", "fig_lpa_items.pdf")
ggsave(out, p, width = 17, height = 7.5, device = cairo_pdf)
