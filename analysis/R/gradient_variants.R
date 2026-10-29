library(tidyverse)

source(here::here("analysis", "R", "load_and_clean.R"))

# exploratory: variants of the gradient figure (candidate figure 3). Per drug, weighted 100% stacked
# bars of the regime choice, one row per position on a "group A" score (young, theoretical, urban,
# left). Pages cross two x definitions (4 regimes, 9 answer options) with four y definitions.

drug_labels <- c(alcohol = "Alcohol", tabak_en_vapes = "Tobacco/vapes", cannabis = "Cannabis",
                 psychedelica = "Psychedelics", mdma = "MDMA", speed = "Speed", cocaine = "Cocaine",
                 heroine = "Heroin")

option_labels <- c("Verbod", "Onderzoek", "Behandeling", "Recept", "Speciaalzaak zonder winst",
                   "Speciaalzaak met winst", "Overal, zonder reclame", "Overal, met reclame", "Vrije verkoop")

# regime colours as in raw_distributions.R; the nine options are shades within their regime's hue
regime_colours <- setNames(viridisLite::viridis(9, end = 0.9)[c(1, 3, 6, 8)], str_to_sentence(regime_labels))
option_regime <- c(1, 2, 2, 2, 3, 3, 4, 4, 4)
option_colours <- setNames(c(
  regime_colours[1],
  colorspace::lighten(regime_colours[2], c(0.35, 0.15, -0.1)),
  colorspace::lighten(regime_colours[3], c(0.25, -0.1)),
  colorspace::lighten(regime_colours[4], c(0.35, 0.1, -0.15))
), option_labels)

# group A traits; middle education and moderate urbanisation count 0 in the binary count;
# respondents without urbanisation or a left/right vote are left out
traits <- df |>
  transmute(w,
            age = as.integer(age_4_rec), edu = as.integer(haven::as_factor(edu_3_rec)),
            urb = as.integer(haven::as_factor(urb)), left = as.integer(vote_lr == "left"),
            across(starts_with("beleid_voorkeur_"), as.numeric)) |>
  filter(!is.na(urb), !is.na(left)) |>
  mutate(binary = (age <= 2) + (edu == 3) + (urb <= 2) + left,
         half = (age <= 2) + c(0, 0.5, 1)[edu] + c(1, 1, 0.5, 0, 0)[urb] + left,
         score = (4 - age) / 3 + (edu - 1) / 2 + (5 - urb) / 4 + left)

stopifnot(identical(as.vector(table(traits$binary)), c(154L, 281L, 294L, 274L, 132L)))

# decile breaks on the full ordinal score; ties keep a score in one row, so rows are near-equal
decile_breaks <- unique(quantile(traits$score, seq(0, 1, 0.1)))

score_range <- function(lo, hi) sprintf("%.2f–%.2f", lo, hi)

y_variants <- list(
  binary = list(title = "number of group A traits (0–4)",
                note = "Middle education and moderate urbanisation count as 0.",
                row = \(d) factor(d$binary)),
  half = list(title = "group A traits in half points (0–4)",
              note = "Middle education and moderate urbanisation count as 0.5.",
              row = \(d) factor(d$half)),
  bins = list(title = "full ordinal group A score (0–4), in steps of 0.5",
              note = paste("Age (4 levels), education (3) and urbanisation (5) each scaled 0–1, plus vote",
                           "(left = 1); rows cut at 0.5."),
              row = \(d) cut(d$score, seq(0, 4, 0.5), include.lowest = TRUE,
                             labels = score_range(seq(0, 3.5, 0.5), seq(0.5, 4, 0.5)))),
  deciles = list(title = "full ordinal group A score (0–4), in deciles",
                 note = paste("Age (4 levels), education (3) and urbanisation (5) each scaled 0–1, plus vote",
                              "(left = 1); rows are deciles of the score, labelled by its range."),
                 row = \(d) {
                   r <- cut(d$score, decile_breaks, include.lowest = TRUE)
                   lo <- tapply(d$score, r, min)
                   hi <- tapply(d$score, r, max)
                   factor(r, labels = score_range(lo, hi))
                 })
)

x_variants <- list(
  regimes = list(title = "four regimes", labels = names(regime_colours), colours = regime_colours,
                 code = \(x) cut(x, c(0, 1, 4, 6, 9), labels = FALSE)),
  options = list(title = "nine answer options", labels = option_labels, colours = option_colours,
                 code = \(x) x)
)

# weighted % per answer category in each row and drug; "no opinion" (99) is already NA
gradient <- function(yv, xv) {
  d <- traits |> mutate(row = y_variants[[yv]]$row(traits))
  levels_x <- x_variants[[xv]]$labels
  d |>
    select(w, row, all_of(paste0("beleid_voorkeur_", names(drug_labels)))) |>
    pivot_longer(-c(w, row), names_to = "drug", names_prefix = "beleid_voorkeur_", values_to = "x") |>
    filter(!is.na(x)) |>
    mutate(cat = factor(levels_x[x_variants[[xv]]$code(x)], levels = levels_x)) |>
    summarise(w = sum(w), .by = c(row, drug, cat)) |>
    complete(row, drug, cat, fill = list(w = 0)) |>
    mutate(p = w / sum(w), .by = c(row, drug)) |>
    mutate(drug = factor(drug_labels[drug], levels = drug_labels))
}

# row labels: n per row, with the two ends named
row_labels <- function(yv) {
  r <- y_variants[[yv]]$row(traits)
  n <- table(r)
  lab <- sprintf("%s (n = %d)", names(n), n)
  lab[length(lab)] <- paste0(lab[length(lab)], "\n(young, theoretical, urban, left)")
  lab[1] <- paste0(lab[1], "\n(old, practical, rural, right)")
  setNames(lab, names(n))
}

# white text on dark fills
text_colour <- function(cols) if_else(colSums(col2rgb(cols) * c(0.299, 0.587, 0.114)) / 255 < 0.55, "white", "black")

plot_gradient <- function(yv, xv) {
  data <- gradient(yv, xv)
  cols <- x_variants[[xv]]$colours
  data <- mutate(data, txt = text_colour(cols[as.character(cat)]))
  ggplot(data, aes(p, row, fill = cat)) +
    geom_col(position = position_fill(reverse = TRUE), width = 0.8) +
    # group = cat keeps the labels stacked like the bars (the colour mapping would regroup them)
    geom_text(aes(label = if_else(p >= 0.1, scales::percent(p, 1), ""), colour = txt, group = cat),
              position = position_fill(reverse = TRUE, vjust = 0.5), size = 2.3, show.legend = FALSE) +
    facet_wrap(~drug, nrow = 2) +
    scale_fill_manual(values = cols, breaks = names(cols), drop = FALSE) +
    scale_colour_identity() +
    scale_x_continuous(labels = scales::label_percent(), breaks = c(0, 0.5, 1), expand = expansion(0)) +
    scale_y_discrete(labels = row_labels(yv)) +
    guides(fill = guide_legend(nrow = if (xv == "options") 2 else 1)) +
    labs(title = sprintf("Regime choice by %s — x: %s", y_variants[[yv]]$title, x_variants[[xv]]$title),
         subtitle = paste("Weighted % per drug; labels shown from 10%. Group A: age 18–49, theoretical education,",
                          "high or very high urbanisation, left vote (TK 2025).\n",
                          y_variants[[yv]]$note, "Respondents without urbanisation or a left/right vote left out",
                          sprintf("(n = %d).", nrow(traits))),
         x = NULL, y = NULL, fill = NULL) +
    theme_minimal(base_size = 9) +
    theme(panel.grid = element_blank(),
          strip.text = element_text(face = "bold", size = 10),
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(size = 7.5),
          legend.position = "bottom",
          panel.spacing.x = unit(1.2, "lines"),
          plot.margin = margin(10, 15, 10, 10))
}

# each row sums to 100% per drug, for every variant
for (yv in names(y_variants)) for (xv in names(x_variants)) {
  s <- summarise(gradient(yv, xv), s = sum(p), .by = c(row, drug))$s
  stopifnot(all(abs(s - 1) < 1e-9))
}

pages <- expand_grid(y = names(y_variants), x = names(x_variants)) |> transpose()
stopifnot(length(pages) == 8)

out_dir <- here::here("analysis", "output", "gradient")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
cairo_pdf(file.path(out_dir, "gradient_variants.pdf"), width = 13, height = 6.5, onefile = TRUE)
walk(pages, \(v) print(plot_gradient(v$y, v$x)))
dev.off()
