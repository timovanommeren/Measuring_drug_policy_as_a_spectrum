library(tidyverse)

source(here::here("analysis", "R", "load_and_clean.R"))

# exploratory: every party in the sample, with its logo, on two axes: support for a set of regimes
# (x) and support for punitive measures (y, beliefs_crime_1-4). Two PDFs:
# - strict regulation (medical/research + specialist shop, regimes 2-6): pooled over drugs, then per drug
# - per drug, strict regulation followed by not banning (regimes 2-9), to compare the two

drug_labels <- c(alcohol = "Alcohol", tabak_en_vapes = "Tobacco and vapes", cannabis = "Cannabis",
                 psychedelica = "Psychedelics", mdma = "MDMA", cocaine = "Cocaine / crack",
                 speed = "Speed", heroine = "Heroin")

regime_sets <- list(
  middle = list(codes = 2:6, short = "strict regulation",
                title = "Support for research, medical or sales via specialty stores (strict regulation)",
                note = "medical/research (2–4) or specialist shop (5–6)"),
  not_ban = list(codes = 2:9, short = "not banning",
                 title = "Support for not banning (any regime other than a ban)",
                 note = "any regime other than a ban (2–9)")
)

y_title <- "Support for punitive measures"

# items 1 and 3 lean liberal, 2 and 4 punitive: reverse 1 and 3 so higher = more punitive
crime_items <- paste0("beliefs_crime_", 1:4)
reversed <- c("beliefs_crime_1", "beliefs_crime_3")

# parties only: non-voters, blank, "Anders" and DENK (n = 1) left out
excluded <- c("Niet-stemmers", "Blanco", "Mocht (nog) niet stemmen", "DENK", "Anders")

crime <- df |>
  mutate(across(all_of(crime_items), \(x) na_if(as.numeric(x), 99)),
         across(all_of(reversed), \(x) 8 - x))

stopifnot(all(unlist(select(crime, all_of(crime_items))) %in% c(1:7, NA)))

alpha <- psych::alpha(select(crime, all_of(crime_items)), check.keys = FALSE, warnings = FALSE)
message(sprintf("Punitive measures, Cronbach's alpha (items 1 and 3 reversed): %.2f", alpha$total$raw_alpha))

# punitive score: mean of the answered items, at least 3 of 4
crime <- crime |>
  mutate(n_items = rowSums(!is.na(pick(all_of(crime_items)))),
         crime = if_else(n_items >= 3, rowMeans(pick(all_of(crime_items)), na.rm = TRUE), NA_real_))

# per respondent and drug whether the answer is in `codes`, plus the share over the 8 drugs ("pooled");
# "no opinion" (99) is already NA
respondents <- function(codes) {
  policy <- df |>
    select(all_of(paste0("beleid_voorkeur_", names(drug_labels)))) |>
    mutate(across(everything(), \(x) if_else(is.na(x), NA_real_, as.numeric(as.numeric(x) %in% codes))))
  stopifnot(all(unlist(policy) %in% c(0, 1, NA)))
  tibble(w = df$w, party = as.character(df$vote_recall_rec), crime = crime$crime) |>
    bind_cols(rename_with(policy, \(x) str_remove(x, "^beleid_voorkeur_"))) |>
    mutate(pooled = rowMeans(pick(all_of(names(drug_labels))), na.rm = TRUE),
           pooled = if_else(is.nan(pooled), NA_real_, pooled)) |>
    filter(!party %in% excluded)
}

resp <- map(regime_sets, \(s) respondents(s$codes))

# one row per party: x = weighted share in the regime set for `drug` ("pooled" = all 8 drugs),
# y = weighted punitive score
party_map <- function(drug, set) {
  resp[[set]] |>
    rename(x = all_of(drug)) |>
    filter(!is.na(x), !is.na(crime)) |>
    summarise(x = weighted.mean(x, w), crime = weighted.mean(crime, w), n = n(), .by = party)
}

# all remaining parties with respondents are on the map
stopifnot(setequal(party_map("pooled", "middle")$party,
                   setdiff(as.character(unique(df$vote_recall_rec)), excluded)))
stopifnot(all(between(party_map("pooled", "not_ban")$x, 0, 1)))

# logos (SVG, Wikimedia Commons / Wikipedia) in analysis/assets/party_logos/<party>.svg;
# a party without one gets a dot
logo_dir <- here::here("analysis", "assets", "party_logos")
logos <- list.files(logo_dir, pattern = "\\.svg$", full.names = TRUE) |>
  set_names(\(f) tools::file_path_sans_ext(basename(f))) |>
  map(\(f) {
    # crop to the visible content (some SVGs, e.g. CDA, lack a viewBox and leave empty canvas)
    b <- rsvg::rsvg(f, width = 800)
    rows <- range(which(rowSums(b[, , 4] > 0) > 0))
    cols <- range(which(colSums(b[, , 4] > 0) > 0))
    b[rows[1]:rows[2], cols[1]:cols[2], ]
  })

# the punitive score does not depend on the drug: same y limits on every page
y_lim <- party_map("pooled", "middle")$crime |> range() |> (\(r) r + c(-1, 1) * 0.12 * diff(r))()

# logos and their "n = " labels as one grob, positioned in panel coordinates (npc) and sized in mm,
# so logos keep their shape; every logo gets the same area for a given n
logo_grob <- function(data, xlim, ylim, scale = 1) {
  grobs <- pmap(filter(data, party %in% names(logos)), \(party, x, crime, n, ...) {
    img <- logos[[party]]
    aspect <- ncol(img) / nrow(img)
    side <- scale * (7 + 9 * sqrt(n / max(data$n)))
    w <- side * sqrt(aspect)
    h <- side / sqrt(aspect)
    px <- grid::unit((x - xlim[1]) / diff(xlim), "npc")
    py <- grid::unit((crime - ylim[1]) / diff(ylim), "npc")
    grid::gList(
      grid::rasterGrob(img, x = px, y = py, width = grid::unit(w, "mm"), height = grid::unit(h, "mm"),
                       interpolate = TRUE),
      grid::textGrob(sprintf("n = %d", n), x = px, y = py - grid::unit(h / 2 + 1.5, "mm"),
                     gp = grid::gpar(fontsize = 6.5 * sqrt(scale), col = "grey30"))
    )
  })
  grid::gTree(children = do.call(grid::gList, grobs))
}

plot_map <- function(drug, set, logo_scale = 1) {
  s <- regime_sets[[set]]
  data <- party_map(drug, set)
  x_ref <- weighted.mean(resp[[set]][[drug]], resp[[set]]$w, na.rm = TRUE)
  xlim <- range(data$x) + c(-1, 1) * 0.1 * diff(range(data$x))
  ggplot(data, aes(x, crime)) +
    geom_hline(yintercept = 4, linetype = "dashed", linewidth = 0.3, colour = "grey50") +
    geom_vline(xintercept = x_ref, linetype = "dashed", linewidth = 0.3, colour = "grey50") +
    annotation_custom(logo_grob(data, xlim, y_lim, logo_scale)) +
    geom_point(data = filter(data, !party %in% names(logos)), size = 2.5, colour = "#3b528b") +
    geom_text(data = filter(data, !party %in% names(logos)), aes(label = sprintf("%s (n = %d)", party, n)),
              vjust = 2, size = 2.4, colour = "grey30") +
    scale_x_continuous(labels = scales::label_percent()) +
    coord_cartesian(xlim = xlim, ylim = y_lim, expand = FALSE) +
    labs(title = sprintf("Parties by support for %s and punitive measures%s", s$short,
                         if (drug == "pooled") ", all drugs pooled" else paste(" —", drug_labels[[drug]])),
         subtitle = paste0(
           "x: weighted % of answers in ", s$note,
           if (drug == "pooled") ", averaged per respondent over the 8 drugs" else "",
           "; dashed line = mean of all respondents.\n",
           sprintf(paste("y: mean of beliefs_crime_1–4 (1–7; items 1 and 3 reversed, at least 3 of 4 answered),",
                         "higher = more punitive; α = %.2f. Dashed line = neutral (4).\n"), alpha$total$raw_alpha),
           "Logo size reflects n. Geen mening excluded; non-voters, blank, Anders and DENK (n = 1) left out. Vote: TK 2025."),
         x = paste(s$title, "(weighted %)"), y = paste(y_title, "(1–7)")) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          plot.margin = margin(10, 20, 10, 10))
}

out_dir <- here::here("analysis", "output", "raw_distributions")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cairo_pdf(file.path(out_dir, "party_map_middle_regimes_crime.pdf"), width = 11, height = 7, onefile = TRUE)
walk(c("pooled", names(drug_labels)), \(drug) print(plot_map(drug, "middle")))
dev.off()

# per drug: strict regulation, then not banning
cairo_pdf(file.path(out_dir, "party_map_middle_vs_not_ban.pdf"), width = 11, height = 7, onefile = TRUE)
walk(names(drug_labels), \(drug) walk(names(regime_sets), \(set) print(plot_map(drug, set))))
dev.off()


## Paper: figure 3b, pooled strict-regulation map; caption in results_table.R (out/fig_gradient.tex)

p_paper <- plot_map("pooled", "middle", logo_scale = 0.6) +
  labs(title = NULL, subtitle = NULL,
       x = "Support for research, medical or sales via specialty stores (strict regulation)",
       y = "Support for punitive measures (1–7)") +
  theme(text = element_text(size = 8), axis.text = element_text(size = 7), plot.margin = margin(4, 8, 4, 4))
ggsave(here::here("out", "fig_party_map.pdf"), p_paper, width = 6.5, height = 3.6, device = cairo_pdf)

# numbers for the caption
write_csv(tibble(alpha = alpha$total$raw_alpha, n = sum(party_map("pooled", "middle")$n)),
          here::here("out", "fig_party_map_stats.csv"))
