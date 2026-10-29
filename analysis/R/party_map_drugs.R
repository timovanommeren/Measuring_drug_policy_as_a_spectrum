library(tidyverse)

source(here::here("analysis", "R", "load_and_clean.R"))
source(here::here("analysis", "R", "party_logos.R"))

# exploratory: every party in the sample, with its logo, by preferred policy for two sets of drugs.
# Two PDFs:
# - mean preferred policy for every legal drug (y) against every illicit drug (x), cannabis counting as
#   both and speed and heroin left out (11 pages)
# - support for the middle options for illicit drugs (x) vs legal drugs (y): cannabis counted with the
#   illicit drugs or not, crossed with alcohol only or alcohol and tobacco (4 pages; page 4 = figure 4)
# - the drug groups of figure 2b (legal, soft, hard): mean preferred policy and support for the middle
#   options per comparison (10 pages)

drug_names <- c(alcohol = "alcohol", tabak_en_vapes = "tobacco and vapes", cannabis = "cannabis",
                psychedelica = "psychedelics", mdma = "MDMA", cocaine = "cocaine", speed = "speed",
                heroine = "heroin")
hard <- c("psychedelica", "mdma", "cocaine", "speed", "heroine")

splits <- list(
  list(x = hard, y = "alcohol", x_label = "illicit drugs, without cannabis", y_label = "alcohol"),
  list(x = hard, y = c("alcohol", "tabak_en_vapes"), x_label = "illicit drugs, without cannabis",
       y_label = "alcohol and tobacco"),
  list(x = c("cannabis", hard), y = "alcohol", x_label = "illicit drugs, with cannabis", y_label = "alcohol"),
  list(x = c("cannabis", hard), y = c("alcohol", "tabak_en_vapes"), x_label = "illicit drugs, with cannabis",
       y_label = "alcohol and tobacco")
)

# every legal drug (y) against every illicit drug (x); cannabis counts as both, speed and heroin left out
pair_legal <- c("alcohol", "tabak_en_vapes", "cannabis")
pair_illicit <- c("cannabis", "psychedelica", "mdma", "cocaine")
pairs <- expand_grid(y = pair_legal, x = pair_illicit) |>
  filter(x != y) |>
  pmap(\(y, x) list(x = x, y = y, x_label = drug_names[[x]], y_label = drug_names[[y]]))
stopifnot(length(pairs) == 11)
# in front: all illicit drugs, cannabis included, against alcohol and tobacco
pairs <- c(list(list(x = c("cannabis", hard), y = c("alcohol", "tabak_en_vapes"),
                     x_label = "illicit drugs, with cannabis", y_label = "alcohol and tobacco")), pairs)

# the drug groups of figure 2b: legal against all illicit drugs, against cannabis, and each pair of the
# legal, soft and hard drugs (the stricter group on x)
legal <- c("alcohol", "tabak_en_vapes")
soft <- c("cannabis", "psychedelica", "mdma")
hard_group <- c("speed", "cocaine", "heroine")
groups <- list(
  list(x = c(soft, hard_group), y = legal, x_label = "all illicit drugs", y_label = "legal drugs"),
  list(x = "cannabis", y = legal, x_label = "cannabis", y_label = "legal drugs"),
  list(x = soft, y = legal, x_label = "soft drugs", y_label = "legal drugs"),
  list(x = hard_group, y = legal, x_label = "hard drugs", y_label = "legal drugs"),
  list(x = hard_group, y = soft, x_label = "hard drugs", y_label = "soft drugs")
)

# tick labels for mean preferred policy: the answer options (as in figure 2) with their number
option_ticks <- c("Ban (1)", "Research only (2)", "Treatment only (3)", "Prescription only (4)",
                  "Specialist shop,\nnon-profit (5)", "Specialist shop,\nfor-profit (6)",
                  "Sold everywhere,\nno advertising (7)", "Sold everywhere,\nwith advertising (8)", "Free sale (9)")

# quadrants around the sample means (mean preferred policy): liberal on the legal drugs (top) or not,
# on the illicit drugs (right) or not
quadrants <- tibble(
  qx = c("left", "right", "left", "right"), qy = c("top", "top", "bottom", "bottom"),
  label = c("Double standards (status quo)", "Liberals", "Abstainers", "Reformers"),
  fill = c("#E69F00", "#009E73", "#56B4E9", "#CC79A7")
)

# stat "mean": per answer (1-9) the value it contributes, a respondent's score is the mean over the drugs
# answered, then the weighted mean per party. stat "median" / "mode": per party the weighted median /
# most chosen answer over all its answers on the drugs of an axis.
# same_axes: the same axis ranges on every page (when the pages show variants of one comparison)
measures <- list(
  mean = list(stat = "mean", value = \(x) x, file = "party_map_drug_pairs_mean.pdf", percent = FALSE,
              splits = pairs, same_axes = FALSE, quadrants = TRUE, title = "mean preferred policy",
              axis = "Mean preferred policy", scale = "", ref_note = "mean of all respondents",
              method = "Per respondent the mean answer (1 = ban … 9 = free sale) over the drugs answered, then the weighted mean per party."),
  median = list(stat = "median", value = \(x) x, file = "party_map_drug_pairs_median.pdf", percent = FALSE,
                splits = pairs, same_axes = FALSE, quadrants = TRUE, title = "median preferred policy",
                axis = "Median preferred policy", scale = "", ref_note = "median of all answers",
                method = paste("Per party the weighted median of all its answers on the drugs of an axis, interpolated",
                               "within the median answer\n(grouped-data median), so parties with the same median answer",
                               "still differ.")),
  mode = list(stat = "mode", value = \(x) x, file = "party_map_drug_pairs_mode.pdf", percent = FALSE,
              splits = pairs, same_axes = FALSE, quadrants = TRUE, title = "most chosen policy",
              axis = "Most chosen policy", scale = "",
              ref_note = "median of all answers (interpolated)",
              method = paste("Per party the most chosen answer (weighted) over all its answers on the drugs of an axis;",
                             "parties with the same answers are spread within the cell.")),
  mean_median = list(stat = "mean_median", value = \(x) x, file = "party_map_drug_pairs_mean_of_medians.pdf",
                     percent = FALSE, splits = pairs, same_axes = FALSE, quadrants = TRUE,
                     title = "mean of the median policies", axis = "Mean of the median policies", scale = "",
                     ref_note = "mean of the per-drug medians of all answers",
                     method = paste("Per party and drug the weighted median answer (interpolated, grouped-data median),",
                                    "then the mean over the drugs of an axis.\nWith one drug on an axis this is",
                                    "the median itself.")),
  mean_mode = list(stat = "mean_mode", value = \(x) x, file = "party_map_drug_pairs_mean_of_modes.pdf",
                   percent = FALSE, splits = pairs, same_axes = FALSE, quadrants = TRUE,
                   title = "mean of the most chosen policies", axis = "Mean of the most chosen policies", scale = "",
                   ref_note = "mean of the per-drug medians of all answers",
                   method = paste("Per party and drug the most chosen answer (weighted), then the mean over the drugs",
                                  "of an axis. With one drug on an axis this is\nthe most chosen answer itself;",
                                  "parties with the same values are spread within the cell.")),
  mean_groups = list(stat = "mean", value = \(x) x, file = NULL, percent = FALSE, splits = groups,
                     same_axes = FALSE, quadrants = TRUE, title = "mean preferred policy",
                     axis = "Mean preferred policy", scale = "", ref_note = "mean of all respondents",
                     method = "Per respondent the mean answer (1 = ban … 9 = free sale) over the drugs answered, then the weighted mean per party."),
  middle_groups = list(stat = "mean", value = \(x) as.numeric(x %in% 2:6), file = NULL, percent = TRUE,
                       splits = groups, same_axes = FALSE, quadrants = FALSE,
                       title = "support for the middle options", axis = "Support for the middle options",
                       scale = " (weighted %)", ref_note = "mean of all respondents",
                       method = paste("Per respondent the share of answers in medical/research (2–4) or specialist shop (5–6)",
                                      "over the drugs answered, then the weighted mean per party.")),
  middle = list(stat = "mean", value = \(x) as.numeric(x %in% 2:6), file = "party_map_illicit_vs_legal_middle.pdf",
                percent = TRUE, splits = splits, same_axes = TRUE, quadrants = FALSE,
                title = "support for the middle options", axis = "Support for the middle options",
                scale = " (weighted %)", ref_note = "mean of all respondents",
                method = paste("Per respondent the share of answers in medical/research (2–4) or specialist shop (5–6)",
                               "over the drugs answered, then the weighted mean per party."))
)

# parties only: non-voters, blank, "Anders" and DENK (n = 1) left out
excluded <- c("Niet-stemmers", "Blanco", "Mocht (nog) niet stemmen", "DENK", "Anders")

# "no opinion" (99) is already NA
policy <- df |>
  transmute(w, party = as.character(vote_recall_rec),
            across(all_of(paste0("beleid_voorkeur_", names(drug_names))), as.numeric)) |>
  rename_with(\(x) str_remove(x, "^beleid_voorkeur_")) |>
  filter(!party %in% excluded)

stopifnot(all(unlist(select(policy, -w, -party)) %in% c(1:9, NA)))

# per respondent: mean value over the drugs answered; NA when none answered
block_score <- function(drugs, value) {
  s <- rowMeans(as.matrix(mutate(select(policy, all_of(drugs)), across(everything(), value))), na.rm = TRUE)
  if_else(is.nan(s), NA_real_, s)
}

# respondents with both scores, and one row per party
respondent_scores <- function(split, measure) {
  value <- measures[[measure]]$value
  policy |>
    mutate(x = block_score(split$x, value), y = block_score(split$y, value)) |>
    filter(!is.na(x), !is.na(y))
}

# weighted median of answers 1-9, interpolated within the median answer k (between k - 0.5 and k + 0.5)
interp_median <- function(a, w) {
  p <- tapply(w, factor(a, levels = 1:9), sum, default = 0)
  cum <- cumsum(p / sum(p))
  k <- unname(which(cum >= 0.5)[1])
  below <- if (k == 1) 0 else cum[[k - 1]]
  k - 0.5 + (0.5 - below) / (cum[[k]] - below)
}

# weighted most chosen answer
w_mode <- function(a, w) as.numeric(names(which.max(tapply(w, a, sum))))

# all answers (respondent x drug) on `drugs` of the respondents in `d`
pooled <- function(d, drugs) {
  d |>
    select(w, party, all_of(drugs)) |>
    pivot_longer(all_of(drugs), values_to = "a") |>
    filter(!is.na(a))
}

party_map <- function(split, measure) {
  m <- measures[[measure]]
  d <- respondent_scores(split, measure)
  if (m$stat == "mean") {
    return(summarise(d, x = weighted.mean(x, w), y = weighted.mean(y, w), n = n(), .by = party))
  }
  f <- if (m$stat %in% c("median", "mean_median")) interp_median else w_mode
  by_party <- if (startsWith(m$stat, "mean_")) {
    # per drug, then the mean over the drugs (each drug counts equally)
    \(drugs, name) map(drugs, \(drug) pooled(d, drug) |> summarise(v = f(a, w), .by = party)) |>
      list_rbind() |>
      summarise("{name}" := mean(v), .by = party)
  } else {
    \(drugs, name) pooled(d, drugs) |> summarise("{name}" := f(a, w), .by = party)
  }
  count(d, party) |>
    left_join(by_party(split$x, "x"), by = "party") |>
    left_join(by_party(split$y, "y"), by = "party") |>
    select(party, x, y, n)
}

# sample value for the dashed lines: the mean, or for the median and mode the interpolated median
sample_ref <- function(split, measure) {
  d <- respondent_scores(split, measure)
  if (measures[[measure]]$stat == "mean") return(c(x = weighted.mean(d$x, d$w), y = weighted.mean(d$y, d$w)))
  med <- \(drugs) with(pooled(d, drugs), interp_median(a, w))
  # mean of the per-drug statistics: the mean over the drugs of the per-drug median
  if (startsWith(measures[[measure]]$stat, "mean_")) med <- \(drugs) mean(map_dbl(drugs, \(drug) with(pooled(d, drug), interp_median(a, w))))
  c(x = med(split$x), y = med(split$y))
}

# mode: parties in the same cell on a small grid inside it, the largest first. `aspect` = a cell's width
# over its height on the page, so wide cells get more columns than rows
spread_in_cells <- function(data, aspect) {
  data |>
    arrange(desc(n)) |>
    mutate(cell_x = x, cell_y = y) |>
    mutate(k = n(), i = row_number(), cols = pmin(k, ceiling(sqrt(k * aspect))), rows = ceiling(k / cols),
           col = (i - 1) %% cols, row = (i - 1) %/% cols,
           x = cell_x + if_else(k > 1, (col - (cols - 1) / 2) * 0.95 / cols, 0),
           y = cell_y - if_else(k > 1, (row - (rows - 1) / 2) * 0.9 / rows, 0),
           .by = c(cell_x, cell_y)) |>
    select(party, x, y, n)
}

# all remaining parties with respondents are on every map
for (m in names(measures)) for (s in measures[[m]]$splits) {
  stopifnot(setequal(party_map(s, m)$party, setdiff(as.character(unique(df$vote_recall_rec)), excluded)))
}
stopifnot(all(between(unlist(map(splits, \(s) party_map(s, "middle")[c("x", "y")])), 0, 1)))

pad <- \(r) r + c(-1, 1) * 0.08 * diff(r)

plot_map <- function(split, measure, xlim, ylim, logo_scale = 1, quad_size = 3.6, repel_mm = NULL) {
  m <- measures[[measure]]
  data <- party_map(split, measure)
  # the panel is about 9.3 x 4.6 inches on the 11 x 7 page
  if (m$stat %in% c("mode", "mean_mode")) data <- spread_in_cells(data, aspect = (9.3 / diff(xlim)) / (4.6 / diff(ylim)))
  # logos moved apart on a panel of `repel_mm` (width, height in mm), with a line from the party's own
  # position to its logo when it moved
  repel_layers <- NULL
  if (!is.null(repel_mm)) {
    data <- repel_logos(data, xlim, ylim, repel_mm, logo_scale)
    moved <- filter(data, abs(x - x0) / diff(xlim) * repel_mm[1] > 1 | abs(y - y0) / diff(ylim) * repel_mm[2] > 1)
    repel_layers <- geom_segment(data = moved, aes(x0, y0, xend = x, yend = y), colour = "grey55", linewidth = 0.25)
  }
  ref <- sample_ref(split, measure)
  # mode: thin lines between the answer cells
  cell_lines <- if (m$stat %in% c("mode", "mean_mode")) list(
    geom_vline(xintercept = 0.5 + 1:8, colour = "grey85", linewidth = 0.3),
    geom_hline(yintercept = 0.5 + 1:8, colour = "grey85", linewidth = 0.3)
  )
  # mean preferred policy: ticks at the answer options, named; scale limits = the plotted range, so
  # every option inside it gets a tick
  x_scale <- if (m$percent) {
    scale_x_continuous(labels = scales::label_percent())
  } else {
    scale_x_continuous(breaks = 1:9, labels = option_ticks, limits = xlim, oob = scales::oob_keep)
  }
  y_scale <- if (m$percent) {
    scale_y_continuous(labels = scales::label_percent())
  } else {
    scale_y_continuous(breaks = 1:9, labels = option_ticks, limits = ylim, oob = scales::oob_keep)
  }
  # tinted quadrants split at the sample means, each named in its outer corner
  quad <- quadrants |>
    mutate(xmin = if_else(qx == "left", xlim[1], ref[["x"]]), xmax = if_else(qx == "left", ref[["x"]], xlim[2]),
           ymin = if_else(qy == "bottom", ylim[1], ref[["y"]]), ymax = if_else(qy == "bottom", ref[["y"]], ylim[2]),
           lx = if_else(qx == "left", xlim[1] + 0.01 * diff(xlim), xlim[2] - 0.01 * diff(xlim)),
           ly = if_else(qy == "bottom", ylim[1] + 0.015 * diff(ylim), ylim[2] - 0.015 * diff(ylim)),
           hjust = if_else(qx == "left", 0, 1), vjust = if_else(qy == "bottom", 0, 1))
  quad_layers <- if (m$quadrants) list(
    geom_rect(data = quad, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = quad$fill,
              alpha = 0.1, inherit.aes = FALSE),
    geom_text(data = quad, aes(lx, ly, label = label, hjust = hjust, vjust = vjust),
              colour = colorspace::darken(quad$fill, 0.35), fontface = "bold", size = quad_size, inherit.aes = FALSE)
  )
  ggplot(data, aes(x, y)) +
    quad_layers +
    cell_lines +
    geom_hline(yintercept = ref[["y"]], linetype = "dashed", linewidth = 0.3, colour = "grey50") +
    geom_vline(xintercept = ref[["x"]], linetype = "dashed", linewidth = 0.3, colour = "grey50") +
    repel_layers +
    annotation_custom(logo_grob(data, xlim, ylim, logo_scale)) +
    geom_point(data = filter(data, !party %in% names(logos)), size = 2.5, colour = "#3b528b") +
    x_scale +
    y_scale +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(title = sprintf("Parties by %s: %s vs %s", m$title, split$x_label, split$y_label),
         subtitle = paste0(
           m$method, "\n",
           "x: ", paste(drug_names[split$x], collapse = ", "), "; y: ",
           paste(drug_names[split$y], collapse = ", "), ". Dashed lines = ", m$ref_note, ".\n",
           if (m$same_axes) "Same axis ranges on every page; x and y differ in range. "
           else "Axis ranges fitted per page and per axis. ",
           "Logo size reflects n.\n",
           "Geen mening excluded; non-voters, blank, Anders and DENK (n = 1) left out. Vote: TK 2025."),
         x = paste0(m$axis, ", ", split$x_label, m$scale), y = paste0(m$axis, ", ", split$y_label, m$scale)) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          axis.text = element_text(lineheight = 0.9),
          plot.margin = margin(10, 45, 10, 10))
}

out_dir <- here::here("analysis", "output", "party_maps")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

pages <- list()
for (measure in names(measures)) {
  m <- measures[[measure]]
  maps <- map(m$splits, \(s) party_map(s, measure))
  # the same axis ranges on every page of a PDF, so the pages compare; otherwise fitted per page
  lims <- if (m$same_axes) {
    rep(list(list(x = pad(range(map(maps, "x"))), y = pad(range(map(maps, "y"))))), length(maps))
  } else {
    # wider margins with quadrants, so the corner labels stay clear of the logos; mode: whole answer
    # cells, with room for the spread logos and the corner labels
    pad_page <- if (m$stat %in% c("mode", "mean_mode")) \(r) r + c(-1, 1) else if (m$quadrants) \(r) r + c(-1, 1) * 0.16 * diff(r) else pad
    pmap(list(maps, m$splits), \(d, s) {
      ref <- sample_ref(s, measure)
      list(x = pad_page(range(c(d$x, ref[["x"]]))), y = pad_page(range(c(d$y, ref[["y"]]))))
    })
  }
  # smaller logos on pages where parties share a point (and are spread within its cell)
  logo_scale <- map_dbl(maps, \(d) if (anyDuplicated(d[c("x", "y")]) > 0) 0.5 else 1)
  pages[[measure]] <- pmap(list(m$splits, lims, logo_scale), \(s, l, ls) plot_map(s, measure, l$x, l$y, ls))
  if (is.null(m$file)) next
  cairo_pdf(file.path(out_dir, m$file), width = 11, height = 7, onefile = TRUE)
  walk(pages[[measure]], print)
  dev.off()
}

# drug groups: per comparison the mean preferred policy, then the support for the middle options
cairo_pdf(file.path(out_dir, "party_map_drug_groups_mean_middle.pdf"), width = 11, height = 7, onefile = TRUE)
walk2(pages$mean_groups, pages$middle_groups, \(a, b) { print(a); print(b) })
dev.off()


## Paper: figure 4, mean preferred policy for all illicit drugs vs the legal drugs (page 1 of
## party_map_drug_groups_mean_middle.pdf)

paper_split <- groups[[1]]
paper_map <- party_map(paper_split, "mean_groups")
paper_ref <- sample_ref(paper_split, "mean_groups")
# room for the corner labels, as on the exploratory pages
pad_paper <- \(r) r + c(-1, 1) * 0.16 * diff(r)
p_paper <- plot_map(paper_split, "mean_groups", pad_paper(range(c(paper_map$x, paper_ref[["x"]]))),
                    pad_paper(range(c(paper_map$y, paper_ref[["y"]]))), logo_scale = 0.5, quad_size = 2.6,
                    repel_mm = c(5.45, 3.95) * 25.4) +
  labs(title = NULL, subtitle = NULL,
       x = "Mean preferred policy: illicit drugs",
       y = "Mean preferred policy:\nalcohol and tobacco") +
  theme(text = element_text(size = 8), axis.text = element_text(size = 7), plot.margin = margin(4, 8, 4, 4))
ggsave(here::here("out", "fig_party_map.pdf"), p_paper, width = 6.5, height = 4.6, device = cairo_pdf)

caption <- paste0(
  r"((a) Parties by mean preferred policy for the illicit drugs (horizontal) and for alcohol and tobacco )",
  r"((vertical), from ban (1) to free sale (9). Per respondent the mean answer over the substances answered, )",
  r"(then the weighted mean per party: horizontal over cannabis, psychedelics, MDMA, speed, cocaine and heroin, )",
  r"(vertical over alcohol and tobacco. Dashed lines are the sample means and split the map into four )",
  r"(quadrants: more liberal than average on the legal drugs only (double standards, the status quo), on both )",
  r"((liberals), on neither (abstainers) or on the illicit drugs only (reformers). )",
  sprintf(r"(Logo size reflects $n$ ($n = %d$ in total); DENK ($n = 1$), other parties, blank and )",
          sum(paper_map$n)),
  r"(non-voters are left out. Vote: TK 2025. )",
  r"((b) The four quadrants on the U-curve of harms by regulatory regime, adapted from the Transform Drug )",
  r"(Policy Foundation: the arrows show the direction of change each quadrant prefers, for the illicit drugs )",
  r"((left) and for alcohol, tobacco and vapes (right).)"
)

# panel b (figures/fig_regulation_quadrants.pdf) is drawn separately; the panel letters are set in LaTeX
writeLines(c(
  r"(\begin{figure}[H])",
  r"(  \centering)",
  r"(  \makebox[\linewidth][l]{\textbf{a}})",
  r"(  \includegraphics[width=\linewidth]{out/fig_party_map.pdf}\\[4pt])",
  r"(  \makebox[\linewidth][l]{\textbf{b}})",
  r"(  \includegraphics[width=0.72\linewidth]{figures/fig_regulation_quadrants.pdf})",
  paste0(r"(  \caption{)", caption, "}"),
  r"(  \label{fig:party_map})",
  r"(\end{figure})"
), here::here("out", "fig_party_map.tex"))
