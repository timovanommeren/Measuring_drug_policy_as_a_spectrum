library(tidyverse)

source(here::here("analysis", "R", "load_and_clean.R"))
source(here::here("analysis", "R", "party_logos.R"))

# exploratory: every party in the sample, with its logo, by preferred policy for illicit drugs (x)
# and for legal drugs (y). Two PDFs, one per measure (mean preferred policy; support for the middle
# options), each with one page per split: cannabis counted with the illicit drugs or not, crossed
# with alcohol only or alcohol and tobacco

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

# per answer (1-9) the value it contributes; a respondent's score is the mean over the drugs answered
measures <- list(
  mean = list(value = \(x) x, file = "party_map_illicit_vs_legal_mean.pdf", percent = FALSE,
              title = "mean preferred policy", axis = "Mean preferred policy",
              note = "the mean answer (1 = ban … 9 = free sale)", scale = " (1 = ban … 9 = free sale)"),
  middle = list(value = \(x) as.numeric(x %in% 2:6), file = "party_map_illicit_vs_legal_middle.pdf",
                percent = TRUE, title = "support for the middle options", axis = "Support for the middle options",
                note = "the share of answers in medical/research (2–4) or specialist shop (5–6)",
                scale = " (weighted %)")
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

party_map <- function(split, measure) {
  respondent_scores(split, measure) |>
    summarise(x = weighted.mean(x, w), y = weighted.mean(y, w), n = n(), .by = party)
}

# all remaining parties with respondents are on every map
for (s in splits) for (m in names(measures)) {
  stopifnot(setequal(party_map(s, m)$party, setdiff(as.character(unique(df$vote_recall_rec)), excluded)))
}
stopifnot(all(between(unlist(map(splits, \(s) party_map(s, "middle")[c("x", "y")])), 0, 1)))

pad <- \(r) r + c(-1, 1) * 0.08 * diff(r)

plot_map <- function(split, measure, xlim, ylim, logo_scale = 1) {
  m <- measures[[measure]]
  data <- party_map(split, measure)
  all <- respondent_scores(split, measure)
  ref <- c(x = weighted.mean(all$x, all$w), y = weighted.mean(all$y, all$w))
  axis_labels <- if (m$percent) scales::label_percent() else waiver()
  ggplot(data, aes(x, y)) +
    geom_hline(yintercept = ref[["y"]], linetype = "dashed", linewidth = 0.3, colour = "grey50") +
    geom_vline(xintercept = ref[["x"]], linetype = "dashed", linewidth = 0.3, colour = "grey50") +
    annotation_custom(logo_grob(data, xlim, ylim, logo_scale)) +
    geom_point(data = filter(data, !party %in% names(logos)), size = 2.5, colour = "#3b528b") +
    scale_x_continuous(labels = axis_labels) +
    scale_y_continuous(labels = axis_labels) +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(title = sprintf("Parties by %s: %s vs %s", m$title, split$x_label, split$y_label),
         subtitle = paste0(
           "Per respondent ", m$note, " over the drugs answered, then the weighted mean per party.\n",
           "x: ", paste(drug_names[split$x], collapse = ", "), "; y: ",
           paste(drug_names[split$y], collapse = " and "), ". Dashed lines = mean of all respondents.\n",
           "Same axis ranges on every page; x and y differ in range. Logo size reflects n.\n",
           "Geen mening excluded; non-voters, blank, Anders and DENK (n = 1) left out. Vote: TK 2025."),
         x = paste0(m$axis, ", ", split$x_label, m$scale), y = paste0(m$axis, ", ", split$y_label, m$scale)) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          plot.margin = margin(10, 20, 10, 10))
}

out_dir <- here::here("analysis", "output", "party_maps")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (measure in names(measures)) {
  # the same axis ranges on every page of a PDF, so the pages compare
  maps <- map(splits, \(s) party_map(s, measure))
  xlim <- pad(range(map(maps, "x")))
  ylim <- pad(range(map(maps, "y")))
  cairo_pdf(file.path(out_dir, measures[[measure]]$file), width = 11, height = 7, onefile = TRUE)
  walk(splits, \(s) print(plot_map(s, measure, xlim, ylim)))
  dev.off()
}


## Paper: figure 4, middle options for illicit drugs with cannabis vs alcohol and tobacco (page 4 of
## party_map_illicit_vs_legal_middle.pdf)

paper_split <- splits[[4]]
paper_map <- party_map(paper_split, "middle")
# more room around the logos than on the exploratory pages
pad_paper <- \(r) r + c(-1, 1) * 0.13 * diff(r)
p_paper <- plot_map(paper_split, "middle", pad_paper(range(paper_map$x)), pad_paper(range(paper_map$y)),
                    logo_scale = 0.6) +
  labs(title = NULL, subtitle = NULL,
       x = "Support for strict regulation: cannabis and illicit drugs",
       y = "Support for strict regulation:\nalcohol and tobacco") +
  theme(text = element_text(size = 8), axis.text = element_text(size = 7), plot.margin = margin(4, 8, 4, 4))
ggsave(here::here("out", "fig_party_map.pdf"), p_paper, width = 6.5, height = 3.6, device = cairo_pdf)

caption <- paste0(
  r"(Parties by support for strict regulation of cannabis and the illicit drugs (horizontal) and of alcohol )",
  r"(and tobacco (vertical). Per respondent the share of answers in medical/research (2--4) or specialist )",
  r"(shop (5--6) over the substances answered, then the weighted mean per party: horizontal over cannabis, )",
  r"(psychedelics, MDMA, cocaine, speed and heroin, vertical over alcohol and tobacco. Dashed lines are the )",
  sprintf(r"(sample means. Logo size reflects $n$ ($n = %d$ in total); DENK ($n = 1$), other parties, blank and )",
          sum(paper_map$n)),
  r"(non-voters are left out. Vote: TK 2025.)"
)

writeLines(c(
  r"(\begin{figure}[H])",
  r"(  \centering)",
  r"(  \includegraphics[width=\linewidth]{out/fig_party_map.pdf})",
  paste0(r"(  \caption{)", caption, "}"),
  r"(  \label{fig:party_map})",
  r"(\end{figure})"
), here::here("out", "fig_party_map.tex"))
