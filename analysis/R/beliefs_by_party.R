library(tidyverse)

source(here::here("analysis", "R", "load_and_clean.R"))

# exploratory: raw responses to each belief item (society, addiction, regulate, crime) per party,
# as weighted % per answer (one PDF, one page per item)

# item wording from the variable labels
item_labels <- grep("^beliefs_", names(df), value = TRUE) |>
  set_names() |>
  map_chr(\(v) attr(df[[v]], "label"))

stopifnot(length(item_labels) == 18)

# same party grouping as raw_distributions.R: small parties (n < 30, except Volt) and
# "Anders" together, non-voters and blank together
party_n <- count(df, vote_recall_rec)
small <- setdiff(as.character(party_n$vote_recall_rec[party_n$n < 30]), "Volt")
no_vote <- c("Niet-stemmers", "Blanco", "Mocht (nog) niet stemmen")

df <- df |>
  mutate(vote = as.character(vote_recall_rec) |>
           case_match(no_vote ~ "Did not vote / blank",
                      c(small, "Anders") ~ "Other party",
                      .default = as.character(vote_recall_rec)))

answer_labels <- c("Helemaal mee oneens", "Mee oneens", "Enigszins mee oneens", "Neutraal",
                   "Enigszins mee eens", "Mee eens", "Helemaal mee eens", "Geen mening")
answer_colours <- setNames(c(RColorBrewer::brewer.pal(7, "RdBu"), "grey75"), answer_labels)

# one row per respondent x item with an answer, geen mening (99) included
long <- df |>
  select(w, vote, all_of(names(item_labels))) |>
  pivot_longer(all_of(names(item_labels)), names_to = "item", values_to = "x") |>
  filter(!is.na(x)) |>
  mutate(x = as.numeric(x),
         answer = factor(match(x, c(1:7, 99)), levels = 1:8, labels = answer_labels))

stopifnot(!anyNA(long$answer))

# weighted % per answer in each party plus "Total"; parties ordered by weighted mean agreement
# (geen mening excluded from the mean), highest at the top, Total above all
party_dist <- function(item) {
  d <- bind_rows(mutate(filter(long, item == .env$item), category = "Total"),
                 mutate(filter(long, item == .env$item), category = vote))
  order <- d |>
    filter(x != 99) |>
    summarise(mean = weighted.mean(x, w), .by = category) |>
    arrange(category == "Total", mean) |>
    pull(category)
  shares <- d |>
    summarise(w = sum(w), n = n(), .by = c(category, answer)) |>
    complete(category, answer, fill = list(w = 0, n = 0)) |>
    mutate(p = w / sum(w), n = sum(n), .by = category)
  n_of <- setNames(shares$n, shares$category)
  shares |>
    mutate(label = factor(sprintf("%s (n = %d)", category, n),
                          levels = sprintf("%s (n = %d)", order, n_of[order])))
}

# each party sums to 100%
stopifnot(all(abs(summarise(party_dist(names(item_labels)[1]), s = sum(p), .by = category)$s - 1) < 1e-9))

plot_item <- function(item) {
  data <- party_dist(item)
  ggplot(data, aes(p, label, fill = fct_rev(answer))) +
    geom_col(width = 0.75) +
    geom_text(aes(label = if_else(p >= 0.04, scales::percent(p, 1), "")),
              position = position_stack(vjust = 0.5), size = 2.4,
              colour = if_else(data$answer %in% answer_labels[c(1, 7)], "white", "black")) +
    scale_fill_manual(values = answer_colours, breaks = answer_labels) +
    scale_x_continuous(labels = scales::label_percent(), expand = expansion(0)) +
    labs(title = sprintf("%s by vote (TK 2025, party): raw responses", item),
         subtitle = paste0("“", item_labels[[item]], "”\n",
                           "Weighted % per answer; parties ordered by mean agreement (geen mening excluded ",
                           "from the mean), highest at the top."),
         x = "Respondents (weighted %)", y = NULL, fill = NULL) +
    guides(fill = guide_legend(nrow = 1)) +
    theme_minimal(base_size = 10) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          legend.position = "bottom",
          plot.margin = margin(10, 30, 10, 10))
}

out_dir <- here::here("analysis", "output", "raw_distributions")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
cairo_pdf(file.path(out_dir, "beliefs_by_party.pdf"), width = 11, height = 7, onefile = TRUE)
walk(names(item_labels), \(item) print(plot_item(item)))
dev.off()
