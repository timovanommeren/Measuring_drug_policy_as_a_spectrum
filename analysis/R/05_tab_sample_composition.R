# Table: sample composition, unweighted and weighted -> out/tab_sample_composition.tex

source(here::here("analysis", "R", "00_load_and_clean.R"))

comp_vars <- df |>
  transmute(
    Age       = age_group,
    Sex       = fct_recode(sex_rec, Male = "Man", Female = "Vrouw"),
    Education = edu_group,
    Region    = factor(geo_nielsen5_rec, 1:5, c("Three largest cities and suburbs", "Rest of the west",
                                                "North", "East", "South")),
    Urbanisation = factor(urb, 1:5, c("Very high", "High", "Moderate", "Low", "Not urban")),
    `Vote recall, 2025 election` = haven::as_factor(vote_recall_rec) |>
      fct_recode(Other = "Anders", Blank = "Blanco", `Did not vote` = "Niet-stemmers") |>
      fct_drop() |> fct_infreq() |> fct_relevel("Other", "Blank", "Did not vote", after = Inf),
    w
  )

comp <- select(comp_vars, -w) |>
  imap(\(x, v) tibble(variable = v, level = fct_na_value_to_level(x, "Unknown"), w = comp_vars$w) |>
         summarise(n = n(), w = sum(w), .by = c(variable, level)) |>
         arrange(level) |>
         mutate(unweighted = 100 * n / sum(n), weighted = 100 * w / sum(w))) |>
  list_rbind()

rows <- comp |>
  mutate(row = sprintf("\\quad %s & %d & %.1f & %.1f \\\\", str_replace(level, "\u2013", "--"), n, unweighted, weighted)) |>
  summarise(rows = paste(c(sprintf("\\addlinespace\n\\multicolumn{4}{@{}l}{\\textit{%s}} \\\\", first(variable)), row),
                         collapse = "\n"), .by = variable)

writeLines(c(
  "\\begin{table}[htbp]",
  "\\centering\\small",
  sprintf(paste("\\caption{Sample composition ($n = %s$): number of respondents, unweighted and weighted",
                "percentages. Region in Nielsen regions; urbanisation in CBS address-density classes.}"), format(nrow(df), big.mark = "{,}")),
  "\\label{tab:sample_composition}",
  "\\begin{tabular}{@{}lrrr@{}}",
  "\\toprule",
  " & $n$ & Unweighted \\% & Weighted \\% \\\\",
  "\\midrule",
  rows$rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
), here::here("out", "tab_sample_composition.tex"))
