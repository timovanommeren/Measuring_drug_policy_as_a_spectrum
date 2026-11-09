# =============================================================================
# report_helpers.R
# -----------------------------------------------------------------------------
# Helpers for the descriptive report (analysis/reports/descriptive_plots/).
# They work on `df` from 00_load_and_clean.R and take its conventions as given:
# export variable names, codes as delivered (policy 1-9, use 1-4), "geen
# mening" already NA, question wording and answer labels from the .sav /
# codebook labels on the columns.
#
# Everything is WEIGHTED with the survey weight `w` unless a function says
# otherwise. Counts are weighted counts (sums of weights; the weights average 1,
# so they read as respondents), and every `n` reported beside them is the
# unweighted number of respondents.
# =============================================================================

library(tidyverse)


# ---- Weighted statistics -------------------------------------------------------
w_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

# weighted SD, with the n / (n - 1) correction of the unweighted SD
w_sd <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  n <- sum(ok)
  if (n < 2) return(NA_real_)
  sqrt(sum(w[ok] * (x[ok] - w_mean(x, w))^2) / sum(w[ok]) * n / (n - 1))
}

# weighted quantile, interpolating between order statistics placed at their cumulative weights: with equal
# weights this is exactly quantile()'s default (type 7)
w_quantile <- function(x, w, probs) {
  ok <- !is.na(x) & !is.na(w)
  o <- order(x[ok])
  cw <- cumsum(w[ok][o]) * sum(ok) / sum(w[ok])          # cumulative weight, scaled to 1..n
  stats::approx(cw, x[ok][o], xout = probs * (sum(ok) - 1) + 1, rule = 2, ties = "ordered")$y
}

# weighted counts of codes 1..k
w_tabulate <- function(codes, w, k) vapply(seq_len(k), \(i) sum(w[!is.na(codes) & codes == i]), numeric(1))

# weighted correlation matrix, pairwise complete. Spearman = weighted Pearson correlation of the ranks within
# each complete pair, so that with equal weights this is exactly cor(use = "pairwise.complete.obs").
w_cor <- function(x, w, method = c("pearson", "spearman")) {
  method <- match.arg(method)
  x <- as.matrix(x)
  k <- ncol(x)
  R <- diag(k)
  dimnames(R) <- list(colnames(x), colnames(x))
  for (i in seq_len(k)) for (j in seq_len(k)) if (i < j) {
    ok <- !is.na(x[, i]) & !is.na(x[, j])
    xy <- cbind(x[ok, i], x[ok, j])
    if (method == "spearman") xy <- apply(xy, 2, rank)
    R[i, j] <- R[j, i] <- if (sum(ok) < 3) NA_real_ else
      suppressWarnings(stats::cov.wt(xy, wt = w[ok], cor = TRUE)$cor[1, 2])
  }
  R
}


# ---- Labels --------------------------------------------------------------------
# a column's question wording: its .sav variable label
var_label <- function(col, data = df) str_squish(attr(data[[col]], "label") %||% col)

# a column's answer options (code -> label) from its value labels, without "Geen mening" (NA in df)
answer_scale <- function(col, data = df) {
  l <- attr(data[[col]], "labels")
  l <- l[l != 99]
  tibble(code = unname(l), label = names(l))
}

# item wording without the question stem a block shares, e.g. "Welk beleid vindt u het beste? Alcohol" ->
# "Alcohol". The stem is the longest common prefix of the block's labels up to a "?", "." or ":".
item_titles <- function(cols, data = df) {
  labs <- vapply(cols, var_label, "", data = data, USE.NAMES = FALSE)
  if (length(labs) < 2) return(labs)
  chars <- strsplit(labs, "")
  n <- min(lengths(chars))
  same <- vapply(seq_len(n), \(i) length(unique(vapply(chars, `[`, "", i))) == 1, logical(1))
  common <- substr(labs[1], 1, if (all(same)) n else which(!same)[1] - 1)
  stem <- str_extract(common, "^.*[?.:]\\s")
  if (is.na(stem)) labs else str_squish(str_remove(labs, fixed(stem)))
}

# item wording numbered by its position in the block ("3. ...")
numbered_titles <- function(cols, data = df) paste0(seq_along(cols), ". ", item_titles(cols, data))

# the emotion labels: per topic its name ("Alcohol") and per feeling its label ("Bang / bezorgd"), split off the
# .sav labels "<stem>. <Topic> <Feeling>"
emotion_labels <- function(topics = emo_topics, feelings = emo_feelings, data = df) {
  titles <- matrix(item_titles(as.vector(outer(paste0("emo_", topics, "_"), feelings, paste0)), data),
                   nrow = length(topics))
  feeling <- vapply(seq_along(feelings), \(j) {
    words <- strsplit(titles[, j], " ")
    k <- 1
    while (k < min(lengths(words)) &&
           length(unique(vapply(words, \(x) paste(tail(x, k + 1), collapse = " "), ""))) == 1) k <- k + 1
    paste(tail(words[[1]], k), collapse = " ")
  }, "")
  topic <- str_squish(str_remove(titles[, 1], paste0(" ", feeling[1], "$")))
  list(topic = set_names(topic, topics), feeling = set_names(feeling, feelings))
}

# shortens text to fit an axis, ending in an ellipsis, cut at a word boundary where possible
cap_text <- function(x, max_chars = 46) {
  vapply(as.character(x), function(s) {
    s <- trimws(gsub("\\s+", " ", s))
    if (is.na(s) || nchar(s) <= max_chars) return(s)
    cut <- substr(s, 1, max_chars - 1)
    space <- regexpr("[^ ]*$", cut)
    if (space > max_chars * 0.6) cut <- substr(cut, 1, space - 1)
    paste0(trimws(cut), "…")
  }, character(1), USE.NAMES = FALSE)
}


# ---- Selecting items -------------------------------------------------------------
# all columns of a block by prefix, in questionnaire order (beliefs_society_1, ..., beliefs_crime_4). A
# number must close the name, which keeps the _scored copies out; one letter may follow it (betrokkenheid_1b).
select_items <- function(prefix, all_names = names(df)) {
  cols <- grep(paste0("^", prefix, "(_[A-Za-z0-9]+)*_[0-9]+[a-z]?$"), all_names, value = TRUE)
  if (!length(cols)) return(cols)
  index <- strsplit(sub(paste0("^", prefix, "_"), "", cols), "_")
  depth <- max(lengths(index))
  padded <- matrix(vapply(index, \(i) c(i, rep("0", depth - length(i))), character(depth)), nrow = depth)
  # numeric segments sort numerically, named segments in order of first appearance (the export's order)
  ranks <- apply(padded, 1, function(segment) {
    n <- suppressWarnings(as.numeric(segment))
    if (all(!is.na(n))) sprintf("%04d", n) else sprintf("%04d", match(segment, unique(segment)))
  })
  cols[order(apply(matrix(ranks, ncol = depth), 1, paste, collapse = "-"))]
}

scored_copies <- function(items) if (!length(items)) character(0) else paste0(items, "_scored")


# ---- Tables ----------------------------------------------------------------------
# weighted counts of one column over its answer options (factor levels, or value labels), every option kept;
# attribute "n_valid" is the unweighted number of respondents who answered
tabulate_weighted <- function(col, data = df, levels = NULL) {
  x <- data[[col]]
  ok <- !is.na(x)
  if (is.factor(x)) {
    cats <- levels %||% levels(x)
    codes <- match(as.character(x), cats)
  } else {
    scale <- answer_scale(col, data)
    cats <- levels %||% scale$label
    codes <- match(as.numeric(x), scale$code)
  }
  out <- tibble(category = factor(cats, levels = cats), n = w_tabulate(codes, data$w, length(cats)))
  attr(out, "n_valid") <- sum(ok)
  out
}

# one frequency table for a block: a row per item, a weighted count per answer option. Sliders (no value
# labels beyond the end points) are binned into tens.
block_frequencies <- function(cols, data = df, bin_width = 10, digits = 1) {
  titles <- item_titles(cols, data)
  slider <- nrow(answer_scale(cols[1], data)) <= 2 && max(data[[cols[1]]], na.rm = TRUE) > 10
  if (!slider) {
    counts <- lapply(cols, \(cl) set_names(round(tabulate_weighted(cl, data)$n, digits),
                                           answer_scale(cl, data)$label))
    names_out <- answer_scale(cols[1], data)$label
  } else {
    breaks <- seq(0, 100, by = bin_width)
    names_out <- paste0(head(breaks, -1), "-", breaks[-1])
    counts <- lapply(cols, \(cl) {
      b <- cut(as.numeric(data[[cl]]), breaks = breaks, include.lowest = TRUE, labels = FALSE)
      round(w_tabulate(b, data$w, length(names_out)), digits)
    })
  }
  out <- data.frame(item = titles, do.call(rbind, counts), check.names = FALSE)
  names(out)[-1] <- names_out
  out$n <- vapply(cols, \(cl) sum(!is.na(data[[cl]])), integer(1))
  rownames(out) <- NULL
  out
}

# weighted mean and SD per item, with the unweighted n
block_descriptives <- function(cols, data = df, digits = 2) {
  tibble(item = item_titles(cols, data),
         n    = vapply(cols, \(cl) sum(!is.na(data[[cl]])), integer(1)),
         M    = round(vapply(cols, \(cl) w_mean(as.numeric(data[[cl]]), data$w), numeric(1)), digits),
         SD   = round(vapply(cols, \(cl) w_sd(as.numeric(data[[cl]]), data$w), numeric(1)), digits),
         min  = vapply(cols, \(cl) min(data[[cl]], na.rm = TRUE), numeric(1)),
         max  = vapply(cols, \(cl) max(data[[cl]], na.rm = TRUE), numeric(1)))
}


# ---- Charts ----------------------------------------------------------------------
# horizontal bars of a tabulate_weighted() table
plot_bar_horizontal <- function(count_table, title, n_valid = NULL) {
  ggplot(count_table, aes(x = n, y = fct_rev(category))) +
    geom_col(fill = "#3B6EA5", width = 0.7) +
    geom_text(aes(label = round(n)), hjust = -0.3, size = 3.3, colour = "grey20") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
    scale_y_discrete(labels = scales::label_wrap(45)) +
    labs(title = str_wrap(cap_text(title, 140), width = 70),
         subtitle = if (!is.null(n_valid)) paste0("n = ", n_valid, " respondents; bars are weighted counts"),
         x = "Respondents (weighted)", y = NULL) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 11),
          plot.subtitle = element_text(colour = "grey40", size = 9),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y = element_text(colour = "grey10"))
}

# UNWEIGHTED: one bar per respondent, fastest to slowest, in minutes; the axis stops at 45 minutes and longer
# interviews are drawn at the cap in orange. The dashed line is the weighted median.
plot_duration_bars <- function(seconds, w, title = "Completion time per respondent") {
  ok <- !is.na(seconds)
  minutes <- seconds[ok] / 60
  med <- w_quantile(minutes, w[ok], 0.5)
  o <- order(minutes)
  y_cap <- 45
  d <- tibble(respondent = seq_along(o), minutes = pmin(minutes[o], y_cap), past_cap = minutes[o] > y_cap)
  subtitle <- paste0("n = ", sum(ok), " respondents (unweighted: one bar each); weighted median ",
                     round(med, 1), " min")
  if (any(d$past_cap))
    subtitle <- paste0(subtitle, "; ", sum(d$past_cap), " in orange past the ", y_cap,
                       "-minute axis (longest ", round(max(minutes)), " min)")
  ggplot(d, aes(x = respondent, y = minutes, fill = past_cap)) +
    geom_col(width = 0.85) +
    scale_fill_manual(values = c(`FALSE` = "#3B6EA5", `TRUE` = "#E69F00"), guide = "none") +
    geom_hline(yintercept = med, linetype = "dashed", colour = "grey40") +
    coord_cartesian(ylim = c(0, y_cap)) +
    labs(title = title, subtitle = subtitle,
         x = "Respondent (each bar = one respondent, sorted fastest to slowest)",
         y = "Completion time (minutes)") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 11),
          plot.subtitle = element_text(colour = "grey40", size = 9),
          axis.text.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank())
}

# the largest phi two binary items with base rates p and q can reach
phi_max <- function(p, q) {
  lo <- pmin(p, q); hi <- pmax(p, q)
  sqrt((lo * (1 - hi)) / (hi * (1 - lo)))
}


# ---- The splits of the interactive charts -------------------------------------------
# one entry per split button: the groups and which group every respondent is in
build_splits <- function(data = df) {
  grouped <- function(label, column, mode = "grouped") {
    column <- if (is.factor(column)) column else haven::as_factor(column)
    column <- droplevels(column)
    list(label = label, mode = mode, groups = levels(column), assign = as.integer(column))
  }
  list(
    politics  = grouped("Stemherinnering (links / midden / rechts)", data$vote_3),
    gender    = grouped("Geslacht", data$sex_rec),
    age       = grouped("Leeftijd", data$age_group, mode = "stacked"),
    substance = grouped("Middelengebruik", data$substance_ladder),
    opleiding = grouped("Opleiding", fct_rev(data$edu_group)),     # practical -> theoretical
    stad      = grouped("Stedelijkheid", data$urb_group),
    mdma      = grouped("MDMA-regulering", data$mdma_support))
}


# ---- The chart viewers -------------------------------------------------------------
# one of the two viewers in browsers.js, to inline into the HTML
browser_js <- function(part) {
  js <- readLines(here::here("analysis", "reports", "descriptive_plots", "browsers.js"), warn = FALSE)
  starts <- c(grep("^// ==== ", js), length(js) + 1)
  i <- grep(paste0("^// ==== ", part, " "), js)
  js[(i + 1):(starts[match(i, starts) + 1] - 1)]
}
