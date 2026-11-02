# =============================================================================
# plot_helpers.R
# -----------------------------------------------------------------------------
# Generic helpers for the descriptive report: item titles, code -> label
# scales, counting, the slider anchors from the QSF, and item selection. No
# question wording or answer labels are typed in here; they come from the QSF
# via R/data.R.
# =============================================================================

library(readr)    # fast, predictable CSV reading
library(dplyr)    # data manipulation
library(tidyr)    # reshaping
library(stringr)  # tidy string handling (splitting, wrapping labels)
library(forcats)  # factor level handling
library(ggplot2)  # plotting


# -----------------------------------------------------------------------------
# item_title()
# -----------------------------------------------------------------------------
# Matrix questions store their wording as "Shared stem - Sub-item"
# (e.g. "In hoeverre ben je het eens...? - Regulering ..."). For a plot title
# we only want the sub-item, so we drop everything up to and including the
# first " - ". Questions without that separator are returned unchanged.
# -----------------------------------------------------------------------------
item_title <- function(question_text) {
  # str_squish() collapses embedded newlines (e.g. the slider instructions)
  # and repeated spaces to one space each; then drop everything up to and
  # including the first " - " so only the matrix sub-item (or the whole
  # question, if there is no separator) remains.
  str_squish(question_text) |> str_remove("^.*? - ")
}


# -----------------------------------------------------------------------------
# read_slider_anchors()
# -----------------------------------------------------------------------------
# Reads the labelled anchor points of a slider question from the Qualtrics
# survey definition (.qsf). These anchor texts (e.g. the 7 policy definitions
# from "verbod" to "vrije verkoop") are NOT in the data CSV, so they have to
# come from the survey file. Reading them here keeps them in sync: change the
# wording in Qualtrics, re-export the .qsf, and the plots follow.
#
# The anchors are assumed to be spaced evenly across the slider's numeric range
# (Qualtrics places labelled ticks at equal intervals), so their positions are
# seq(min, max, length.out = number_of_labels).
#
# Arguments:
#   qsf_path        : path to the .qsf survey-definition file
#   data_export_tag : the question's export tag (e.g. "beleid_voorkeur")
# Returns:
#   a data frame with columns `pos` (0-100 position) and `label`, or NULL if
#   the question or its labels are not found
# -----------------------------------------------------------------------------
read_slider_anchors <- function(qsf_path, data_export_tag) {
  qsf <- jsonlite::fromJSON(qsf_path, simplifyVector = FALSE)

  for (element in qsf$SurveyElements) {
    if (!identical(element$Element, "SQ")) next
    payload <- element$Payload
    if (!identical(payload$DataExportTag, data_export_tag)) next

    labels <- payload$Labels
    if (is.null(labels)) return(NULL)

    # keep only numeric-keyed anchors (drops the "NA" / no-opinion entry) and
    # order them by their numeric key so the labels run in slider order
    keys     <- names(labels)
    key_nums <- suppressWarnings(as.numeric(keys))
    keys     <- keys[!is.na(key_nums)]
    keys     <- keys[order(key_nums[!is.na(key_nums)])]

    texts <- vapply(keys, function(k) labels[[k]]$Display, character(1))

    slider_min <- as.numeric(payload$Configuration$CSSliderMin %||% 0)
    slider_max <- as.numeric(payload$Configuration$CSSliderMax %||% 100)

    return(data.frame(
      pos   = seq(slider_min, slider_max, length.out = length(texts)),
      label = texts,
      stringsAsFactors = FALSE
    ))
  }
  NULL
}


# scored_copies()
# -----------------------------------------------------------------------------
# The `_scored` names for a set of items - the reverse-scored copies the data
# layer builds beside every Likert item.
#
# WHY THIS IS NOT JUST paste0(). `paste0(character(0), "_scored")` is "_scored",
# a one-element vector naming a column that does not exist, because paste0
# treats a zero-length argument as "". An empty item block is not hypothetical:
# the pooled dataset has no behaviour items at all, since pilot 1's four were
# replaced by three new ones. The same mistake was made in three places and each
# time surfaced far from its cause - as "all(unlist(BLOCKS) %in% names(pilot))
# is not TRUE", as a dimnames length error, and as a missing model column.
# -----------------------------------------------------------------------------
scored_copies <- function(items)
  if (!length(items)) character(0) else paste0(items, "_scored")


# -----------------------------------------------------------------------------
# label_from_scale()
# -----------------------------------------------------------------------------
# One column of numeric answer codes, as an ordered factor carrying the answer
# options themselves. This is what the demographic questions are stored as: the
# survey asked "welke opleiding heb je afgerond", so the value of `opleiding`
# should read "hbo/wo-bachelor" and not 4.
#
# The report used to keep BOTH - the numeric column and a labelled copy under a
# second name (`age_band`, `opleiding_niveau`, `gender`) - which meant two
# columns for one question and a standing question of which one a given line
# meant. There is now one column per question, and it is this one.
#
# MATCHED ON THE CODE, not indexed by it. Qualtrics answer codes are not
# guaranteed to run 1..k: an option deleted from the middle of a question leaves
# its code behind, so `labels[code]` would silently shift every answer after the
# gap by one. A code with no matching option becomes NA rather than a wrong
# label.
#
# Arguments:
#   codes : numeric (or character) answer codes, one per respondent
#   scale : a code -> label data frame from qsf_scale()
#   drop  : leave out options nobody chose. FALSE keeps the full answer scale,
#           which is what a codebook and a bar chart want; TRUE is for a model
#           matrix, where an empty level is an all-zero column.
# -----------------------------------------------------------------------------
label_from_scale <- function(codes, scale, drop = FALSE) {
  out <- factor(match(as.character(codes), as.character(scale$code)),
                levels = seq_along(scale$label), labels = scale$label,
                ordered = TRUE)
  if (drop) droplevels(out) else out
}


# -----------------------------------------------------------------------------
# tabulate_question()
# -----------------------------------------------------------------------------
# Turns one question column (numeric codes from the VALUES export) into a tidy
# count table using a scale from qsf_scale(). Every scale point appears, even
# with a count of 0, so bars are comparable across items. Missing answers
# (blank cells) are excluded and reported via the `n_valid` attribute.
#
# Arguments:
#   values_df : the VALUES export (filtered to real responses)
#   column    : the column to tabulate
#   scale     : a code -> label data frame from qsf_scale()
#   multi     : TRUE for "check all that apply" columns
# Returns:
#   a tibble (category, n); attribute "n_valid" holds the number of respondents
# -----------------------------------------------------------------------------
tabulate_question <- function(values_df, column, scale, multi = FALSE) {
  raw <- values_df[[column]]
  # a numeric column already encodes a blank cell as NA; only the raw
  # character exports need the empty string translating
  if (is.character(raw)) raw[raw == ""] <- NA

  if (multi) {
    n_valid <- sum(!is.na(raw))
    codes   <- str_trim(unlist(str_split(raw[!is.na(raw)], ",")))
  } else {
    n_valid <- sum(!is.na(raw))
    codes   <- raw[!is.na(raw)]
  }

  # translate numeric codes to their ordered labels
  labelled <- factor(
    scale$label[match(codes, scale$code)],
    levels = scale$label
  )

  # .drop = FALSE keeps every scale point, even ones with a count of 0
  counts <- dplyr::count(tibble(category = labelled), category,
                         .drop = FALSE, name = "n")

  attr(counts, "n_valid") <- n_valid
  counts
}


# -----------------------------------------------------------------------------
# plot_bar_horizontal()
# -----------------------------------------------------------------------------
# Horizontal bar plot, kept for questions with long answer labels (background
# questions, and the "check all that apply" emotion question) where turning the
# labels vertical would be unreadable. Bars are ordered by the scale; the count
# is printed at the end of each bar.
#
# Arguments:
#   count_table : output of tabulate_question()
#   title       : plot title
#   n_valid     : number of respondents who answered
#   subtitle    : optional custom subtitle (used for multi-answer note)
# Returns:
#   a ggplot object
# -----------------------------------------------------------------------------
plot_bar_horizontal <- function(count_table, title, n_valid = NULL,
                                subtitle = NULL) {
  if (is.null(subtitle) && !is.null(n_valid)) {
    subtitle <- paste0("n = ", n_valid, " respondents")
  }

  ggplot(count_table, aes(x = n, y = fct_rev(category))) +
    geom_col(fill = "#3B6EA5", width = 0.7) +
    geom_text(aes(label = n), hjust = -0.3, size = 3.3, colour = "grey20") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
    # Answer options can be long - the consent item's option is a sentence - and
    # an unwrapped label either runs off the panel or squeezes the bars into
    # nothing. Wrapping keeps the whole text readable at the cost of height.
    scale_y_discrete(labels = scales::label_wrap(45)) +
    labs(
      # Capped BEFORE wrapping. Some question texts are not a sentence: the
      # consent item's wording is the whole consent form, and wrapping that at
      # 70 characters produced forty lines of title that pushed the bars off
      # the figure entirely. A title identifies the chart; the full wording
      # belongs in the report text, not on the plot.
      title = str_wrap(cap_text(title, 140), width = 70),
      subtitle = subtitle,
      x = "Number of respondents",
      y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(colour = "grey40", size = 9),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.y = element_text(colour = "grey10")
    )
}


# -----------------------------------------------------------------------------
# count_values()
# -----------------------------------------------------------------------------
# Counts a plain character column that is already its own label (e.g. the
# metadata columns UserLanguage), without needing a code -> label scale.
# Missing values are excluded and reported via the "n_valid" attribute.
#
# Arguments:
#   x              : a character vector to count
#   ordered_levels : optional display order; if NULL, ordered by frequency
# Returns:
#   a tibble (category, n); attribute "n_valid" holds the number answered
# -----------------------------------------------------------------------------
count_values <- function(x, ordered_levels = NULL) {
  if (is.character(x)) x[x == ""] <- NA
  n_valid <- sum(!is.na(x))
  x <- x[!is.na(x)]

  if (is.null(ordered_levels)) {
    counts <- dplyr::count(tibble(category = x), category, sort = TRUE, name = "n") |>
      dplyr::mutate(category = fct_inorder(category))
  } else {
    x <- factor(x, levels = ordered_levels)
    # .drop = FALSE keeps every level, even ones with a count of 0
    counts <- dplyr::count(tibble(category = x), category, .drop = FALSE, name = "n")
  }

  attr(counts, "n_valid") <- n_valid
  counts
}


# -----------------------------------------------------------------------------
# plot_duration_bars()
# -----------------------------------------------------------------------------
# One bar per respondent showing how long they took to complete the survey,
# sorted from fastest to slowest and expressed in minutes. This makes speeders
# (implausibly fast) and long-tail outliers (left the tab open) easy to spot.
#
# Completion times are very heavy-tailed: a few respondents leave the tab open
# for hours, which would flatten everyone else onto the axis. So the y-axis is
# capped at 45 minutes; respondents above it are drawn at the cap in orange and
# counted in the subtitle, keeping the bulk of the distribution readable while
# still flagging the extreme cases. A dashed line marks the median of ALL
# responses, capped ones included. Sorted respondent ids carry no meaning, so
# the x-axis is blank.
#
# Arguments:
#   durations_seconds : the "Duration (in seconds)" column (character/numeric)
#   title             : plot title
# Returns:
#   a ggplot object
# -----------------------------------------------------------------------------
plot_duration_bars <- function(durations_seconds,
                               title = "Completion time per respondent") {
  minutes <- suppressWarnings(as.numeric(durations_seconds)) / 60
  minutes <- sort(minutes[!is.na(minutes)])
  n   <- length(minutes)
  
  df <- data.frame(
    respondent = seq_len(n),
    minutes    = minutes
  )
  
  # Median of all valid responses
  med <- median(minutes)

  # THE AXIS STOPS AT 45 MINUTES; NOBODY IS DROPPED. One pilot-1 respondent left
  # the survey open for 89 hours, and on a linear axis that single bar flattens
  # the other 241 into invisible lines. coord_cartesian() clips the VIEW rather
  # than the data, so the median, the n and every bar are still computed from
  # all of it. Bars past the cap are drawn AT the cap in orange, so a bar that
  # hits the top reads as "longer than this" rather than as exactly 45 minutes,
  # and the subtitle counts them instead of letting them set the scale.
  y_cap <- 45
  over  <- sum(minutes > y_cap)
  df$past_cap <- df$minutes > y_cap
  df$minutes  <- pmin(df$minutes, y_cap)

  subtitle <- paste0("n = ", n, " respondents; median ", round(med, 1), " min")
  if (over > 0)
    subtitle <- paste0(subtitle, "; ", over, " in orange past the ", y_cap,
                       "-minute axis (longest ", round(max(minutes)), " min)")

  ggplot(df, aes(x = respondent, y = minutes, fill = past_cap)) +
    # Orange is the Okabe-Ito colour-blind-safe orange, against the report blue.
    geom_col(width = 0.85) +
    scale_fill_manual(values = c(`FALSE` = "#3B6EA5", `TRUE` = "#E69F00"),
                      guide = "none") +
    geom_hline(yintercept = med, linetype = "dashed", colour = "grey40") +
    coord_cartesian(ylim = c(0, y_cap)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Respondent (each bar = one respondent, sorted fastest to slowest)",
      y = "Completion time (minutes)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(colour = "grey40", size = 9),
      axis.text.x = element_blank(),          # sorted ids are not meaningful
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
}


# -----------------------------------------------------------------------------
# select_items()
# -----------------------------------------------------------------------------
# Selects all columns of a matrix block by their variable-name prefix and
# returns them in numeric order (beliefs_1, beliefs_2, ... not beliefs_1,
# beliefs_10). Matching on the variable name means adding or removing items in
# Qualtrics needs no code change here.
#
# Arguments:
#   prefix    : the shared variable-name stem (e.g. "beliefs")
#   all_names : the column names to search (e.g. names(values_clean))
# Returns:
#   a character vector of matching column names, ordered by their number
# -----------------------------------------------------------------------------
select_items <- function(prefix, all_names) {
  # Match "<prefix>", any number of intermediate segments, then the ITEM NUMBER:
  # `beliefs_1` in the first pilot, `beliefs_crim_1` in the second, where the
  # items were regrouped into four matrix questions and the blocks are named
  # after their subject.
  #
  # Ending on a number is what keeps the derived copies out. `pilot` also holds
  # `beliefs_1_scored` and `beliefs_crim_1_scored`, and a pattern that allowed
  # any trailing segment swept those in - every caller then asked for
  # `beliefs_1_scored_scored`, which exists nowhere.
  # [A-Za-z0-9] works in both the base and stringr regex engines.
  #
  # One lower-case letter may follow the number: the main study's soft-launch
  # export carries `betrokkenheid_1b` and `_2b` beside `_1`, `_2`, `_3`. Such a
  # segment is not numeric, so the ordering below falls back to the order of
  # first appearance, which is the export's.
  cols <- grep(paste0("^", prefix, "(_[A-Za-z0-9]+)*_[0-9]+[a-z]?$"), all_names,
               value = TRUE)
  if (!length(cols)) return(cols)

  # Order by every segment, not just the last one: sorting `beliefs_crim_1`,
  # `beliefs_heal_1`, ... on the trailing number alone would interleave the
  # blocks.
  index <- strsplit(sub(paste0("^", prefix, "_"), "", cols), "_")
  depth <- max(lengths(index))
  padded <- vapply(index, function(i) c(i, rep("0", depth - length(i))),
                   character(depth))
  padded <- matrix(padded, nrow = depth)          # one ROW per segment position

  # A numeric segment sorts numerically, so beliefs_10 still follows beliefs_2.
  # A NAMED segment has no order of its own, so it takes the order in which the
  # names first appear in `all_names` - the export's order, which is the order
  # the questionnaire asked them in. Sorting those alphabetically would give
  # crim, harm, heal, soci: the last block moves to second, silently renumbering
  # most of the questionnaire.
  # apply() over the ROWS returns one COLUMN per segment position, i.e. one row
  # per item - which is what the paste below needs. No transpose.
  ranks <- apply(padded, 1, function(segment) {
    n <- suppressWarnings(as.numeric(segment))
    if (all(!is.na(n))) sprintf("%04d", n)
    else sprintf("%04d", match(segment, unique(segment)))
  })
  key <- apply(matrix(ranks, ncol = depth), 1, paste, collapse = "-")
  cols[order(key)]
}


# -----------------------------------------------------------------------------
# numbered_item_title()
# -----------------------------------------------------------------------------
# Prefixes an item's wording with its number inside the block, taken from the
# variable name (beliefs_13 -> "13. ..."). The Involvement tab numbers its
# items the same way, so the two tabs and the results spreadsheet can be read
# side by side.
# -----------------------------------------------------------------------------
numbered_item_title <- function(variable, wording) {
  n <- sub("^.*_", "", variable)
  paste0(n, ". ", item_title(wording[[variable]]))
}


# -----------------------------------------------------------------------------
# cap_text()
# -----------------------------------------------------------------------------
# Shortens long item wording so it fits an axis, ending with a real ellipsis so
# it is obvious the text was cut rather than the item simply being terse. Where
# possible the cut falls on a word boundary, which reads better than slicing
# mid-word.
#
# Used for the correlation-matrix axes, where the alternative - item numbers
# plus a lookup table underneath - meant no cell could be read on its own.
# -----------------------------------------------------------------------------
cap_text <- function(x, max_chars = 46) {
  vapply(as.character(x), function(s) {
    s <- trimws(gsub("\\s+", " ", s))
    if (is.na(s) || nchar(s) <= max_chars) return(s)
    cut <- substr(s, 1, max_chars - 1)
    space <- regexpr("[^ ]*$", cut)                 # start of the trailing word
    if (space > max_chars * 0.6) cut <- substr(cut, 1, space - 1)
    paste0(trimws(cut), "…")
  }, character(1), USE.NAMES = FALSE)
}


# -----------------------------------------------------------------------------
# block_frequencies()
# -----------------------------------------------------------------------------
# One frequency table for a whole question block: a row per item, a column per
# response option, counts in the cells.
#
# Every descriptives block opens with one of these. The charts below them show
# shape and let you split by subgroup; the table is what you read an exact
# number off, and it is the fastest way to spot an option nobody used or an item
# far more skipped than its neighbours.
#
# Two kinds of question need two kinds of table, so the SCALE decides:
#   discrete   a scale with rows (Likert, multiple choice, the emotion ticks)
#              gives one column per option, in scale order, zeros kept
#   slider     a scale with no rows (0-100 sliders have no answer options at
#              all) is binned into tens instead - a raw frequency over 101
#              possible values would say nothing
#
# `multi = TRUE` for the emotion question, whose cells hold several codes.
block_frequencies <- function(values_df, columns, scale, multi = FALSE,
                              wording = NULL, bin_width = 10) {
  label_of <- function(column)
    if (is.null(wording) || is.na(wording[column])) column else item_title(wording[[column]])

  if (nrow(scale) > 0) {
    counts <- lapply(columns, function(column) {
      tab <- tabulate_question(values_df, column, scale, multi = multi)
      stats::setNames(as.integer(tab$n), as.character(tab$category))
    })
    out <- data.frame(item = vapply(columns, label_of, character(1)),
                      do.call(rbind, counts), check.names = FALSE)
    out$N <- vapply(columns, function(column)
      attr(tabulate_question(values_df, column, scale, multi = multi), "n_valid"),
      integer(1))
  } else {
    # slider: bin the 0-100 range, and keep every bin so gaps are visible
    breaks <- seq(0, 100, by = bin_width)
    bins   <- paste0(utils::head(breaks, -1), "-", breaks[-1])
    counts <- lapply(columns, function(column) {
      x <- suppressWarnings(as.numeric(values_df[[column]]))
      b <- cut(x, breaks = breaks, include.lowest = TRUE, labels = bins)
      as.integer(table(factor(b, levels = bins)))
    })
    out <- data.frame(item = vapply(columns, label_of, character(1)),
                      do.call(rbind, counts), check.names = FALSE)
    names(out)[-1] <- bins
    out$N <- vapply(columns, function(column)
      sum(!is.na(suppressWarnings(as.numeric(values_df[[column]])))), integer(1))
  }
  rownames(out) <- NULL
  out
}


# -----------------------------------------------------------------------------
# block_descriptives()
# -----------------------------------------------------------------------------
# Mean and standard deviation per item, the summary a paper reports for a rating
# scale. Used at the top of the belief and policy blocks, where a full frequency
# table is too wide to read: seven or eleven columns of counts say less about a
# 1-7 item than its centre and spread do.
#
# The blocks whose options are categories rather than degrees - substance use,
# the emotion ticks - keep their frequency table, because a mean over "nooit /
# niet afgelopen jaar / wel eens / vaak" would imply an interval scale the
# question does not have.
#
# Takes the TYPED dataset (`pilot`), not the raw export: the columns are already
# numeric there, and for the policy sliders they already carry their substance
# names.
block_descriptives <- function(data, columns, wording = NULL, digits = 2) {
  label_of <- function(column)
    if (is.null(wording) || is.na(wording[column])) column else item_title(wording[[column]])

  values <- lapply(columns, function(column) stats::na.omit(data[[column]]))
  data.frame(
    item = vapply(columns, label_of, character(1)),
    N    = vapply(values, length, integer(1)),
    M    = round(vapply(values, mean, numeric(1)), digits),
    SD   = round(vapply(values, stats::sd, numeric(1)), digits),
    min  = vapply(values, min, numeric(1)),
    max  = vapply(values, max, numeric(1)),
    row.names = NULL, check.names = FALSE)
}
