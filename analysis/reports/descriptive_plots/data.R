# =============================================================================
# data.R
# -----------------------------------------------------------------------------
# The data layer of descriptive_plots.Rmd: reads the Kieskompas delivery of the
# main study (22 October 2026, analysis/data/) and the questionnaire
# (analysis/assets/main_study_10_05_fixed.qsf), and builds `pilot` (one row per
# completed interview) plus the item lists, scales and labels the sections use.
#
# The sections were written for the pilot reports, hence the object name
# `pilot` and the pilots' variable names (beleid_<substance>, emo_<topic>_*).
#
# Code 99 is "geen mening" on the 1-7 matrices and the policy rules and is set
# to NA; on the 0-100 involvement sliders 99 is a real position.
# =============================================================================

library(here)

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

# =============================================================================
# The data
# =============================================================================

DELIVERY <- list(
  completed   = here("analysis", "data", "NLOZ26-10_Drugsbeleid_gedeeld_261022.csv"),
  in_progress = here("analysis", "data", "NLOZ26-10_Drugsbeleid_in progress_gedeeld_261022.csv"),
  sav         = here("analysis", "data", "NLOZ26-10_Drugsbeleid_gedeeld_261022.sav"),   # value labels
  label       = "full main study, delivered 22 October 2026")
QSF_PATH <- here("analysis", "assets", "main_study_10_05_fixed.qsf")

SCALE_MIN <- 1
SCALE_MAX <- 7


# ---- Constants ----------------------------------------------------------------
# export column -> substance key
POLICY_RAW <- c(beleid_voorkeur_alcohol = "alcohol", beleid_voorkeur_tabak_en_vapes = "nicotine",
                beleid_voorkeur_cannabis = "cannabis", beleid_voorkeur_psychedelica = "psychedelica",
                beleid_voorkeur_mdma = "mdma", beleid_voorkeur_cocaine = "cocaine",
                beleid_voorkeur_speed = "speed", beleid_voorkeur_heroine = "heroine",
                beleid_voorkeur_pijnstillers = "pijnstillers")
MDMA_SUPPORT_MIN <- 40   # the "MDMA-regulering" split, on the 0-100 slider

# Beliefs and behaviour. Anti items (agreeing favours prohibition) get a
# reverse-scored copy, <item>_scored, so that high always means liberal.
BELIEF_META <- data.frame(
  variable = c(paste0("beliefs_society_", 1:5), paste0("beliefs_addiction_", 1:5),
               paste0("beliefs_regulate_", 1:4), paste0("beliefs_crime_", 1:4)),
  wording = c(
    "Mensen mogen zelf beslissen of ze schadelijke middelen gebruiken.",
    "Drugsgebruik is slecht voor de normen en waarden van onze samenleving.",
    "Nederland moet zich houden aan het internationale verbod op drugs.",
    "Het is verkeerd om geld te verdienen aan middelen waar mensen verslaafd aan kunnen raken.",
    "We streven naar een samenleving waarin niemand verslavende middelen gebruikt.",
    "Als een middel legaal te koop is, raken meer mensen eraan verslaafd.",
    "Verslaafden zoeken eerder hulp als drugs niet verboden zijn.",
    "Mensen die verslaafd zijn hebben dat aan zichzelf te danken.",
    "Het gebruik van softdrugs leidt vaak tot het gebruik van harddrugs.",
    "Verboden harddrugs zijn ongezonder dan alcohol en tabak.",
    "Als een middel verboden is, gebruiken minder mensen het.",
    "Het reguleren van de verkoop van drugs levert Nederland veel geld op.",
    "Drugsgebruik is minder gevaarlijk als de verkoop wordt toegestaan.",
    "Een verbod werkt goed om jongeren te beschermen tegen de gevaren van verslavende middelen.",
    "Er zou in Nederland minder criminaliteit zijn als de verkoop van drugs wordt toegestaan.",
    "We moeten meer politie hebben om problemen met drugs aan te pakken.",
    "Drugs hebben voor eigen gebruik moet niet strafbaar zijn.",
    "Drugsgebruikers zijn medeverantwoordelijk voor drugscriminaliteit."),
  direction = c("Pro", "Anti", "Anti", "Anti", "Anti", "Anti", "Pro", "Anti", "Anti", "Anti",
                "Anti", "Pro", "Pro", "Anti", "Pro", "Anti", "Pro", "Anti"))
BELIEF_META$raw      <- BELIEF_META$variable
BELIEF_META$subscale <- sub("^beliefs_([a-z]+)_[0-9]+$", "\\1", BELIEF_META$variable)
GEDRAG_META <- data.frame(
  raw = paste0("gedrag_", 1:3), variable = paste0("gedrag_beleid_", 1:3),
  wording = c("Ik zou drugsdealers altijd melden bij de politie.", "Ik vermijd drugsgebruikers.",
              "Als iemand een drug heeft gebruikt die ik niet ken, zou ik ernaar vragen."),
  direction = c("Anti", "Anti", "Pro"))

# Own use: export 1-4 shifted to 0-3 (nooit, niet laatste jaar, wel eens, vaak).
USE_RAW <- c(gebruik_1 = "gebruik_alcohol", gebruik_2 = "gebruik_tabak",
             gebruik_3 = "gebruik_soft", gebruik_4 = "gebruik_hard")

# Emotions: gevoelens_1_<topic>_<answer> in the order of QSF question Q28.
EMOTION_TOPICS <- c("emo_alc", "emo_tab", "emo_soft", "emo_hard", "emo_wod", "emo_leg")
EMOTION_TOPIC_LABELS <- setNames(c("alcohol", "tabak", "softdrugs", "harddrugs", "war on drugs",
                                   "legalisering van drugs"), EMOTION_TOPICS)
EMOTION_ANSWERS <- c("boos", "bang_bezorgd", "blij", "geen_van_deze")
EMOTION_NONE    <- "geen_van_deze"
EMOTION_TYPES   <- setdiff(EMOTION_ANSWERS, EMOTION_NONE)
EMOTION_LABELS  <- c(boos = "boos", bang_bezorgd = "bang / bezorgd", blij = "blij",
                     geen_van_deze = "geen van deze")
EMOTION_GRID <- expand.grid(answer = seq_along(EMOTION_ANSWERS), topic = seq_along(EMOTION_TOPICS))
EMOTION_GRID$raw      <- sprintf("gevoelens_1_%d_%d", EMOTION_GRID$topic, EMOTION_GRID$answer)
EMOTION_GRID$variable <- paste0(EMOTION_TOPICS[EMOTION_GRID$topic], "_", EMOTION_ANSWERS[EMOTION_GRID$answer])

# Involvement: five 0-100 sliders. 1b and 2b are not in the QSF.
INVOLVEMENT_WORDING <- c(
  betrokkenheid_1  = "In mijn omgeving heb ik te maken met drugs of met criminaliteit door drugs.",
  betrokkenheid_1b = "betrokkenheid_1b",
  betrokkenheid_2  = "Over drugs of over drugscriminaliteit weet ik meer dan de meeste mensen.",
  betrokkenheid_2b = "betrokkenheid_2b",
  betrokkenheid_3  = "Hoe een partij omgaat met drugs en drugscriminaliteit, weegt mee bij mijn stemkeuze.")
INVOLVEMENT_RAW <- names(INVOLVEMENT_WORDING)

# demographics: the agency's recodes, labelled from the .sav
DEMOGRAPHICS <- c(age_7_rec = "leeftijd", sex_rec = "geslacht", edu_3_rec = "opleiding",
                  urb = "stedelijkheid", vote_recall_rec = "stemherinnering")


# ---- Helpers --------------------------------------------------------------------
# Questionnaire text is rich text: strip tags, decode entities, squash spaces.
qsf_text <- function(x) {
  x <- as.character(x)[1]
  if (is.na(x)) return(NA_character_)
  x <- gsub("<[^>]*>", " ", x)
  named <- c("&nbsp;" = " ", "&amp;" = "&", "&lt;" = "<", "&gt;" = ">", "&quot;" = "\"",
             "&#39;" = "'", "&apos;" = "'", "&euro;" = "€", "&eacute;" = "é", "&euml;" = "ë",
             "&iuml;" = "ï", "&ouml;" = "ö", "&uuml;" = "ü", "&hellip;" = "…", "&ndash;" = "–",
             "&mdash;" = "—", "&rsquo;" = "’", "&lsquo;" = "‘", "&ldquo;" = "“", "&rdquo;" = "”")
  for (entity in names(named)) x <- gsub(entity, named[[entity]], x, fixed = TRUE)
  for (entity in unique(regmatches(x, gregexpr("&#[0-9]+;", x))[[1]]))
    x <- gsub(entity, intToUtf8(as.integer(gsub("[^0-9]", "", entity))), x, fixed = TRUE)
  trimws(gsub(" +([,.;:!?])", "\\1", gsub("[[:space:]]+", " ", x)))
}

# every question in the QSF, keyed by export tag (entries without choices dropped)
qsf_questions <- function(path = QSF_PATH) {
  qs <- Filter(function(e) identical(e$Element, "SQ") && !is.null(e$Payload$DataExportTag) &&
                 (length(e$Payload$Choices) > 0 || length(e$Payload$Answers) > 0),
               jsonlite::fromJSON(path, simplifyVector = FALSE)$SurveyElements)
  setNames(lapply(qs, `[[`, "Payload"), vapply(qs, function(e) e$Payload$DataExportTag, ""))
}

# code -> label of one question: a matrix's Answers, a multiple choice's Choices
qsf_scale <- function(tag, questions = qsf_questions()) {
  p <- questions[[tag]]
  if (is.null(p)) stop("no question tagged '", tag, "' in the .qsf", call. = FALSE)
  options <- switch(p$QuestionType, Matrix = p$Answers, MC = p$Choices, NULL)
  if (!length(options)) return(data.frame(code = character(0), label = character(0)))
  data.frame(code = names(options), label = vapply(options, function(o) qsf_text(o$Display), ""),
             row.names = NULL)
}

# the sub-items of a matrix or slider, named by export column; the export numbers
# columns by position in ChoiceOrder, not by choice key
qsf_items <- function(tag, questions = qsf_questions()) {
  p <- questions[[tag]]
  if (is.null(p)) stop("no question tagged '", tag, "' in the .qsf", call. = FALSE)
  order <- if (length(p$ChoiceOrder)) as.character(unlist(p$ChoiceOrder)) else names(p$Choices)
  setNames(vapply(order, function(k) qsf_text(p$Choices[[k]]$Display), ""),
           paste0(tag, "_", seq_along(order)))
}

sav_value_labels <- function(variables)
  lapply(setNames(variables, variables), function(v) attr(haven::read_sav(DELIVERY$sav, n_max = 1)[[v]], "labels"))

# codes -> factor labelled from the .sav; a code without a label stops the build
label_codes <- function(x, labels) {
  present <- sort(unique(x[!is.na(x)]))
  if (length(missing <- setdiff(present, unname(labels))))
    stop("no label in the .sav for code(s) ", paste(missing, collapse = ", "), call. = FALSE)
  factor(x, levels = present, labels = names(labels)[match(present, unname(labels))])
}

# the emotion binaries as an item block, topic-major, without "geen van deze"
emotion_item_columns <- function(topics = EMOTION_TOPICS, emotions = EMOTION_TYPES)
  as.vector(t(outer(topics, emotions, paste, sep = "_")))
emotion_item_labels <- function(emotion_cols, wording, topics = EMOTION_TOPICS,
                                emotions = EMOTION_TYPES, sep = " - ") {
  stopifnot(length(emotion_cols) == length(topics))
  topic_text <- sub("^.*? - ", "", gsub("[\r\n]+", " ", unname(wording[emotion_cols])))
  as.vector(t(outer(topic_text, unname(EMOTION_LABELS[emotions]), paste, sep = sep)))
}
# the largest phi two binary items with base rates p and q can reach
phi_max <- function(p, q) {
  lo <- pmin(p, q); hi <- pmax(p, q)
  sqrt((lo * (1 - hi)) / (hi * (1 - lo)))
}


# ---- Read the delivery ------------------------------------------------------------
read_delivery <- function(f) read.csv(f, na.strings = c("", " ", "NA"), fileEncoding = "UTF-8-BOM",
                                      check.names = FALSE)
raw  <- read_delivery(DELIVERY$completed)
open <- read_delivery(DELIVERY$in_progress)
stopifnot(!any(open$InterviewId %in% raw$InterviewId))
open[setdiff(names(raw), names(open))] <- NA
raw <- rbind(cbind(raw, InterviewState = "Completed"), cbind(open[names(raw)], InterviewState = "InProgress"))

no_answer <- function(x) replace(x, x == 99, NA)
all_rows <- data.frame(ResponseId = raw$InterviewId, state = raw$InterviewState,
                       complete = raw$InterviewState == "Completed",
                       duration_sec = raw$InterviewDuration, duration_min = raw$InterviewDuration / 60)
for (v in names(POLICY_RAW)) all_rows[[paste0("beleid_", POLICY_RAW[[v]])]] <- no_answer(raw[[v]])
likert <- rbind(BELIEF_META[c("raw", "variable", "direction")], GEDRAG_META[c("raw", "variable", "direction")])
for (i in seq_len(nrow(likert))) all_rows[[likert$variable[i]]] <- no_answer(raw[[likert$raw[i]]])
for (i in seq_len(nrow(likert))) {
  x <- all_rows[[likert$variable[i]]]
  all_rows[[paste0(likert$variable[i], "_scored")]] <- if (likert$direction[i] == "Anti") (SCALE_MIN + SCALE_MAX) - x else x
}
for (v in names(USE_RAW)) all_rows[[USE_RAW[[v]]]] <- raw[[v]] - 1
for (i in seq_len(nrow(EMOTION_GRID))) all_rows[[EMOTION_GRID$variable[i]]] <- raw[[EMOTION_GRID$raw[i]]]
all_rows[INVOLVEMENT_RAW] <- raw[INVOLVEMENT_RAW]
all_rows$betrokkenheid_mean <- rowMeans(all_rows[INVOLVEMENT_RAW])

# substance ladder: the heaviest group a respondent has ever used
ever <- function(v) !is.na(all_rows[[v]]) & all_rows[[v]] >= 1
ladder <- ifelse(ever("gebruik_hard"), "hard drugs", ifelse(ever("gebruik_soft"), "cannabis",
          ifelse(ever("gebruik_alcohol") | ever("gebruik_tabak"), "legaal", "nooit")))
ladder[rowSums(!is.na(all_rows[unname(USE_RAW)])) == 0] <- NA
all_rows$substance_ladder <- factor(ladder, levels = c("nooit", "legaal", "cannabis", "hard drugs"))

# the sample: completed interviews; the in-progress ones are reported as dropped
status_scale <- data.frame(code = c("Completed", "InProgress"),
                           label = c("Completed", "InProgress (niet afgerond)"))
data_values <- data.frame(Status = all_rows$state)
blank_is_na <- function(x) { if (is.character(x)) x[x == ""] <- NA; is.na(x) }
n_genuine <- nrow(all_rows)


# ---- Emotions: the multi-select cell, rebuilt from the 0/1 boxes ---------------
questions <- qsf_questions(QSF_PATH)
emotion_scale <- qsf_scale("Q28", questions)
stopifnot(identical(gsub("[^a-z]+", "_", tolower(emotion_scale$label)), EMOTION_ANSWERS))
for (topic in EMOTION_TOPICS) {
  ticks <- as.matrix(all_rows[paste0(topic, "_", EMOTION_ANSWERS)])
  all_rows[[topic]] <- apply(ticks, 1, function(r)
    if (all(is.na(r))) NA_character_ else paste(emotion_scale$code[which(r == 1)], collapse = ","))
}
emotion_cols <- EMOTION_TOPICS


# ---- Policy: rule k at its position on the QSF's 0-100 slider -------------------
POLICY_COLUMNS <- setNames(paste0("beleid_", POLICY_RAW), POLICY_RAW)
policy_anchors <- read_slider_anchors(QSF_PATH, "beleid_voorkeur")
stopifnot(nrow(policy_anchors) == 9)
for (v in POLICY_COLUMNS) all_rows[[v]] <- policy_anchors$pos[all_rows[[v]]]


# ---- Demographics, labelled from the .sav ------------------------------------------
age_scale <- qsf_scale("leeftijd", questions)
demo_labels <- sav_value_labels(names(DEMOGRAPHICS))
# the agency's age_7_rec has the QSF's seven bands
stopifnot(identical(unname(demo_labels$age_7_rec), as.numeric(age_scale$code)),
          identical(gsub("[^0-9+]", "", names(demo_labels$age_7_rec)),
                    gsub("[^0-9+]", "", sub(" jaar of ouder", "+", age_scale$label))))
all_rows$leeftijd <- label_from_scale(raw$age_7_rec, age_scale)
for (v in names(DEMOGRAPHICS)[-1]) all_rows[[DEMOGRAPHICS[[v]]]] <- label_codes(raw[[v]], demo_labels[[v]])
demo_cols <- unname(DEMOGRAPHICS)


# ---- Wording and item metadata -----------------------------------------------------
belief_cols <- BELIEF_META$variable
gedrag_cols <- GEDRAG_META$variable
wording <- c(
  setNames(BELIEF_META$wording, belief_cols),
  setNames(GEDRAG_META$wording, gedrag_cols),
  setNames(qsf_items("beleid_voorkeur", questions), POLICY_COLUMNS),
  setNames(qsf_items("gebruik_ooit", questions), unname(USE_RAW)),
  INVOLVEMENT_WORDING,
  setNames(qsf_items("Q28", questions), EMOTION_TOPICS),
  leeftijd        = "Wat is je leeftijd? (age_7_rec)",
  geslacht        = "Geslacht (sex_rec)",
  opleiding       = "Opleiding (edu_3_rec)",
  stedelijkheid   = "Stedelijkheid (urb)",
  stemherinnering = "Stemherinnering (vote_recall_rec)")
stopifnot(identical(unname(wording[belief_cols]),
                    unname(unlist(lapply(c("beliefs_society", "beliefs_addiction", "beliefs_regulate",
                                           "beliefs_crime"), qsf_items, questions = questions)))))

# the shared answer scale of a set of columns, from the QSF
question_scale <- function(columns, multi = FALSE) {
  tags <- unique(sub("_[0-9]+[a-z]?$", "", columns))
  tags <- unique(ifelse(grepl("^gebruik_", tags), "gebruik_ooit", tags))
  scales <- lapply(tags, qsf_scale, questions = questions)
  stopifnot(all(vapply(scales, identical, logical(1), scales[[1]])))
  scales[[1]]
}

meta <- rbind(data.frame(variable = belief_cols, block = "beliefs", category = BELIEF_META$subscale,
                         direction = BELIEF_META$direction),
              data.frame(variable = gedrag_cols, block = "gedrag_beleid", category = "gedrag",
                         direction = GEDRAG_META$direction))
item_meta <- data.frame(variable = meta$variable, block = meta$block,
                        item_text_nl = unname(wording[meta$variable]), category = meta$category,
                        direction_itempool = meta$direction, direction_used = meta$direction,
                        reverse_coded = ifelse(meta$direction == "Anti", "yes", "no"), note = "",
                        row.names = meta$variable)


# ---- Column blocks and `pilot` -------------------------------------------------------
likert_cols  <- item_meta$variable
slider_cols  <- unname(POLICY_COLUMNS)
invol_cols   <- select_items("betrokkenheid", names(all_rows))
stopifnot(identical(invol_cols, INVOLVEMENT_RAW))
USE_COLUMNS  <- setNames(unname(USE_RAW), c("alcohol", "tabak", "soft", "hard"))
subst_cols   <- unname(USE_COLUMNS)
reverse_cols <- item_meta$variable[item_meta$reverse_coded == "yes"]
REQUIRED_COLS <- c(likert_cols, emotion_cols, invol_cols)
substance_scale <- question_scale(subst_cols)
substance_scale$code <- as.character(as.numeric(substance_scale$code) - 1)

binaries <- as.vector(t(outer(EMOTION_TOPICS, EMOTION_ANSWERS, paste, sep = "_")))
pilot <- all_rows[all_rows$complete, c("ResponseId", "duration_sec", "duration_min", demo_cols,
                                       likert_cols, paste0(likert_cols, "_scored"), slider_cols,
                                       invol_cols, "betrokkenheid_mean", subst_cols, emotion_cols,
                                       binaries, "substance_ladder")]
rownames(pilot) <- NULL
dropped <- all_rows[!all_rows$complete, ]
emotions_binary <- pilot[c("ResponseId", binaries)]
n_pilot <- nrow(pilot)
ANALYSIS_COLUMNS <- names(pilot)


# ---- The splits the descriptive charts offer -------------------------------------------
build_demographics <- function(pilot) {
  grouped <- function(label, column, mode = "grouped")
    list(label = label, mode = mode, groups = levels(column), assign = as.integer(column))
  list(
    politics  = grouped("Stemherinnering (vote_recall_rec)", pilot$stemherinnering),
    gender    = grouped("Geslacht (sex_rec)", pilot$geslacht),
    age       = grouped("Leeftijd", pilot$leeftijd, mode = "stacked"),
    substance = grouped("Middelengebruik", pilot$substance_ladder),
    opleiding = grouped("Opleiding (edu_3_rec)", pilot$opleiding, mode = "stacked"),
    stad      = grouped("Stedelijkheid (urb)", pilot$stedelijkheid),
    mdma      = grouped("MDMA-regulering",
                        factor(ifelse(pilot$beleid_mdma >= MDMA_SUPPORT_MIN, 2L, 1L),
                               levels = 1:2, labels = c("tegen", "voor"))))
}


# one of the two chart viewers in browsers.js, to inline into the HTML
browser_js <- function(part) {
  js <- readLines(here("analysis", "reports", "descriptive_plots", "browsers.js"), warn = FALSE)
  starts <- c(grep("^// ==== ", js), length(js) + 1)
  i <- grep(paste0("^// ==== ", part, " "), js)
  js[(i + 1):(starts[match(i, starts) + 1] - 1)]
}
