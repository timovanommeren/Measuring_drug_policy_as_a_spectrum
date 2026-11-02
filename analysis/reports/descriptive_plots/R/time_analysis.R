# =============================================================================
# time_analysis.R
# -----------------------------------------------------------------------------
# Response-pattern measures for the completion-time tab.
#
# THE QUESTION is methodological: completion times differ by a factor of four,
# and what that says about how the survey works. Not who answered what.
#
# WHAT IS NOT AVAILABLE. The export carries `Duration (in seconds)` and nothing
# else - no per-page timings, and the survey definition contains no Qualtrics
# Timing questions. So "which page is slow?" cannot be answered from this data
# at all, and every measure below is an inference from total duration crossed
# with what people answered. Adding a Timing question per page is the single
# change that would make the next survey answerable directly.
#
# NOTHING IS CAPPED. A respondent who took half an hour took half an hour; that
# is the phenomenon under study, not an outlier to be trimmed. Where the spread
# makes a plot unreadable the AXIS is logged, which compresses without dropping
# anyone.
#
# DEPENDENCIES: `pilot` and the column blocks from the data layer.
# =============================================================================


# -----------------------------------------------------------------------------
# response_patterns()
# -----------------------------------------------------------------------------
# One row per respondent, one column per careless-responding mechanism. Each is
# a count or a proportion, so a reader can check any of them by hand.
#
#   sd_likert        within-respondent SD across the Likert items. The standard
#                    non-differentiation measure: 0 means every item got the
#                    same answer.
#   longest_run      longest streak of identical consecutive answers. Catches
#                    the respondent who straightlines a block and then wakes up,
#                    which the SD averages away.
#   n_options_used   how many of the scale's points the respondent used at all.
#   extreme_share    share of Likert answers at either end of the scale.
#   midpoint_share   share at the exact middle.
#   geen_mening      policy sliders left blank, i.e. answered "Geen mening".
#   slider_default   sliders left at 50, the position they start at - a slider
#                    that was never touched looks the same as one deliberately
#                    set to the middle, and both are worth counting.
#   emotions_ticked  boxes ticked across the emotion question, the one item
#                    whose length the respondent controls.
# -----------------------------------------------------------------------------
response_patterns <- function(pilot, likert_cols, slider_cols, scale_min,
                              scale_max) {
  likert <- as.matrix(pilot[likert_cols])
  slider <- as.matrix(pilot[slider_cols])
  emotion_cols <- grep("_(boos|bang_bezorgd|hoop|blij|geen_van_deze)$",
                       names(pilot), value = TRUE)

  midpoint <- (scale_min + scale_max) / 2
  run_length <- function(v) {
    v <- v[!is.na(v)]
    if (!length(v)) return(NA_integer_)
    max(rle(v)$lengths)
  }

  data.frame(
    sd_likert       = apply(likert, 1, stats::sd, na.rm = TRUE),
    longest_run     = apply(likert, 1, run_length),
    n_options_used  = apply(likert, 1, function(v) length(unique(stats::na.omit(v)))),
    extreme_share   = rowMeans(likert == scale_min | likert == scale_max, na.rm = TRUE),
    midpoint_share  = rowMeans(likert == midpoint, na.rm = TRUE),
    geen_mening     = rowSums(is.na(slider)),
    slider_default  = rowSums(slider == 50, na.rm = TRUE),
    emotions_ticked = if (length(emotion_cols))
      rowSums(as.matrix(pilot[emotion_cols]) == 1, na.rm = TRUE) else NA_integer_
  )
}


# -----------------------------------------------------------------------------
# correlate_with_duration()
# -----------------------------------------------------------------------------
# Each measure against log duration, with a confidence interval.
#
# LOGGED because duration is right-skewed and Spearman is used anyway - the log
# matters for the plots, and keeping one definition of "duration" across the tab
# avoids two numbers that differ for no visible reason.
#
# The interval is the point of the table. A correlation near zero says nothing
# on its own; near zero with a tight interval says the effect is absent, and
# near zero with a wide one says the sample cannot tell. Fisher's z, which is
# what cor.test() reports for Pearson, is used on the ranks.
# -----------------------------------------------------------------------------
correlate_with_duration <- function(patterns, duration) {
  log_duration <- log(duration)
  do.call(rbind, lapply(names(patterns), function(measure) {
    x  <- patterns[[measure]]
    ok <- is.finite(x) & is.finite(log_duration)
    if (sum(ok) < 4 || stats::sd(x[ok]) == 0)
      return(data.frame(maat = measure, r = NA_real_, laag = NA_real_,
                        hoog = NA_real_, p = NA_real_, n = sum(ok)))
    # Spearman as Pearson on the ranks, so cor.test() supplies the interval
    test <- stats::cor.test(rank(x[ok]), rank(log_duration[ok]))
    data.frame(maat = measure,
               r    = round(unname(test$estimate), 2),
               laag = round(test$conf.int[1], 2),
               hoog = round(test$conf.int[2], 2),
               p    = round(test$p.value, 3),
               n    = sum(ok))
  }))
}


# -----------------------------------------------------------------------------
# quality_by_speed()
# -----------------------------------------------------------------------------
# Does answering quickly cost data quality? The correlations above cannot say:
# a measure can be uncorrelated with duration and the fast group still produce a
# less coherent scale.
#
# Splits respondents into equal-sized speed groups and reports, per group, the
# reliability of the item set and the mean corrected item-total correlation.
# Group sizes are returned alongside, because with n of this order a difference
# between two groups of thirty is not much of a difference.
# -----------------------------------------------------------------------------
quality_by_speed <- function(items, duration, groups = 3,
                             labels = c("snelst", "midden", "traagst")) {
  cuts  <- stats::quantile(duration, seq(0, 1, length.out = groups + 1), na.rm = TRUE)
  speed <- cut(duration, cuts, include.lowest = TRUE, labels = labels)

  do.call(rbind, lapply(labels, function(g) {
    block <- items[!is.na(speed) & speed == g, , drop = FALSE]
    fit   <- suppressWarnings(psych::alpha(block, warnings = FALSE))
    data.frame(groep = g,
               n     = nrow(block),
               `mediane duur` = round(stats::median(duration[!is.na(speed) &
                                                             speed == g]), 1),
               alpha = round(fit$total$raw_alpha, 2),
               `gem. item-rest` = round(mean(fit$item.stats$r.drop), 2),
               check.names = FALSE)
  }))
}


# -----------------------------------------------------------------------------
# duration_by_group()
# -----------------------------------------------------------------------------
# Median duration per level of a grouping, WITH the cell sizes.
#
# The sizes are not decoration. `UserLanguage` in this pilot looks like a strong
# effect - 11.2 minutes against 7.8 - on a single Spanish-language respondent.
# A table of medians without an n column manufactures findings like that one.
# -----------------------------------------------------------------------------
duration_by_group <- function(duration, group, label) {
  ok <- !is.na(group) & !is.na(duration)
  group <- droplevels(as.factor(group[ok]))
  data.frame(
    kenmerk  = label,
    groep    = levels(group),
    n        = as.integer(table(group)),
    mediaan  = round(as.numeric(tapply(duration[ok], group, stats::median)), 1),
    IQR      = round(as.numeric(tapply(duration[ok], group, stats::IQR)), 1),
    row.names = NULL, check.names = FALSE)
}
