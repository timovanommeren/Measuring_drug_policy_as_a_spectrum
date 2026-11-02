# =============================================================================
# descriptives_cube.R
# -----------------------------------------------------------------------------
# The counts cube behind the interactive descriptive charts: for every survey
# item, how often each response category was chosen, overall and within every
# demographic, substance and emotion split.
#
# It exists as a precomputed cube because the charts are drawn in the browser
# (descriptives_browser.js). Sending counts rather than raw responses is what
# makes the split buttons instant, and keeps respondent-level data out of the
# HTML.
#
# Produces `desc_data`, a nested list serialised to JSON by
# sections/descriptives.Rmd. Needs `pilot`, `emotions_binary`, the column
# blocks and build_demographics() from R/data.R.
# =============================================================================

# Counts per response category, overall and per demographic group, for every
# item question. The charts are drawn from this in the browser (see
# descriptives_browser.js), which is what makes the split buttons instant.

# Demographic/substance/emotion split definitions this whole cube is built
# against. build_demographics() only attaches the display metadata (label,
# grouped vs stacked) to grouping columns `pilot` already carries.
dem <- build_demographics(pilot)

# jsonlite would emit a JSON *object* (not *array*) for a named vector, so
# strip the names the emotion vectors carry from the cleaning step.
desc_emotion_topics <- unname(EMOTION_TOPICS)

# ONLY THE OPTIONS EVERY TOPIC WAS ASKED WITH. This block draws one row per
# topic against one shared list of categories, so a category has to exist for
# all of them. In the pooled dataset it does not: pilot 1 offered `hoop` and
# pilot 2 did not, and pilot 2 added two topics pilot 1 never asked, so
# `emo_wod_hoop` is a combination NEITHER instrument put to anyone.
#
# Dropping it here costs nothing that is not shown elsewhere: `hoop` still
# appears in the frequency table and in the raw-combination charts further up
# the section, both of which are built per topic and can be ragged.
desc_emotion_names <- Filter(
  function(emotion) all(paste0(desc_emotion_topics, "_", emotion) %in%
                        names(emotions_binary)),
  c(EMOTION_TYPES, EMOTION_NONE))
stopifnot(length(desc_emotion_names) > 0)

# The category labels are derived from the column suffixes rather than taken
# from `emotion_scale$label`. Those two were assumed to be parallel, and in the
# pooled dataset they are not: its answer scale is the UNION of the two
# instruments (so that a pilot-1 respondent's `hoop` still decodes) while its
# binaries are only the options both pilots offered. Reading the labels off the
# same vector that names the columns cannot go out of step with them.
desc_emotion_labels <- unname(EMOTION_LABELS[desc_emotion_names])
stopifnot(!anyNA(desc_emotion_labels))

# Which respondents belong to group `g` of a split. Most splits assign
# everyone to exactly one group and carry an `assign` vector; the raw emotion
# splits carry a `membership` matrix instead, because their groups OVERLAP -- a
# respondent who ticked two emotions belongs to two of them.
split_rows <- function(spec, g) {
  if (!is.null(spec$membership)) return(spec$membership[, g])
  !is.na(spec$assign) & spec$assign == g
}
split_sizes <- function(spec)
  vapply(seq_along(spec$groups), function(g) sum(split_rows(spec, g)), numeric(1))

# Counts how often each response category occurs, overall and within every
# demographic group. `codes` is an integer vector already mapped onto 1..k.
counts_by_group <- function(codes, k, demographics) {
  out <- list(all = as.integer(tabulate(codes, nbins = k)))
  for (d in names(demographics)) {
    spec <- demographics[[d]]
    out[[d]] <- lapply(seq_along(spec$groups), function(g)
      as.integer(tabulate(codes[split_rows(spec, g)], nbins = k)))
  }
  out
}

# Smoothed distribution of the underlying continuous slider values, overall
# and per demographic group, so the bars can be drawn over a continuous
# curve. The curve is scaled to counts (density x n x binwidth) so it is
# directly comparable with the bar heights.
density_by_group <- function(values, demographics, from = 0, to = 100,
                             binwidth = 100 / 7, n_points = 96) {
  curve <- function(x) {
    x <- x[!is.na(x)]
    # A group of one or two people has no density worth drawing. Returning NULL
    # here would be natural in R, but jsonlite renders an empty list as `{}`,
    # which is TRUTHY in JavaScript - the browser then sails past its "no curve"
    # guard and fails on the missing coordinates. Returning empty coordinate
    # vectors instead keeps the shape of the object intact, so the browser draws
    # a path with no points and simply shows nothing.
    if (length(x) < 3)
      return(list(x = numeric(0), y = numeric(0)))
    d <- stats::density(x, from = from, to = to, n = n_points)
    list(x = round(d$x, 1), y = round(d$y * length(x) * binwidth, 2))
  }
  out <- list(all = curve(values))
  for (d in names(demographics)) {
    spec <- demographics[[d]]
    out[[d]] <- lapply(seq_along(spec$groups), function(g)
      curve(values[split_rows(spec, g)]))
  }
  out
}

desc_blocks <- list()

# ---- ordered Likert blocks ------------------------------------------------
for (spec in list(list("beliefs", "Opvattingen (beliefs)"),
                  list("gedrag_beleid", "Gedrag"))) {
  cols  <- select_items(spec[[1]], names(pilot))
  # A block the dataset does not have produces no chart rather than an error.
  # The pooled dataset has no `gedrag` block at all - pilot 1's four behaviour
  # items were replaced by three new ones, so none of them is common to both
  # pilots - and question_scale() on an empty column set fails deep inside,
  # with "subscript out of bounds", which says nothing about the cause.
  if (!length(cols)) next
  scale <- question_scale(cols)
  items <- lapply(cols, function(cl) {
    codes <- match(pilot[[cl]], as.numeric(scale$code))
    # mean / SD on the scale points, so the browser can offer the same
    # "order by SD / by mean" toggle as before
    cnt <- counts_by_group(codes, nrow(scale), dem)
    list(title = numbered_item_title(cl, wording),
         mean = round(mean(codes, na.rm = TRUE), 2),
         sd   = round(stats::sd(codes, na.rm = TRUE), 2),
         counts = cnt)
  })
  desc_blocks[[length(desc_blocks) + 1]] <- list(
    id = spec[[1]], title = spec[[2]], categories = scale$label,
    orderable = TRUE, items = items)
}

# ---- substance use: an explicit "skipped" category -------------------------
# This question is the only one without ForceResponse, so a blank is a genuine
# refusal. Giving it its own category keeps it visible instead of vanishing.
cols  <- subst_cols
scale <- substance_scale
cats  <- c(scale$label, "overgeslagen")
# Numbered by POSITION rather than by numbered_item_title(), which reads the
# number off the end of the column name: these columns are named after their
# substance now, so there is no number in the name to read. The order is the
# survey's, so the numbers are the same ones the other blocks show.
items <- lapply(seq_along(cols), function(i) {
  cl    <- cols[i]
  codes <- match(pilot[[cl]], as.numeric(scale$code))
  codes[is.na(codes)] <- nrow(scale) + 1L          # blank -> the skipped bin
  valid <- codes[codes <= nrow(scale)]        # exclude "skipped" from mean/SD
  cnt <- counts_by_group(codes, length(cats), dem)
  list(title = paste0(i, ". ", item_title(wording[[cl]])),
       mean = round(mean(valid, na.rm = TRUE), 2),
       sd   = round(stats::sd(valid, na.rm = TRUE), 2),
       counts = cnt)
})
desc_blocks[[length(desc_blocks) + 1]] <- list(
  id = "gebruik", title = "Middelengebruik", categories = cats,
  orderable = TRUE, items = items)

# ---- policy sliders: 7 policy bins + "Geen mening" + density ---------------
anchors <- read_slider_anchors(QSF_PATH, "beleid_voorkeur")
cols    <- unname(POLICY_COLUMNS)
k_pol   <- nrow(anchors)
# bin edges sit half-way between the labelled anchor positions, so every bin
# is centred on one policy option
edges <- c(-Inf, (anchors$pos[-k_pol] + anchors$pos[-1]) / 2, Inf)
items <- lapply(cols, function(cl) {
  val   <- pilot[[cl]]
  codes <- as.integer(cut(val, breaks = edges, labels = FALSE))
  codes[is.na(val)] <- k_pol + 1L                  # blank = "Geen mening"
  cnt <- counts_by_group(codes, k_pol + 1L, dem)
  list(title = item_title(wording[[cl]]),
       counts  = cnt,
       # one bin is the distance between two rules: 12.5 points for the
       # soft launch's nine, where the pilots' seven took 100 / 7
       density = density_by_group(val, dem, binwidth = diff(anchors$pos)[1]))
})
desc_blocks[[length(desc_blocks) + 1]] <- list(
  id = "beleid_voorkeur", title = "Beleidsvoorkeur",
  categories = c(anchors$label, "Geen mening"),
  anchorPos = anchors$pos, hasDensity = TRUE, items = items)

# ---- involvement sliders: equal-width bins + density -----------------------
cols <- select_items("betrokkenheid", names(pilot))
nb   <- 5
ed   <- seq(0, 100, length.out = nb + 1)
items <- lapply(cols, function(cl) {
  val   <- pilot[[cl]]
  codes <- pmin(floor(val / (100 / nb)) + 1L, nb)
  cnt <- counts_by_group(codes, nb, dem)
  list(title = item_title(wording[[cl]]),
       counts  = cnt,
       density = density_by_group(val, dem, binwidth = 100 / nb))
})
desc_blocks[[length(desc_blocks) + 1]] <- list(
  id = "betrokkenheid", title = "Betrokkenheid",
  categories = paste0(ed[-length(ed)], "–", ed[-1]),
  anchorPos = (ed[-length(ed)] + ed[-1]) / 2, hasDensity = TRUE, items = items)

# ---- emotions: share endorsing each emotion, per topic ---------------------
# The topic and emotion lists are passed in rather than parsed back out of the
# column names, because several emotion labels themselves contain underscores
# ("bang_bezorgd", "geen_van_deze") and could not be split off reliably.
items <- lapply(desc_emotion_topics, function(tp) {
  cls <- paste0(tp, "_", desc_emotion_names)
  # counts here are "number endorsing"; each emotion is its own binary item
  cnt <- list(all = as.integer(colSums(emotions_binary[cls], na.rm = TRUE)))
  for (d in names(dem)) {
    a <- dem[[d]]$assign
    cnt[[d]] <- lapply(seq_along(dem[[d]]$groups), function(g)
      as.integer(colSums(emotions_binary[!is.na(a) & a == g, cls], na.rm = TRUE)))
  }
  # the survey's own name for the topic, not the column stem it is stored under
  list(title = unname(EMOTION_TOPIC_LABELS[tp]), counts = cnt)
})
desc_blocks[[length(desc_blocks) + 1]] <- list(
  id = "emotions", title = "Emoties", categories = desc_emotion_labels,
  multi = TRUE, items = items)

# ---- group sizes and how many respondents fall outside each split ---------
dem_out <- lapply(names(dem), function(d) {
  spec <- dem[[d]]
  # `overlapping` tells the browser that the groups are not a partition of the
  # sample, so it must show counts: percentages of overlapping groups invite
  # being added up, and they do not add up to anything.
  list(label = spec$label, mode = spec$mode, groups = spec$groups,
       n = as.integer(split_sizes(spec)),
       overlapping = !is.null(spec$membership),
       excluded = as.integer(if (is.null(spec$membership))
                               sum(is.na(spec$assign))
                             else sum(rowSums(spec$membership) == 0)))
})
names(dem_out) <- names(dem)

desc_data <- list(N = nrow(pilot), demographics = dem_out, blocks = desc_blocks)
