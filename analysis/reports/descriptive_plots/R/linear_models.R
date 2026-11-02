# =============================================================================
# linear_models.R
# -----------------------------------------------------------------------------
# Which variables predict policy preference, asked at five levels of aggregation
# over the policy sliders, plus a demographic model to compare those five to.
#
# Every model is a linear model whose predictors are chosen by AIC stepwise
# selection in both directions: it starts from the full candidate set, and at
# each step may drop a term or take back one it dropped earlier.
# Everything is resolved by name from the vectors R/data.R exports, and every
# model is refitted on each knit.
# =============================================================================

library(here)


# Sourcing defines; sections/linear_models.Rmd fits and prints the models.


# ---- The policy outcomes -----------------------------------------------------
# Five row means over the `beleid_*` sliders. A MEAN rather than a sum, which is
# what the scratch code took: the sliders all run 0-100, so a mean stays on that
# scale and the five outcomes are directly comparable to each other, while a sum
# of two is on a different scale from a sum of six.
#
# The last two are UNIONS of the first three rather than lists retyped, so
# `illegaal` cannot drift out of agreement with its parts.
#
# PIJNSTILLERS IS IN NONE OF THEM. Heavy painkillers are legal on prescription,
# so putting them in `harddrugs` would make `illegaal` untrue to its name, and
# the item cannot separate prescribed use from the rest anyway, so
# `beleid_pijnstillers` is in no model.
#
# Psychedelica counts as a soft drug here, grouped with cannabis: these sets are
# about which policies get averaged together.
POLICY_OUTCOME_SETS <- local({
  legaal    <- LEGAL_SUBSTANCES                        # alcohol, nicotine
  softdrugs <- c("cannabis", "psychedelica")
  harddrugs <- c("mdma", "cocaine", "speed", "heroine")
  list(legaal    = legaal,
       softdrugs = softdrugs,
       harddrugs = harddrugs,
       illegaal  = c(softdrugs, harddrugs),
       alle      = c(softdrugs, harddrugs, legaal))
})

policy_outcome <- function(data, substances, policy_columns = POLICY_COLUMNS) {
  columns <- policy_columns[substances]
  stopifnot(!anyNA(columns), all(columns %in% names(data)))
  # na.rm is deliberately absent: averaging over whichever sliders a respondent
  # happened to answer would give a different quantity per respondent. A
  # respondent who skipped one slider gets NA here and leaves the analysis frame.
  rowMeans(data[unname(columns)])
}


# ---- The candidate predictors ------------------------------------------------
# Beliefs, behaviour, own use and emotions - the pool the scratch version of this
# file already assembled, resolved by name so that adding an item to a block adds
# it here too.
#
# Beliefs and behaviour enter as their `_scored` copies, as in the PCA section:
# on the reverse-worded items a high raw answer means LESS support for
# liberalisation, so without the flip a coefficient's sign would record how an
# item happened to be phrased rather than what respondents think.
#
# The `_geen_van_deze` emotion boxes are excluded. Each is 1 exactly when the
# other three boxes for that topic are 0, so it is a deterministic function of
# its neighbours: it carries no information of its own and makes the design
# matrix rank-deficient.
policy_predictors <- function(data,
                              use_columns = USE_COLUMNS,
                              emotions    = emotions_binary) {
  # scored_copies() rather than paste0(): a block with no items must contribute
  # no predictors. The pooled dataset has no behaviour items, and paste0 would
  # add a predictor called "_scored" that no dataset has.
  scored <- function(prefix) scored_copies(select_items(prefix, names(data)))
  emotion_columns <- grep("_geen_van_deze$",
                          setdiff(names(emotions), "ResponseId"),
                          value = TRUE, invert = TRUE)
  out <- c(scored("beliefs"), scored("gedrag_beleid"),
           unname(use_columns), emotion_columns)
  # Only what every respondent answered. step() needs the model frame to stay
  # fixed as terms enter and leave, and a predictor half the sample was never
  # asked would take that half out of every model at once. ANALYSIS_COLUMNS is
  # names(pilot) in both single-pilot layers, so nothing changes there.
  if (exists("ANALYSIS_COLUMNS")) out <- intersect(out, ANALYSIS_COLUMNS)
  stopifnot(all(out %in% names(data)), length(out) > 0)
  out
}


# ---- The demographic model's predictors --------------------------------------
# The five demographic questions, as factors carrying the survey's own answer
# options. `consent` is not among them: every respondent answered 1, and a
# constant cannot enter a model.
# The agency's recodes, which the data layer stores as factors (leeftijd
# labelled from the QSF bands, the rest from the .sav value labels).
DEMOGRAPHIC_ITEMS <- c("stedelijkheid", "geslacht", "stemherinnering", "leeftijd",
                       "opleiding")

# WHY THIS DOES NOT REUSE `politics`, which is the same question already
# collapsed: that collapse is lossy by design. It maps stemgedrag 1-5 onto
# links/midden/rechts and leaves "Ik zou niet stemmen" and "Anders" as NA - some
# thirty respondents - so a model built on it would be fitted on different
# people from the other five, and the comparison this one exists to make would
# not hold. Every answer option is a level here and nobody goes missing.
#
# Three of the five questions are already stored as their own answer options and
# need no conversion; `stad` and `stemgedrag` are still answer codes and are
# labelled here. The scale is passed in as a function rather than read from the
# global `question_scale`, so a test can hand it the pinned .qsf directly.
#
# droplevels() because an answer option nobody chose - there is one, the oldest
# age band - would otherwise become an all-zero column in the design matrix, and
# an aliased term with an NA coefficient.
# Only the questions THIS dataset asked. `opleiding` is pilot 2's alone, so the
# pooled dataset has four of the five - and asking for a column that is not
# there returns NULL, which lapply() turns into a zero-row frame and cbind()
# rejects with "arguments imply differing number of rows: 242, 0".
demographic_frame <- function(data, scale_of = question_scale) {
  items <- intersect(DEMOGRAPHIC_ITEMS, names(data))
  stopifnot(length(items) > 0)
  out <- lapply(items, function(item) {
    column <- data[[item]]
    if (is.factor(column)) droplevels(column)
    else label_from_scale(column, scale_of(item), drop = TRUE)
  })
  names(out) <- items
  as.data.frame(out, stringsAsFactors = FALSE)
}


# ---- The analysis frame ------------------------------------------------------
# ONE complete-case frame, shared by all six models. Not a convenience:
# step() requires the model frame to stay fixed as terms enter and leave, and
# under the default na.action a dropped predictor silently changes which rows
# are in use, so the models compared at successive steps would be fitted on
# different respondents. Sharing it across the six is the second reason - it is
# what makes their R-squared and their term counts comparable at all.
policy_model_data <- function(data,
                              scale_of       = question_scale,
                              policy_columns = POLICY_COLUMNS,
                              use_columns    = USE_COLUMNS,
                              emotions       = emotions_binary) {
  outcomes <- as.data.frame(lapply(POLICY_OUTCOME_SETS, function(substances)
    policy_outcome(data, substances, policy_columns)))
  predictors   <- data[policy_predictors(data, use_columns, emotions)]
  demographics <- demographic_frame(data, scale_of)

  # TWO FRAMES, NOT ONE. The five item models share a frame, which is what makes
  # them comparable to each other and is required by step() besides. The
  # demographic model gets its own.
  #
  # It has to, because `opleiding` was asked in the second pilot only. In one
  # shared frame it would take every pilot-1 respondent out of ALL SIX models to
  # serve the sixth - the five would drop from 227 respondents to about 120,
  # losing half the sample to a variable they do not use. Each frame now keeps
  # whoever answered what it needs, and the two n's are reported side by side
  # rather than one being quietly imposed on the other.
  items <- cbind(outcomes, predictors)
  demos <- cbind(outcomes, demographics)
  list(frame             = items[complete.cases(items), ],
       demographic_frame = demos[complete.cases(demos), ],
       outcomes          = names(outcomes),
       predictors        = names(predictors),
       demographics      = names(demographics))
}


# ---- The models --------------------------------------------------------------
# One model per outcome over the item pool, then the same most-aggregate outcome
# over the demographics alone. trace = 0 because step()'s default is to print
# every candidate model it considered along the way.
fit_policy_models <- function(model_data) {
  fit <- function(outcome, predictors, frame) {
    formula <- reformulate(predictors, response = outcome)
    step(lm(formula, data = frame), direction = "both", trace = 0)
  }
  models <- lapply(model_data$outcomes, fit,
                   predictors = model_data$predictors,
                   frame      = model_data$frame)
  names(models) <- model_data$outcomes

  # The baseline the five are read against: the broadest outcome, predicted by
  # who the respondent is rather than by what they think. On its OWN frame - see
  # policy_model_data() - so that a demographic question one pilot did not ask
  # costs this model respondents rather than costing all six.
  demographic_outcome <- model_data$outcomes[length(model_data$outcomes)]
  models[[paste0(demographic_outcome, " (demografie)")]] <-
    fit(demographic_outcome, model_data$demographics, model_data$demographic_frame)
  models
}


# ---- Reading the models: labels and coefficient tables -----------------------
# What a reader needs from a fitted model is one table per outcome: every
# retained predictor, sorted by |t|, carrying the statement it stands for. The
# printed `summary()` gives none of that - it sorts by the order terms happened
# to enter the formula and identifies each by a column name.
#
# The two functions below build that table; sections/linear_models.Rmd prints
# it.

# The statement an item column asks, without the question stem the export
# prefixes to matrix items. Same idiom as the correlation screen's axis labels.
item_statement <- function(column, item_wording = wording) {
  if (!column %in% names(item_wording)) return(NA_character_)
  trimws(sub("^.*? - ", "", gsub("[[:space:]]+", " ", item_wording[[column]])))
}

# The emotion binaries are DERIVED columns and `wording` has no entry for them -
# it is keyed by the export's six multi-select columns. emotion_item_labels()
# is what the correlation screen uses for the same reason, so the two agree.
emotion_binary_labels <- function(cols = emotion_cols, item_wording = wording) {
  if (!exists("emotion_item_labels") || !exists("emotion_cols"))
    return(stats::setNames(character(0), character(0)))
  stats::setNames(emotion_item_labels(cols, item_wording), emotion_item_columns())
}

# What one contrast column of a demographic factor stands for.
#
# THE DEMOGRAPHIC QUESTIONS ENTER AS ORDERED FACTORS, because label_from_scale()
# in R/plot_helpers.R builds them that way for the descriptive charts,
# where the answer options do have an order on the axis. R's default contrast
# for an ordered factor is POLYNOMIAL, so the model carries `stemgedrag.L`,
# `stemgedrag.Q` and so on - trends across the answer options - rather than one
# term per option. For leeftijd and opleiding that is a reasonable
# parameterisation; for stemgedrag and geslacht the ordering is the order the
# options happen to sit in the survey, and a "linear trend" across it means
# nothing in particular.
#
# This function only NAMES what the model already fitted. Changing the
# parameterisation would change the models, and is a decision about the analysis
# rather than about how it is displayed.
CONTRAST_LABELS <- c(".L" = "lineaire trend", ".Q" = "kwadratische trend",
                     ".C" = "kubische trend")

contrast_label <- function(contrast) {
  if (contrast %in% names(CONTRAST_LABELS))
    return(paste(CONTRAST_LABELS[[contrast]], "over de antwoordopties"))
  if (startsWith(contrast, "^"))
    return(paste0(substring(contrast, 2L),
                  "e-graads trend over de antwoordopties"))
  contrast          # an unordered factor: the term is the level's own name
}

# One readable label per model term. Four kinds of term come out of these
# models, and each is looked up differently:
#
#   beliefs_*_scored, gedrag_beleid_*_scored   the reverse-scored copy of an
#       item. `wording` is keyed by the item, so the suffix comes off first.
#   gebruik_*   an own-use item, whose wording is the bare substance name.
#       Without the prefix "Cannabis" would sit in the table looking like a
#       policy slider.
#   <topic>_<emotie>   a derived binary, labelled as above.
#   <demografie><contrast>   a demographic factor. lm() pastes the contrast's
#       name onto the variable's, so the term is split back against the frame's
#       OWN contrast columns rather than by guessing where the variable name
#       ends - one question's contrast is otherwise indistinguishable from
#       another's level.
#
# A term that matches none of them gets an empty label rather than a wrong one.
model_term_labels <- function(terms, frame,
                              item_wording   = wording,
                              use_columns    = USE_COLUMNS,
                              emotion_labels = emotion_binary_labels()) {
  factor_columns <- names(frame)[vapply(frame, is.factor, logical(1))]
  vapply(terms, function(term) {
    if (term %in% names(emotion_labels))
      return(paste0("emotie: ", emotion_labels[[term]]))
    for (v in factor_columns) {
      if (!startsWith(term, v)) next
      contrast <- substring(term, nchar(v) + 1L)
      if (!contrast %in% colnames(stats::contrasts(frame[[v]]))) next
      return(paste(item_statement(v, item_wording),
                   paste0("(", contrast_label(contrast), ")")))
    }
    column <- sub("_scored$", "", term)
    text   <- item_statement(column, item_wording)
    if (is.na(text)) return("")
    if (column %in% use_columns) paste0("eigen gebruik: ", text) else text
  }, character(1), USE.NAMES = FALSE)
}

# The coefficients of one fitted model, ordered by |t| rather than by the order
# the terms sit in the formula. The intercept is dropped: on these outcomes it
# is the fitted value for a respondent at zero on every predictor, which is
# outside the response scale of all of them.
model_coefficients <- function(fit, frame = stats::model.frame(fit), ...) {
  coefficients <- as.data.frame(summary(fit)$coefficients)
  coefficients$term <- rownames(coefficients)
  coefficients <- coefficients[coefficients$term != "(Intercept)", , drop = FALSE]
  coefficients <- coefficients[order(-abs(coefficients[["t value"]])), , drop = FALSE]
  data.frame(
    voorspeller = coefficients$term,
    stelling    = model_term_labels(coefficients$term, frame, ...),
    b  = round(coefficients$Estimate, 3),
    se = round(coefficients[["Std. Error"]], 3),
    t  = round(coefficients[["t value"]], 1),
    p  = format.pval(coefficients[["Pr(>|t|)"]], digits = 2, eps = 1e-4),
    stringsAsFactors = FALSE, row.names = NULL)
}


