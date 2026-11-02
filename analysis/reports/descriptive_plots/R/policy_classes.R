# =============================================================================
# policy_classes.R
# -----------------------------------------------------------------------------
# Latent profile analysis over the eight policy sliders: are there types of
# respondent, or is there one ladder that everybody walks at a different height?
#
# THAT IS THE QUESTION, and it is worth stating before the method, because the
# figure it feeds (the ladder plot, split by class) makes a strong-looking
# picture either way. Splitting respondents into thirds by their mean slider -
# which is what the first draft of that figure did - GUARANTEES three parallel
# lines at different heights, since the split is on the height. A class model
# is allowed to come back with classes that differ in SHAPE, or with one class,
# and those are different findings.
#
# WHY PROFILES AND NOT CLASSES. The sliders are continuous, so mclust's Gaussian
# mixtures need 8 means and a covariance structure per class - against poLCA on
# the binned anchors, which needs 8 x (k - 1) response probabilities per class
# and is not estimable at pilot n without collapsing the anchors first. The
# cost is real and should be written up: 59-70% of answers sit EXACTLY on an
# anchor (reference/pilot_verdeling.md), so these are not Gaussian variables and
# the within-class normality assumption is violated in a known direction.
# The main study, at ten times the n, can afford poLCA on collapsed anchors as
# the check that this choice does not decide the answer.
#
# Everything here is estimated on the same eight substances the ladder plot
# draws (LADDER_SUBSTANCES below), so the model and the figure cannot describe
# different variables. policy_long() builds the frame behind the policy figures
# in sections/policy_profiles.Rmd; the plots themselves are in that section.
# =============================================================================

library(here)
library(mclust)   # attached, not ::-called: Mclust() looks up
                  # mclustBIC() by name and fails when only the namespace is
                  # loaded. Same idiom as library(remode) in involvement.Rmd.


## ---- EDITABLE DECISIONS -----------------------------------------------------
# PIJNSTILLERS IS LEFT OUT OF THE LADDER AND THE CLASS MODEL. Three quarters of
# the sample puts it within one anchor of "alleen op recept", which is where it
# already legally sits, so as an indicator it carries almost no variance and
# would only add a near-constant dimension to the class model. This follows
# POLICY_OUTCOME_SETS in R/linear_models.R, which leaves it out of every outcome
# for a related reason.
LADDER_SUBSTANCES <- setdiff(names(POLICY_COLUMNS), "pijnstillers")
## -----------------------------------------------------------------------------


# ---- One row per respondent per substance -----------------------------------
# Long rather than wide because every figure here is a per-substance panel or a
# per-substance line, and ggplot wants the substance as a column.
#
# `substances` rather than a hard-coded list: a caller may want the eight of
# LADDER_SUBSTANCES or all nine, and that is the only difference between their
# frames.
policy_long <- function(data, substances = names(POLICY_COLUMNS),
                        policy_columns = POLICY_COLUMNS,
                        labels = SUBSTANCE_LABELS) {
  columns <- policy_columns[substances]
  stopifnot(!anyNA(columns), all(columns %in% names(data)))

  long <- data.frame(
    ResponseId = rep(data$ResponseId, length(columns)),
    middel     = rep(substances, each = nrow(data)),
    beleid     = unlist(data[unname(columns)], use.names = FALSE),
    stringsAsFactors = FALSE)
  long <- long[!is.na(long$beleid), ]

  # Ordered by the median of the substance, computed on this frame, so a figure
  # drawn on a subgroup orders itself by that subgroup.
  #
  # DESCENDING: the most permissive substance is level 1. That is the order the
  # ladder plot reads left to right, and the order the profile table reads.
  ranked <- names(sort(tapply(long$beleid, long$middel, median),
                       decreasing = TRUE))
  long$middel <- factor(long$middel, levels = ranked, labels = labels[ranked],
                        ordered = TRUE)
  long
}



## ---- EDITABLE DECISIONS -----------------------------------------------------
LPA_CLASSES <- 1:6      # class counts to fit; BIC chooses among them
LPA_SEED    <- 20260909 # mclust's initialisation is deterministic, but the
                        # bootstrap LRT below is not
LPA_MODELS  <- NULL     # NULL = let mclust compare all its covariance
                        # structures; set e.g. c("EEI", "VVI") to restrict
LPA_BOOTSTRAP <- 0      # replications for mclustBootstrapLRT; 0 skips it.
                        # 999 takes several minutes and belongs in the main
                        # study, not in every knit of the pilot report.
LPA_MIN_SHARE <- 0.05   # a class smaller than this share of the sample is
                        # FLAGGED, not dropped. On pilot 2 the BIC-preferred
                        # solution has two classes under it; see the note in
                        # class_quality() for why that is reported rather than
                        # fixed by lowering LPA_CLASSES.
## -----------------------------------------------------------------------------


# ---- The indicator matrix ---------------------------------------------------
# Complete cases only: mclust has no missing-data handling, and every missing
# value on these columns is a "Geen mening" answer - a real response option
# rather than a skipped question - so dropping those respondents drops the ones
# most ambivalent about the substance. Reported rather than silent.
policy_indicators <- function(data, substances = LADDER_SUBSTANCES,
                              policy_columns = POLICY_COLUMNS) {
  X <- as.matrix(data[unname(policy_columns[substances])])
  colnames(X) <- substances
  rownames(X) <- data$ResponseId
  X[complete.cases(X), , drop = FALSE]
}


# ---- The model ---------------------------------------------------------------
# Mclust() fits every covariance structure at every class count and returns the
# best by BIC. The whole BIC surface comes back with it, which is what the
# section prints: the winner alone hides how close the runners-up were, and on
# a pilot they are usually close.
fit_policy_classes <- function(data, substances = LADDER_SUBSTANCES,
                               G = LPA_CLASSES, modelNames = LPA_MODELS) {
  set.seed(LPA_SEED)
  X   <- policy_indicators(data, substances)
  fit <- Mclust(X, G = G, modelNames = modelNames, verbose = FALSE)
  fit$indicators <- X
  fit
}


# ---- How much to trust the assignment ---------------------------------------
# The three numbers a latent-class solution is normally reported with, so the
# section does not have to compute them inline.
#
# ENTROPY is the relative entropy of Ramaswamy et al. (1993): 1 when every
# respondent belongs to one class with probability 1, 0 when the posterior is
# uniform. Above .80 is the usual rule of thumb for treating the assignment as
# if it were observed - which is exactly what the ladder figure does when it
# facets on it, so the number belongs in the caption of that figure.
#
# The per-class average posterior probability says WHERE any fuzziness sits: one
# clean class and one that nobody belongs to with confidence is a different
# situation from uniform mild uncertainty.
#
# A NEAR-PERFECT ENTROPY IS NOT GOOD NEWS AT THIS n. On pilot 2 it comes back at
# .99 with two classes of five and nine respondents, which is what separation by
# a handful of outliers looks like rather than what well-separated types look
# like: a component fitted to five people who answered 0 to everything has
# nothing to be uncertain about. That is why `klein` is in the table - entropy
# and class size have to be read together, and entropy alone flatters exactly
# the solution that should be distrusted.
class_quality <- function(fit) {
  z <- fit$z
  if (is.null(z) || fit$G == 1)
    return(list(entropy = NA_real_, table = data.frame(
      klasse = 1L, n = fit$n, aandeel = 1, gem_posterior = 1)))

  # 0 log 0 is 0 here, and R gives NaN, so the zero rows are dropped rather
  # than na.rm'd - na.rm would also hide a genuine NA if one ever appeared.
  p <- z[z > 0]
  entropy <- 1 - (-sum(p * log(p))) / (nrow(z) * log(fit$G))

  sizes <- table(factor(fit$classification, seq_len(fit$G)))
  list(entropy = entropy, table = data.frame(
    klasse        = seq_len(fit$G),
    n             = as.integer(sizes),
    aandeel       = as.numeric(prop.table(sizes)),
    gem_posterior = vapply(seq_len(fit$G), function(g)
      mean(z[fit$classification == g, g]), numeric(1)),
    klein         = as.numeric(prop.table(sizes)) < LPA_MIN_SHARE))
}


# ---- The classes, back on the respondents -----------------------------------
# A factor on `pilot`, NA for the respondents the model could not use.
#
# RENUMBERED BY MEAN PERMISSIVENESS, because mclust's own class numbers are
# arbitrary: refit on one more respondent and class 2 and class 5 can swap, so
# a figure keyed to the raw numbers changes colours for no reason.
#
# NUMBERED AND NOT NAMED. An earlier version of this file called them
# prohibitionist / restrictief / gematigd / regulerend / liberaal / libertair,
# and that was wrong in a way worth recording: those names assert that the
# classes sit on ONE dimension, which is the very thing the model is being
# asked. Pilot 2's solution says otherwise - its second class puts nicotine at
# 9 and alcohol at 71, its third puts cannabis at 7 and nicotine at 71 - so
# ranking them by an overall mean produces names that misdescribe the shapes
# they are attached to. The profile table is where a class gets its meaning;
# names can be assigned by hand once the main-study solution is settled.
policy_class <- function(data, fit) {
  ranked <- order(tapply(rowMeans(fit$indicators), fit$classification, mean))
  name   <- paste("klasse", seq_len(fit$G))

  out <- rep(NA_character_, nrow(data))
  out[match(rownames(fit$indicators), data$ResponseId)] <-
    name[match(fit$classification, ranked)]
  factor(out, levels = name)
}


# ---- Mean slider per substance per class ------------------------------------
# The profile plot's data, and the thick lines of the ladder figure. Long, and
# with `middel` ordered exactly as policy_long() orders it, so a profile line
# and a respondent line can be drawn on one pair of axes.
class_profiles <- function(data, class, substances = LADDER_SUBSTANCES,
                           policy_columns = POLICY_COLUMNS,
                           labels = SUBSTANCE_LABELS) {
  long <- policy_long(data, substances, policy_columns, labels)
  long$klasse <- class[match(long$ResponseId, data$ResponseId)]
  long <- long[!is.na(long$klasse), ]

  stats <- aggregate(beleid ~ klasse + middel, long, function(v)
    c(gemiddelde = mean(v), mediaan = median(v),
      onder = mean(v) - 1.96 * sd(v) / sqrt(length(v)),
      boven = mean(v) + 1.96 * sd(v) / sqrt(length(v)), n = length(v)))
  cbind(stats[c("klasse", "middel")], as.data.frame(stats$beleid))
}


