# =============================================================================
# trend_analysis.R
# -----------------------------------------------------------------------------
# ANOVA trend analysis: split the between-groups variance of a one-way ANOVA
# into components of one degree of freedom each, and test them separately.
# Used on the Analyses tab to ask whether the ordered use items relate linearly
# to their own policy slider.
#
# Both functions here are the same idea with a different contrast matrix, and
# both are one call to aov() with `split =`. Nothing is computed by hand: R
# already partitions sums of squares, and doing that arithmetic longhand only
# adds somewhere for it to go wrong.
#
#   trend_test()       linear (1 df) versus deviation from linearity (k - 2 df)
#   trend_contrasts()  ever tried, frequency among users, curvature among users
#
# DEPENDENCIES: base R only.
# =============================================================================


# -----------------------------------------------------------------------------
# Shared preparation
# -----------------------------------------------------------------------------
# Drop the pairwise-incomplete rows and hand back the two variables in the
# shape aov() wants. A data frame rather than two vectors, so the model term is
# called `g` and the split can be named after it.
# -----------------------------------------------------------------------------
trend_frame <- function(y, g) {
  y <- suppressWarnings(as.numeric(y))
  g <- factor(g)
  keep <- !is.na(y) & !is.na(g)
  data.frame(y = y[keep], g = droplevels(g[keep]))
}

# An empty result, so a substance that cannot be tested still occupies a row in
# the table rather than vanishing from it.
trend_empty <- function(columns, n, k) {
  empty <- as.list(rep(NA_real_, length(columns)))
  names(empty) <- columns
  data.frame(n = n, k = k, empty, stringsAsFactors = FALSE)
}


# -----------------------------------------------------------------------------
# trend_test()
# -----------------------------------------------------------------------------
# An ORDERED factor makes R use orthogonal polynomial contrasts, so the first
# component of the between-groups effect is exactly the linear trend and
# everything after it is the bend. That is the whole partition, and it assumes
# equally spaced levels: the scores are 1, 2, ... k.
#
# Both F ratios are tested against the full model's within-groups mean square,
# which is what `split =` does. Using the linear model's own residual instead
# would fold the deviation back into the error term and defeat the point.
#
# `lin_share` is the proportion of the between-groups sum of squares that a
# straight line already accounts for. Worth reporting beside the p-values,
# because a deviation term can reach significance while the line still carries
# almost all of the effect, and the reverse.
# -----------------------------------------------------------------------------
trend_test <- function(y, g) {
  d <- trend_frame(y, g)
  k <- nlevels(d$g)
  columns <- c("F_lin", "p_lin", "F_dev", "p_dev", "eta2", "lin_share", "slope")

  if (k < 2 || nrow(d) < k + 2) return(trend_empty(columns, nrow(d), k))

  d$g <- ordered(d$g)
  split <- list(linear = 1)
  if (k > 2) split$deviation <- 2:(k - 1)
  tab <- summary(aov(y ~ g, d), split = list(g = split))[[1]]

  between <- tab["g", "Sum Sq"]
  within  <- tab["Residuals", "Sum Sq"]
  cell <- function(row, column) unname(tab[row, column])

  data.frame(
    n         = nrow(d),
    k         = k,
    F_lin     = cell("  g: linear", "F value"),
    p_lin     = cell("  g: linear", "Pr(>F)"),
    F_dev     = if (k > 2) cell("  g: deviation", "F value") else NA,
    p_dev     = if (k > 2) cell("  g: deviation", "Pr(>F)") else NA,
    eta2      = between / (between + within),
    lin_share = cell("  g: linear", "Sum Sq") / between,
    # in slider points per level, which the polynomial contrasts do not give
    # directly: they are scaled to unit length, not to the level index
    slope     = unname(coef(lm(y ~ as.numeric(g), d))[2])
  )
}


# -----------------------------------------------------------------------------
# trend_contrasts()
# -----------------------------------------------------------------------------
# A sharper partition of the same degrees of freedom, for the four-level use
# items specifically. It exists because those four levels are not four steps of
# one thing:
#
#   1 nooit                          )  never tried it
#   2 niet (in het laatste jaar)     )  tried it, not recently
#   3 wel eens (in het laatste jaar)    uses it
#   4 vaak (in het laatste jaar)        uses it often
#
# Level 1 to 2 is a LIFETIME distinction; 2 to 3 to 4 is a FREQUENCY gradient.
# trend_test() scores all three steps as equal, which is what pushes variance
# into its deviation term. These contrasts ask the two questions separately:
#
#   ever       -3  1  1  1   ever tried it, versus never
#   lin_users   0 -1  0  1   frequency gradient among those who have
#   curv        0  1 -2  1   curvature among those who have
#
# Mutually orthogonal, so the three sums of squares partition the between-
# groups total and can be read independently of one another.
#
# Needs exactly four populated levels; anything else gets an empty row, which
# is how heroine drops out - only three of its levels are used.
# -----------------------------------------------------------------------------
TREND_CONTRASTS <- cbind(ever      = c(-3, 1, 1, 1),
                         lin_users = c( 0, -1, 0, 1),
                         curv      = c( 0, 1, -2, 1))

trend_contrasts <- function(y, g, label = "") {
  d <- trend_frame(y, g)
  k <- nlevels(d$g)
  columns <- c("F_ever", "p_ever", "F_users", "p_users", "F_curv", "p_curv")

  if (k != 4 || nrow(d) < 8) {
    return(cbind(label = label, trend_empty(columns, nrow(d), k)))
  }

  contrasts(d$g) <- TREND_CONTRASTS
  tab <- summary(aov(y ~ g, d),
                 split = list(g = list(ever = 1, lin_users = 2, curv = 3)))[[1]]
  # summary.aov names the split rows with two leading spaces
  cell <- function(row, column) unname(tab[paste0("  g: ", row), column])

  data.frame(
    label   = label,
    n       = nrow(d),
    k       = k,
    F_ever  = cell("ever",      "F value"),
    p_ever  = cell("ever",      "Pr(>F)"),
    F_users = cell("lin_users", "F value"),
    p_users = cell("lin_users", "Pr(>F)"),
    F_curv  = cell("curv",      "F value"),
    p_curv  = cell("curv",      "Pr(>F)"),
    stringsAsFactors = FALSE
  )
}
