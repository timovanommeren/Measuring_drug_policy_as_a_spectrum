# =============================================================================
# survey_factors.R
# -----------------------------------------------------------------------------
# One structure over the WHOLE survey, rather than one per block.
#
# PCA.R asks "does this block have a structure?", a question asked five times
# with the answer to each confined to its own items. This file asks the
# different question: given every item the survey asks, how many dimensions are
# there, and which items load on them - a dimension may well cut across the
# blocks, and by construction the per-block analysis cannot see one that does.
#
# TWO STEPS, DELIBERATELY SEPARATE
#   1. how many dimensions?  Parallel analysis on a principal-components
#      extraction, the same rule and the same iteration count PCA.R uses per
#      block, so the two are decided the same way.
#   2. what are they?        An exploratory FACTOR analysis with that many
#      factors. PCA and factor analysis answer different questions - components
#      summarise the items, factors model what the items have in common - and
#      the second is what "which items belong together" asks for.
#
# THE ITEMS ARE NOT ON ONE SCALE. Likert items run 1-7, sliders 0-100, and
# substance use 0-2. Everything here works from a CORRELATION matrix, which is
# scale-free, so no standardisation step is needed or wanted. The correlation is
# Spearman (PCA.R's METHOD), because the substance and Likert items are ordinal.
#
# DEPENDENCIES: PCA.R must be sourced first - METHOD, ROTATION, LOAD_MIN,
# N_ITER_PA, SEED, BLOCKS and the `wording` lookup all come from there.
# =============================================================================


## ---- EDITABLE DECISIONS -----------------------------------------------------
FA_METHOD  <- "minres"   # factoring method: minimum residual, psych's default
FA_MAX     <- 8          # ceiling on the number of factors extracted
## -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# survey_factor_solution()
# -----------------------------------------------------------------------------
# Returns the whole-survey solution as data, printing nothing: the number of
# dimensions, the factor solution, a loadings table with item wording, and the
# items it had to leave out.
# -----------------------------------------------------------------------------
survey_factor_solution <- function(pilot, columns = unlist(unname(BLOCKS)),
                                   block_of = NULL) {
  set.seed(SEED)
  x <- as.matrix(pilot[columns])

  # An item with no variance has no correlations either: its row of R is NA and
  # eigen() then refuses the whole matrix rather than just that item. In this
  # sample `gebruik_heroine` is exactly that.
  constant <- vapply(seq_len(ncol(x)), function(j) {
    v <- x[, j][!is.na(x[, j])]
    length(v) == 0L || stats::var(v) == 0
  }, logical(1))
  dropped <- columns[constant]
  x       <- x[, !constant, drop = FALSE]
  columns <- columns[!constant]

  R <- cor(x, use = "pairwise.complete.obs", method = METHOD)

  # Parallel analysis wants raw data, and psych has no method = "spearman", so
  # it is given the RANKS - Spearman is Pearson on ranks, which keeps step 1 on
  # the same matrix as step 2 instead of silently switching to Pearson.
  complete <- complete.cases(x)
  ranked   <- apply(x[complete, , drop = FALSE], 2, rank, na.last = "keep")
  n_obs    <- sum(complete)

  invisible(utils::capture.output(suppressWarnings(
    pa <- psych::fa.parallel(ranked, fa = "pc", n.iter = N_ITER_PA, plot = FALSE)
  )))
  k <- max(1L, min(pa$ncomp, FA_MAX, ncol(x) - 1L))

  # The factor analysis itself, on the same correlation matrix. Oblique
  # rotation: dimensions of an attitude survey are expected to correlate, and
  # forcing them orthogonal would hide exactly that.
  fit <- psych::fa(R, nfactors = k, rotate = ROTATION, fm = FA_METHOD,
                   n.obs = n_obs)

  loads <- unclass(fit$loadings)
  primary <- apply(abs(loads), 1, which.max)
  prim_load <- loads[cbind(seq_len(nrow(loads)), primary)]

  item_text <- function(column)
    if (is.na(wording[column])) column else item_title(wording[[column]])

  table <- data.frame(
    variable  = columns,
    block     = if (is.null(block_of)) NA_character_ else block_of[columns],
    wording   = vapply(columns, item_text, character(1)),
    factor    = ifelse(abs(prim_load) >= LOAD_MIN, paste0("F", primary), "-"),
    loading   = round(prim_load, 2),
    h2        = round(fit$communality, 2),
    row.names = NULL, stringsAsFactors = FALSE)

  list(k = k, k_parallel = pa$ncomp, n_obs = n_obs, fit = fit, R = R,
       loadings = loads, table = table, columns = columns, dropped = dropped,
       # % of variance each factor accounts for, from psych's own summary
       var_pc = round(100 * fit$Vaccounted["Proportion Var", ], 1),
       cumulative = round(100 * sum(fit$Vaccounted["Proportion Var", ]), 1),
       # factor correlations exist only for an oblique rotation with k > 1
       phi = if (!is.null(fit$Phi)) round(fit$Phi, 2) else NULL)
}
