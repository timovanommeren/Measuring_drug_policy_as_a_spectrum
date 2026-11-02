# =============================================================================
# PCA.R
# -----------------------------------------------------------------------------
# A principal component analysis per block of the survey (sections/pca.Rmd),
# complementing the within-block correlation matrices (sections/correlations.Rmd).
# The correlation screen shows which PAIRS of items are near-duplicates; this
# shows whether a block has a STRUCTURE, decomposing the same Spearman matrix.
#
# Components are estimated within one block at a time, so C1 of one block has
# nothing to do with C1 of another. pca_block() computes one block and returns
# the numbers; the section renders them.
# =============================================================================

# psych is called through its namespace rather than attached: it masks several
# common names.

# ---- Editable decisions --------------------------------------------------------
METHOD        <- "spearman"  # the matrix the correlation screen plots
ROTATION      <- "oblimin"   # oblique: sub-scales are expected to correlate
LOAD_MIN      <- 0.40        # minimum |loading| to call an item a member
MIN_ITEMS_PCA <- 5           # below this: reduced report, no rotation
MODAL_MAX     <- 0.95        # flag an item if one answer holds this share
N_ITER_PA     <- 100         # parallel-analysis iterations
TEXT_CAP      <- 46          # characters of wording on an axis
SEED          <- 2026        # parallel analysis simulates data; fix its seed

# Override the retention rule for a named block, e.g. c(Beliefs = 3). The
# empirical rule (parallel analysis) applies to every block not named here.
FORCE_K <- c()


# ---- The blocks ------------------------------------------------------------------
# In survey order; BLOCK_NAMES also names the matrices in sections/correlations.Rmd.
# Beliefs and Behaviour use the reverse-scored copies, so that high means more
# pro-liberalisation on every item and a loading's sign is not a wording artefact.
BLOCK_NAMES <- c("Beliefs", "Behaviour", "Substance use",
                 "Policy preference", "Involvement")
BLOCKS <- setNames(
  list(
    scored_copies(select_items("beliefs", names(pilot))),
    scored_copies(select_items("gedrag_beleid", names(pilot))),
    subst_cols,                       # own use per substance group (four items)
    unname(POLICY_COLUMNS),
    select_items("betrokkenheid", names(pilot))
  ),
  BLOCK_NAMES
)
BLOCKS <- lapply(BLOCKS, intersect, ANALYSIS_COLUMNS)
stopifnot(all(unlist(BLOCKS) %in% names(pilot)))

# a component needs at least a pair of items
BLOCKS_FITTED <- Filter(function(cols) length(cols) >= 2L, BLOCKS)
stopifnot(length(BLOCKS_FITTED) > 0)


# -----------------------------------------------------------------------------
# pca_block()
# -----------------------------------------------------------------------------
# One block: decide how many components it supports, fit them, and return the
# numbers. It prints nothing and writes nothing.
#
# set.seed() lives here rather than around the loop: parallel analysis
# simulates data, so a block's retention count must not depend on how many
# blocks happened to be fitted before it.
# -----------------------------------------------------------------------------
pca_block <- function(cols, block_name, pilot) {
  set.seed(SEED)
  x <- as.matrix(pilot[cols])

  # An item with NO variance at all has no correlations either: its row and
  # column of R come back NA, and eigen() then refuses the entire matrix rather
  # than just that item. Such an item cannot load on any component, so it is
  # dropped here instead of taking the whole block down with it.
  #
  # This is the extreme end of the near-constant problem handled further down:
  # items that do vary, however slightly, are kept and reported via modal_share.
  constant <- vapply(seq_len(ncol(x)), function(j) {
    v <- x[, j][!is.na(x[, j])]
    length(v) == 0L || stats::var(v) == 0
  }, logical(1))
  if (any(constant)) {
    message(sprintf("%s: dropped %d item(s) with no variance in this sample: %s",
                    block_name, sum(constant), paste(cols[constant], collapse = ", ")))
    x    <- x[, !constant, drop = FALSE]
    cols <- cols[!constant]
  }

  k_items <- ncol(x)

  # The correlation matrix, from the SAME call the correlation screen makes -
  # not a re-derivation. Everything downstream (principal, KMO, Bartlett) reads
  # a correlation matrix, so this is the only place the data enter the model.
  R <- cor(x, use = "pairwise.complete.obs", method = METHOD)

  # Parallel analysis is the exception: it needs raw data, not a matrix, and
  # psych::fa.parallel() has no method = "spearman". Spearman IS Pearson on the
  # ranks, so ranking first gives it the same matrix rather than silently
  # switching the whole retention decision to Pearson. na.last = "keep"
  # matters: rank()'s default would hand the "Geen mening" cells a real rank.
  complete <- complete.cases(x)
  ranked   <- apply(x[complete, , drop = FALSE], 2, rank, na.last = "keep")

  n_obs  <- sum(complete)
  eig    <- eigen(R, only.values = TRUE)$values
  var_pc <- eig / k_items * 100          # % of total variance per component

  # Near-constant items. The report's Analyses tab lists near-zero variance as
  # one of the things that biases a network, and it breaks a PCA for the same
  # reason: an item almost everyone answers identically has nothing left to
  # covary with, so it cannot load anywhere and it drags the block's KMO down.
  # It is also what makes fa.parallel's resampling warn about a zero standard
  # deviation - a resample can easily draw only the modal answer - which is why
  # that call is wrapped below rather than left to emit dozens of warnings.
  modal_share <- apply(x, 2, function(v) {
    v <- v[!is.na(v)]
    max(table(v)) / length(v)
  })

  # ---- how many components? --------------------------------------------------
  # Thin blocks are not asked the question at all: with 3 or 4 items there is
  # not enough covariance structure for a second component to mean anything,
  # and rotation is undefined for a single component.
  thin <- k_items < MIN_ITEMS_PCA
  if (thin) {
    k_pa <- NA_integer_
    k    <- 1L
  } else {
    # fa.parallel() cat()s its own one-line verdict even with plot = FALSE;
    # swallowed here so the caller is the single reporter of it.
    invisible(capture.output(suppressWarnings(
      pa <- psych::fa.parallel(ranked, fa = "pc", n.iter = N_ITER_PA,
                               plot = FALSE)
    )))
    k_pa <- pa$ncomp
    # never extract more components than the block can support
    k <- max(1L, min(as.integer(k_pa), k_items - 1L))
  }
  if (block_name %in% names(FORCE_K)) k <- as.integer(FORCE_K[[block_name]])

  # ---- fit -------------------------------------------------------------------
  rotation <- if (k == 1) "none" else ROTATION
  fit <- psych::principal(R, nfactors = k, rotate = rotation, n.obs = n_obs)

  loads <- unclass(fit$loadings)
  colnames(loads) <- paste0("C", seq_len(ncol(loads)))
  primary   <- apply(abs(loads), 1, which.max)
  prim_load <- apply(abs(loads), 1, max)
  assigned  <- prim_load >= LOAD_MIN

  # `wording` is keyed by the RAW variable name, so the _scored suffix has to
  # come off before the lookup - reverse-scoring does not change the question.
  text_full <- sub("^.*? - ", "",
                   gsub("[\r\n]+", " ", wording[sub("_scored$", "", cols)]))
  text_full[is.na(text_full)] <- cols[is.na(text_full)]

  # Two shapes of the same result. The WIDE table is what reads well in a
  # console and in a knitr::kable(), one row per item; but its columns depend
  # on how many components that block retained, so blocks cannot be stacked in
  # that shape. The csv is therefore built LONG, which stacks cleanly and is
  # what any downstream filtering or joining wants anyway.
  wide <- data.frame(
    block     = block_name,
    number    = seq_along(cols),
    variable  = cols,
    wording   = text_full,
    round(as.data.frame(loads), 3),
    primary   = ifelse(assigned, paste0("C", primary), "-"),
    prim_load = round(prim_load, 3),
    row.names = NULL, stringsAsFactors = FALSE, check.names = FALSE
  )

  long <- data.frame(
    block     = block_name,
    number    = rep(seq_along(cols), times = ncol(loads)),
    variable  = rep(cols, times = ncol(loads)),
    wording   = rep(text_full, times = ncol(loads)),
    component = rep(colnames(loads), each = length(cols)),
    loading   = round(as.vector(loads), 3),
    primary   = rep(ifelse(assigned, paste0("C", primary), "-"),
                    times = ncol(loads)),
    prim_load = rep(round(prim_load, 3), times = ncol(loads)),
    row.names = NULL, stringsAsFactors = FALSE
  )

  # Reliability of each retained component, on the items assigned to it. Alpha
  # only: omega on a handful of items at this n is not stable enough to report.
  # suppressMessages as well as suppressWarnings - on the 0-100 sliders alpha()
  # advises increasing the number of categories, which is about its frequency
  # table, not about the coefficient being reported.
  alphas <- vapply(seq_len(k), function(ci) {
    members <- cols[assigned & primary == ci]
    if (thin || length(members) < 3) return(NA_real_)
    a <- suppressMessages(suppressWarnings(
      psych::alpha(pilot[complete, members], warnings = FALSE)))
    a$total$raw_alpha
  }, numeric(1))
  n_members <- vapply(seq_len(k),
                      function(ci) sum(assigned & primary == ci), integer(1))

  # ---- figures ---------------------------------------------------------------
  # Scree: observed eigenvalues against the flat "one component per item" line.
  scree_df <- data.frame(component = seq_along(eig), eigenvalue = eig)
  scree <- ggplot(scree_df, aes(x = component, y = eigenvalue)) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "grey60") +
    geom_line(colour = "grey30") +
    geom_point(size = 1.8,
               colour = ifelse(scree_df$component <= k, "#B2182B", "grey45")) +
    scale_x_continuous(breaks = seq_along(eig)) +
    labs(title = paste0(block_name, ": scree"), x = "component",
         y = "eigenvalue") +
    theme_minimal(base_size = 9) +
    theme(panel.grid.minor = element_blank())

  # Loading heatmap, in the same visual grammar as the correlation matrices:
  # item wording down the rows, the same blue-white-red diverging scale on a
  # fixed [-1, 1], and a black outline on the cells that clear the membership
  # threshold. Read side by side, a strongly correlated pair in the matrix
  # should be a pair of cells in the same column here.
  tiles <- expand.grid(variable = cols, component = colnames(loads),
                       stringsAsFactors = FALSE)
  tiles$loading <- as.vector(loads)
  tiles$flag    <- abs(tiles$loading) >= LOAD_MIN
  # Wrapped on the axis below rather than truncated here: several items share
  # an opening clause, and a cut-off label makes them indistinguishable.
  row_labels    <- paste0(seq_along(cols), ". ", text_full)
  tiles$row_lab <- factor(row_labels[match(tiles$variable, cols)],
                          levels = row_labels)

  loadings_plot <- ggplot(tiles, aes(x = component, y = row_lab,
                                     fill = loading)) +
    geom_tile(colour = "white") +
    geom_tile(data = tiles[tiles$flag, ], colour = "black", fill = NA) +
    geom_text(aes(label = sub("^0", "", sprintf("%.2f", loading))),
              size = if (k_items > 12) 2.1 else 2.8, colour = "grey15") +
    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                         midpoint = 0, limits = c(-1, 1), name = "loading") +
    scale_y_discrete(limits = rev(row_labels),
                     labels = scales::label_wrap(TEXT_CAP)) +
    labs(title = paste0(block_name, ": ", rotation, " loadings"),
         subtitle = paste0(k_items, " items, ", k, " component",
                           if (k != 1) "s", "; outlined = |loading| >= ",
                           LOAD_MIN),
         x = NULL, y = NULL) +
    theme_minimal(base_size = 9) +
    theme(axis.text.x = element_text(size = 8, colour = "grey15"),
          axis.text.y = element_text(size = 7.5, colour = "grey15", hjust = 1),
          panel.grid = element_blank())

  list(
    block     = block_name,
    thin      = thin,
    rotation  = rotation,
    n_obs     = n_obs,
    eig       = eig,
    var_pc    = var_pc,
    loadings  = loads,
    table     = wide,
    long      = long,
    alphas    = alphas,
    n_members = n_members,
    kmo       = psych::KMO(R),
    bartlett  = psych::cortest.bartlett(R, n = n_obs),
    near_constant = modal_share >= MODAL_MAX,
    modal_share   = modal_share,
    r_scores  = if (k > 1) fit$r.scores else NULL,
    scree     = scree,
    loadings_plot = loadings_plot,
    retention = data.frame(
      Block      = block_name,
      Items      = k_items,
      n          = n_obs,
      `n/item`   = round(n_obs / k_items, 1),
      `Parallel` = if (thin) "-" else as.character(k_pa),
      `Kaiser`   = sum(eig > 1),
      `PC1 var%` = round(var_pc[1], 1),
      Retained   = k,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  )
}


# =============================================================================
# STANDALONE RUN  --  only when this file is being run, not when it is loaded
# =============================================================================
