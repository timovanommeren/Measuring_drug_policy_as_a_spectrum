library(tidyverse)
library(splines)
library(furrr)

source(here::here("analysis", "R", "load_and_clean.R"))

# Does involvement exaggerate demographic differences in preferred drug policy, and above all,
# does it polarise opinion within demographic groups?
#
# Per drug x involvement item: a multinomial model of the (merged) 9 answer options,
# option ~ (sex + age + edu + urb3 + vote) * involvement, ridge-penalised (glmnet), weighted.
# From g-computed distributions per demographic group at low (P25) and high (P75) involvement:
# - Leik's ordinal dispersion D (primary) and Van der Eijk's agreement A (bimodality)
# - the signed gap in mean position between the two groups of each contrast
# Uncertainty: respondent bootstrap. Null N0 (no involvement effect): involvement permuted within
# demographic cells. Null N1 (same involvement effect for all groups): outcomes simulated from
# the additive model. Statistics and figures: moderation_involvement_summary.R.
#
# Usage: Rscript moderation_involvement.R [n_boot n_perm n_sim out_subdir]

args <- commandArgs(trailingOnly = TRUE)
n_boot <- if (length(args) >= 1) as.integer(args[1]) else 1000
n_perm <- if (length(args) >= 2) as.integer(args[2]) else 2000
n_sim  <- if (length(args) >= 3) as.integer(args[3]) else 200
out_dir <- here::here("analysis", "output", if (length(args) >= 4) args[4] else "involvement_moderation")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
n_cores <- 12
min_n <- 30
set.seed(2026)
t_start <- Sys.time()

drug_labels <- c(alcohol = "Alcohol", tabak_en_vapes = "Tobacco and vapes", cannabis = "Cannabis",
                 psychedelica = "Psychedelics", mdma = "MDMA", cocaine = "Cocaine / crack",
                 speed = "Speed", heroine = "Heroin")

item_labels <- c(betrokkenheid_1  = "Drug crime in my surroundings",
                 betrokkenheid_1b = "Drugs in my surroundings",
                 betrokkenheid_2  = "Know more about drugs than most",
                 betrokkenheid_2b = "Know more about drug crime than most",
                 betrokkenheid_3  = "Drug policy weighs in my vote")


## Data

# one respondent is missing on all involvement sliders and is dropped
d <- df |>
  filter(if_all(all_of(names(item_labels)), \(x) !is.na(x))) |>
  mutate(
    age = fct_collapse(age_4_rec, "18–49" = c("18-34", "35-49"), "50+" = c("50-64", "65+")),
    edu = fct_recode(haven::as_factor(edu_2_rec), practical = "Praktisch-opgeleid",
                     theoretical = "Theoretisch-opgeleid"),
    urb3 = factor(case_when(as.integer(urb) <= 2 ~ "urban", as.integer(urb) >= 3 ~ "rural",
                            .default = "unknown"),
                  levels = c("urban", "rural", "unknown")),
    vote = fct_na_value_to_level(vote_lr, "other/none"),
    sex = fct_drop(sex_rec),
    across(all_of(names(item_labels)), \(x) as.numeric(x) / 100)
  )
stopifnot(!anyNA(select(d, age, edu, urb3, vote, sex, w)))

demo_vars <- c("sex", "age", "edu", "urb3", "vote")

# contrasts: the second level is the one expected to be more restrictive (Figure 3);
# sex has no expected direction and is not part of the primary test
contrasts <- tribble(
  ~var,   ~first,        ~second,     ~primary,
  "age",  "18–49",       "50+",       TRUE,
  "edu",  "theoretical", "practical", TRUE,
  "urb3", "urban",       "rural",     TRUE,
  "vote", "left",        "right",     TRUE,
  "sex",  "Man",         "Vrouw",     FALSE
)

profiles <- list(
  "Young, theoretical, urban, left" = list(age = "18–49", edu = "theoretical", urb3 = "urban", vote = "left"),
  "Old, practical, rural, right"    = list(age = "50+", edu = "practical", urb3 = "rural", vote = "right")
)

weighted_quantile <- function(x, w, p) {
  o <- order(x)
  map_dbl(p, \(pp) x[o][which(cumsum(w[o]) >= pp * sum(w))[1]])
}

# low/high involvement and the points of the spline curves, per item (fixed, full sample)
probs <- c(p10 = 0.10, p25 = 0.25, p50 = 0.50, p75 = 0.75, p90 = 0.90)
inv_q <- map(names(item_labels), \(it) setNames(weighted_quantile(d[[it]], d$w, probs), names(probs))) |>
  set_names(names(item_labels))

# spline basis per item, knots fixed on the full sample
spline_knots <- map(names(item_labels), \(it) {
  b <- ns(d[[it]], df = 3)
  list(knots = attr(b, "knots"), boundary = attr(b, "Boundary.knots"))
}) |>
  set_names(names(item_labels))


## Merging sparse answer options

# repeatedly merge the smallest category (< min_n respondents) with its smaller neighbour
merge_options <- function(x, min_n) {
  n <- tabulate(x, 9)
  groups <- as.list(1:9)
  repeat {
    cnt <- map_int(groups, \(g) sum(n[g]))
    if (min(cnt) >= min_n || length(groups) == 1) break
    i <- which.min(cnt)
    nb <- c(i - 1, i + 1)
    nb <- nb[nb >= 1 & nb <= length(groups)]
    j <- nb[which.min(cnt[nb])]
    groups[[min(i, j)]] <- sort(c(groups[[i]], groups[[j]]))
    groups <- groups[-max(i, j)]
  }
  groups
}

category_label <- \(g) if (length(g) == 1) as.character(g) else paste0(min(g), "–", max(g))

# per drug: respondents with an answer, outcome as merged categories, score = weighted mean answer
drug_setup <- function(drug, min_n) {
  x <- as.numeric(d[[paste0("beleid_voorkeur_", drug)]])
  keep <- !is.na(x)
  groups <- merge_options(x[keep], min_n)
  map_cat <- rep(seq_along(groups), lengths(groups))[order(unlist(groups))]
  labels <- map_chr(groups, category_label)
  scores <- map_dbl(groups, \(g) {
    sel <- keep & x %in% g
    sum(x[sel] * d$w[sel]) / sum(d$w[sel])
  })
  list(drug = drug, rows = which(keep), cat = map_cat[x[keep]], labels = labels, scores = scores,
       groups = groups)
}

setups <- map(names(drug_labels), \(dr) drug_setup(dr, min_n)) |> set_names(names(drug_labels))

# merged category per respondent (NA = no answer), so it travels along when resampling
for (dr in names(drug_labels)) {
  d[[paste0("cat_", dr)]] <- NA_integer_
  d[[paste0("cat_", dr)]][setups[[dr]]$rows] <- setups[[dr]]$cat
}


## Polarisation measures on an ordered distribution p

leik_d <- function(p) {
  K <- length(p)
  cdf <- cumsum(p)[-K]
  2 * sum(pmin(cdf, 1 - cdf)) / (K - 1)
}

triples <- map(3:9, \(K) combn(K, 3)) |> set_names(3:9)

# Van der Eijk (2001) agreement: 1 = one peak, 0 = uniform, -1 = two peaks at the ends
agreement <- function(p) {
  K <- length(p)
  tr <- triples[[as.character(K)]]
  rest <- p / sum(p)
  A <- 0
  while (sum(rest) > 1e-9) {
    nz <- rest > 1e-9
    m <- min(rest[nz])
    S <- sum(nz)
    b <- matrix(as.integer(nz)[tr], nrow = 3)
    tu <- sum((b[1, ] == 1 & b[2, ] == 1 & b[3, ] == 0) | (b[1, ] == 0 & b[2, ] == 1 & b[3, ] == 1))
    tdu <- sum(b[1, ] == 1 & b[2, ] == 0 & b[3, ] == 1)
    U <- if (tu + tdu == 0) 1 else ((K - 2) * tu - (K - 1) * tdu) / ((K - 2) * (tu + tdu))
    A <- A + m * S * U * (1 - (S - 1) / (K - 1))
    rest[nz] <- rest[nz] - m
  }
  A
}

stopifnot(abs(leik_d(c(0.5, 0, 0, 0.5)) - 1) < 1e-9, leik_d(c(0, 1, 0)) == 0,
          abs(agreement(c(0.5, 0, 0, 0.5)) + 1) < 1e-9, abs(agreement(rep(0.2, 5))) < 1e-9,
          abs(agreement(c(0, 0, 1, 0)) - 1) < 1e-9)


## Models

formula_main <- function() ~ (sex + age + edu + urb3 + vote) * inv
formula_additive <- function() ~ sex + age + edu + urb3 + vote + inv
formula_spline <- function(item) {
  k <- spline_knots[[item]]
  as.formula(sprintf("~ (sex + age + edu + urb3 + vote) * ns(inv, knots = c(%s), Boundary.knots = c(%s))",
                     paste(k$knots, collapse = ", "), paste(k$boundary, collapse = ", ")))
}
formula_cells <- function() ~ (sex + age + edu + urb3 + vote) * inv + age:vote + age:vote:inv

design <- function(form, data) model.matrix(form, data)[, -1, drop = FALSE]

# ridge penalty per model, chosen once by 10-fold CV on the full sample, then fixed
choose_lambda <- function(form, data, y) {
  cv <- glmnet::cv.glmnet(design(form, data), y, family = "multinomial", alpha = 0,
                          weights = data$w, nfolds = 10)
  cv$lambda.min
}

fit_ridge <- function(form, data, y, lambda, weights = data$w) {
  path <- exp(seq(log(lambda * 100), log(lambda), length.out = 20))
  fit <- glmnet::glmnet(design(form, data), y, family = "multinomial", alpha = 0, weights = weights,
                        lambda = path)
  list(fit = fit, form = form, lambda = lambda, type = "ridge")
}

fit_multinom <- function(form, data, y, weights = data$w) {
  dd <- data
  dd$y <- y
  dd$.wt <- weights
  fit <- nnet::multinom(update(form, y ~ .), data = dd, weights = .wt, trace = FALSE, maxit = 1000,
                        MaxNWts = 5000)
  list(fit = fit, form = form, type = "multinom", converged = fit$convergence == 0)
}

predict_probs <- function(m, data) {
  if (m$type == "ridge") {
    predict(m$fit, design(m$form, data), s = m$lambda, type = "response")[, , 1]
  } else {
    predict(m$fit, data, type = "probs")
  }
}

# weighted mean predicted distribution with the variables in `set` fixed (g-computation)
predict_dist <- function(m, data, set) {
  for (v in names(set)) {
    data[[v]] <- if (is.factor(data[[v]])) factor(set[[v]], levels = levels(data[[v]])) else set[[v]]
  }
  p <- predict_probs(m, data)
  colSums(p * data$w) / sum(data$w)
}

# analysis data for one drug x item from a (resampled/permuted) respondent table; y = merged category
drug_item_data <- function(dat, drug, item) {
  keep <- !is.na(dat[[paste0("cat_", drug)]])
  dd <- dat[keep, c("w", demo_vars, item)]
  names(dd)[names(dd) == item] <- "inv"
  dd$y <- factor(dat[[paste0("cat_", drug)]][keep], levels = seq_along(setups[[drug]]$labels))
  dd
}


## Statistics for one drug x item

stats_main <- function(m, dd, setup, item) {
  q <- inv_q[[item]]
  out <- c()
  dist <- list()
  for (i in seq_len(nrow(contrasts))) {
    cn <- contrasts[i, ]
    for (lv in c(cn$first, cn$second)) {
      p_lo <- predict_dist(m, dd, setNames(list(lv, q[["p25"]]), c(cn$var, "inv")))
      p_hi <- predict_dist(m, dd, setNames(list(lv, q[["p75"]]), c(cn$var, "inv")))
      g <- paste0(cn$var, ":", lv)
      dist[[g]] <- list(lo = p_lo, hi = p_hi)
      out[paste("group", g, c("D_lo", "D_hi", "dD", "A_lo", "A_hi", "dA", "M_lo", "M_hi"), sep = "|")] <-
        c(leik_d(p_lo), leik_d(p_hi), leik_d(p_hi) - leik_d(p_lo),
          agreement(p_lo), agreement(p_hi), agreement(p_hi) - agreement(p_lo),
          sum(p_lo * setup$scores), sum(p_hi * setup$scores))
    }
    a <- dist[[paste0(cn$var, ":", cn$first)]]
    b <- dist[[paste0(cn$var, ":", cn$second)]]
    gap_lo <- sum((b$lo - a$lo) * setup$scores)
    gap_hi <- sum((b$hi - a$hi) * setup$scores)
    dD_diff <- (leik_d(b$hi) - leik_d(b$lo)) - (leik_d(a$hi) - leik_d(a$lo))
    out[paste("contrast", cn$var, c("gap_lo", "gap_hi", "dgap", "dD_diff"), sep = "|")] <-
      c(gap_lo, gap_hi, gap_hi - gap_lo, dD_diff)
  }
  p_lo <- predict_dist(m, dd, list(inv = q[["p25"]]))
  p_hi <- predict_dist(m, dd, list(inv = q[["p75"]]))
  out[paste("whole", "all", c("D_lo", "D_hi", "dD", "A_lo", "A_hi", "dA"), sep = "|")] <-
    c(leik_d(p_lo), leik_d(p_hi), leik_d(p_hi) - leik_d(p_lo),
      agreement(p_lo), agreement(p_hi), agreement(p_hi) - agreement(p_lo))
  out
}

stats_profiles <- function(m, dd, setup, item) {
  q <- inv_q[[item]]
  out <- c()
  for (nm in names(profiles)) {
    for (lvl in c("lo", "hi")) {
      p <- predict_dist(m, dd, c(profiles[[nm]], list(inv = q[[if (lvl == "lo") "p25" else "p75"]])))
      out[paste("profile", nm, paste0("p_", lvl, "_", setup$labels), sep = "|")] <- p
      out[paste("profile", nm, paste0(c("D_", "A_"), lvl), sep = "|")] <- c(leik_d(p), agreement(p))
    }
  }
  out
}

stats_spline <- function(m, dd, setup, item) {
  q <- inv_q[[item]]
  out <- c()
  for (i in seq_len(nrow(contrasts))) {
    cn <- contrasts[i, ]
    for (lv in c(cn$first, cn$second)) {
      for (pp in names(q)) {
        p <- predict_dist(m, dd, setNames(list(lv, q[[pp]]), c(cn$var, "inv")))
        out[paste("spline", paste0(cn$var, ":", lv), paste0(c("D_", "A_", "M_"), pp), sep = "|")] <-
          c(leik_d(p), agreement(p), sum(p * setup$scores))
      }
    }
  }
  out
}

# young/old x left/right cells, among left and right voters only
stats_cells <- function(m, dd, item) {
  q <- inv_q[[item]]
  out <- c()
  for (a in c("18–49", "50+")) {
    for (v in c("left", "right")) {
      p_lo <- predict_dist(m, dd, list(age = a, vote = v, inv = q[["p25"]]))
      p_hi <- predict_dist(m, dd, list(age = a, vote = v, inv = q[["p75"]]))
      out[paste("cell", paste(a, v), c("D_lo", "D_hi", "dD", "A_lo", "A_hi", "dA"), sep = "|")] <-
        c(leik_d(p_lo), leik_d(p_hi), leik_d(p_hi) - leik_d(p_lo),
          agreement(p_lo), agreement(p_hi), agreement(p_hi) - agreement(p_lo))
    }
  }
  out
}

cells_data <- function(dd) {
  dd <- dd[dd$vote %in% c("left", "right"), ]
  dd$vote <- droplevels(dd$vote)
  dd
}


## Penalties (fixed for all resamples)

lambdas <- expand_grid(drug = names(drug_labels), item = names(item_labels))
message(format(Sys.time()), " choosing ridge penalties by cross-validation")
lambdas <- lambdas |>
  mutate(lams = pmap(list(drug, item), \(dr, it) {
    dd <- drug_item_data(d, dr, it)
    dc <- cells_data(dd)
    c(main = choose_lambda(formula_main(), dd, dd$y),
      additive = choose_lambda(formula_additive(), dd, dd$y),
      spline = choose_lambda(formula_spline(it), dd, dd$y),
      cells = choose_lambda(formula_cells(), dc, dc$y))
  })) |>
  unnest_wider(lams)
lam <- \(dr, it, type) lambdas[[type]][lambdas$drug == dr & lambdas$item == it]


## One full run over all drugs x items on a respondent table

# mode: "point" and "boot" (everything), "perm" (main + cells), "sim" (main contrasts, simulated y)
run_all <- function(dat, mode, sim_y = NULL) {
  out <- c()
  for (dr in names(drug_labels)) {
    s <- setups[[dr]]
    for (it in names(item_labels)) {
      dd <- drug_item_data(dat, dr, it)
      if (!is.null(sim_y)) dd$y <- factor(sim_y[[dr]][[it]], levels = seq_along(s$labels))
      m <- fit_ridge(formula_main(), dd, dd$y, lam(dr, it, "main"))
      res <- stats_main(m, dd, s, it)
      if (mode == "sim") res <- res[str_starts(names(res), "contrast")]
      if (mode %in% c("point", "boot", "perm")) {
        dc <- cells_data(dd)
        mc <- fit_ridge(formula_cells(), dc, dc$y, lam(dr, it, "cells"))
        res <- c(res, stats_cells(mc, dc, it))
      }
      if (mode %in% c("point", "boot")) {
        ms <- fit_ridge(formula_spline(it), dd, dd$y, lam(dr, it, "spline"))
        res <- c(res, stats_profiles(m, dd, s, it), stats_spline(ms, dd, s, it))
      }
      out <- c(out, setNames(res, paste(dr, it, names(res), sep = "|")))
    }
  }
  out
}

# permute all involvement items jointly within cells of the five demographics
cell_id <- interaction(d$sex, d$age, d$edu, d$urb3, d$vote, drop = TRUE)
permute_involvement <- function(dat) {
  idx <- seq_len(nrow(dat))
  perm <- unsplit(lapply(split(idx, cell_id), \(i) if (length(i) > 1) sample(i) else i), cell_id)
  dat[, names(item_labels)] <- dat[perm, names(item_labels)]
  dat
}

message(format(Sys.time()), " point estimates")
point <- run_all(d, "point")
point_perm <- run_all(d, "perm")
point_sim <- point[names(point) %in% names(run_all(d, "sim"))]

# additive model per drug x item: predicted probabilities for simulating outcomes under N1
additive_probs <- map(names(drug_labels), \(dr) {
  s <- setups[[dr]]
  map(names(item_labels), \(it) {
    dd <- drug_item_data(d, dr, it)
    m <- fit_ridge(formula_additive(), dd, dd$y, lam(dr, it, "additive"))
    predict_probs(m, dd)
  }) |>
    set_names(names(item_labels))
}) |>
  set_names(names(drug_labels))

draw_safely <- function(f, template) {
  tryCatch({
    r <- f()
    stopifnot(identical(names(r), names(template)))
    r
  }, error = function(e) setNames(rep(NA_real_, length(template)), names(template)))
}

plan(multisession, workers = n_cores)
opts <- furrr_options(seed = TRUE, packages = c("glmnet", "splines", "stringr"))

message(format(Sys.time()), sprintf(" bootstrap (%d draws)", n_boot))
boot <- future_map(seq_len(n_boot), \(b) draw_safely(\() run_all(d[sample(nrow(d), replace = TRUE), ], "boot"),
                                                      point),
                   .options = opts) |>
  do.call(what = rbind)

message(format(Sys.time()), sprintf(" permutations (%d)", n_perm))
perm <- future_map(seq_len(n_perm), \(b) draw_safely(\() run_all(permute_involvement(d), "perm"), point_perm),
                   .options = opts) |>
  do.call(what = rbind)

message(format(Sys.time()), sprintf(" simulations under the additive model (%d)", n_sim))
sim <- future_map(seq_len(n_sim), \(b) draw_safely(\() {
  sim_y <- map(additive_probs, \(pr_drug) map(pr_drug, \(pr) {
    u <- runif(nrow(pr))
    rowSums(u > t(apply(pr, 1, cumsum))) + 1
  }))
  run_all(d, "sim", sim_y)
}, point_sim), .options = opts) |>
  do.call(what = rbind)

plan(sequential)


## Robustness (point estimates)

robust_run <- function(variant) {
  out <- c()
  for (dr in names(drug_labels)) {
    s <- if (variant == "threshold20") drug_setup(dr, 20) else setups[[dr]]
    for (it in names(item_labels)) {
      dd <- drug_item_data(d, dr, it)
      y <- factor(s$cat, levels = seq_along(s$labels))
      if (variant == "regimes") {
        x <- as.numeric(d[[paste0("beleid_voorkeur_", dr)]])[s$rows]
        y <- factor(cut(x, c(0, 1, 4, 6, 9), labels = FALSE), levels = 1:4)
        s$scores <- 1:4
      }
      m <- switch(variant,
                  unweighted = fit_ridge(formula_main(), dd, y, lam(dr, it, "main"), weights = rep(1, nrow(dd))),
                  unpenalised = fit_multinom(formula_main(), dd, y),
                  regimes = fit_multinom(formula_main(), dd, y),
                  threshold20 = fit_ridge(formula_main(), dd, y, choose_lambda(formula_main(), dd, y)))
      if (variant == "unweighted") dd$w <- 1
      res <- stats_main(m, dd, s, it)
      if (m$type == "multinom") res["converged|all|converged"] <- m$converged
      out <- c(out, setNames(res, paste(dr, it, names(res), sep = "|")))
    }
  }
  out
}

message(format(Sys.time()), " robustness")
robust <- map(c("unweighted", "unpenalised", "regimes", "threshold20"), robust_run) |>
  set_names(c("unweighted", "unpenalised", "regimes", "threshold20"))


## Descriptive: raw distributions in the 16 cells of age x edu x urb x vote, by involvement half

raw_cells <- map(names(drug_labels), \(dr) {
  s <- setups[[dr]]
  map(names(item_labels), \(it) {
    dd <- drug_item_data(d, dr, it) |>
      mutate(cat = as.integer(y)) |>
      filter(urb3 != "unknown", vote != "other/none")
    med <- weighted_quantile(d[[it]], d$w, 0.5)
    dd |>
      mutate(half = if_else(inv <= med, "bottom", "top"),
             cell = paste(age, edu, urb3, vote, sep = ", ")) |>
      summarise(n = n(), dist = list(map_dbl(seq_along(s$labels), \(k) sum(w[cat == k]) / sum(w))),
                .by = c(cell, half)) |>
      mutate(D = map_dbl(dist, leik_d), A = map_dbl(dist, agreement), drug = dr, item = it)
  }) |>
    list_rbind()
}) |>
  list_rbind()


## Save

meta <- list(n_boot = n_boot, n_perm = n_perm, n_sim = n_sim, min_n = min_n, inv_q = inv_q,
             contrasts = contrasts, profiles = profiles, drug_labels = drug_labels, item_labels = item_labels,
             merges = map(setups, \(s) s$labels), scores = map(setups, \(s) s$scores), lambdas = lambdas,
             n_respondents = nrow(d), runtime_min = as.numeric(difftime(Sys.time(), t_start, units = "mins")))

saveRDS(list(point = point, point_perm = point_perm, point_sim = point_sim, boot = boot, perm = perm,
             sim = sim, robust = robust, raw_cells = raw_cells, meta = meta),
        file.path(out_dir, "moderation_involvement.rds"))
writeLines(capture.output(sessionInfo()), file.path(out_dir, "sessionInfo.txt"))
message(sprintf("done in %.1f min; failed draws: boot %d, perm %d, sim %d", meta$runtime_min,
                sum(is.na(boot[, 1])), sum(is.na(perm[, 1])), sum(is.na(sim[, 1]))))
