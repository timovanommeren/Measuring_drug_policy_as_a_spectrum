# Measuring drug regulation as a spectrum

Code and manuscript for *Measuring drug regulation as a spectrum: Dutch support for nine models of drug
regulation*. Respondents (Kieskompas panel, October 2026, *n* = 1239, weighted) chose for nine substances
their preferred policy on a nine-point scale from a ban (1) to free sale (9). The paper describes where the
Dutch public stands per substance, how this differs by demographic group, and how voters of each party
position themselves on the illicit versus the legal drugs.

## Repository structure

```
main.tex, references.bib       manuscript (compiled on Overleaf, which syncs with this repo)
out/                           everything R generates for the paper: figures (.pdf) and LaTeX
                               snippets (.tex) that main.tex \input's. Tracked in git.
figures/                       figures made by hand (not by R)
analysis/
  R/
    00_load_and_clean.R        reads the data, recodes, sets the weights and the survey design
    01_fig_policy_preferences.R  Figure 1 (answer scale) and Figure 2 (preferences per substance)
    02_fig_gradient_grid.R     Figure 3 (preferences by demographic) + its detailed appendix version
    03_fig_party_map.R         Figure 4a (party map)
    A1_weights.R               Appendix: weighting diagnostics
    A2_attitudes.R             Appendix: correlations between policy and the attitude items
    utils/                     helpers sourced by other scripts (party logos, party-map scores)
    exploratory/               analyses that are not (yet) in the paper, see below
  reports/
    descriptive_plots/         descriptive report of the main study (item checks, involvement,
                               analyses), see below
  data/                        survey data: NOT in the repo (see "Data")
  ratings/                     hand ratings used by exploratory/modality/ (tracked)
  assets/                      codebook, party logos and the questionnaire (.qsf)
  icons/                       icons of the nine answer options (as shown in the survey)
  output/                      output of the exploratory scripts (not tracked, regenerate locally)
```

Every script sources `00_load_and_clean.R` itself, so each can be run on its own, from the project root
(open `drug_policy_spectrum.Rproj`; paths use `here::here()`).

## What makes what in the paper

| In `main.tex` | File | Script |
|---|---|---|
| Figure 1: answer scale | `out/fig_policy_scale.pdf` | `01_fig_policy_preferences.R` |
| Figure 2: preferred policy per substance | `out/fig_policy_preferences.tex`, `out/policy_preferences_all_drugs.pdf` | `01_fig_policy_preferences.R` |
| Figure 3: preferences by demographic | `out/fig_gradient_grid.tex/.pdf` | `02_fig_gradient_grid.R` |
| Figure 4a: party map | `out/fig_party_map.tex/.pdf` | `03_fig_party_map.R` |
| Figure 4b: quadrants on the U-curve | `figures/fig_regulation_quadrants.pdf` | made by hand |
| Appendix: Figure 3, detailed categories | `out/fig_gradient_grid_detailed.tex/.pdf` | `02_fig_gradient_grid.R` |
| Appendix: attitude correlations | `out/tab_attitude_correlations.tex` | `A2_attitudes.R` |
| Appendix: weighting | `out/fig_weights.tex/.pdf` | `A1_weights.R` |

The `.tex` snippets contain the figure environment with its caption. Numbers in captions (sample sizes
etc.) are computed in R, so edit captions in the R script, not in `out/`. After rerunning a script,
commit the changed files in `out/` and Overleaf picks them up.

## Reproducing

1. Put the data files in `analysis/data/` (see below).
2. Open `drug_policy_spectrum.Rproj` and run `01`–`03`, `A1` and `A2` (in any order).

Packages for the paper: `tidyverse`, `haven`, `here`, `survey`, `patchwork`, `png`, `rsvg`, `colorspace`,
`viridisLite`, `scales`, `RColorBrewer`, `knitr` and `sf` (the COROP map in `A1_weights.R`; it downloads
the CBS boundaries to `analysis/data/coropgebied.geojson` if missing).

## Data

The repository is public, so the survey data are **never committed** (`.gitignore` excludes
`analysis/data/`, `*.sav`, `*.rds` (fitted models contain the data) and csv files under `analysis/`
other than the hand ratings in `analysis/ratings/`). Ask Timo van Ommeren for
`NLOZ26-10_Drugsbeleid_gedeeld_261022.sav` (the paper scripts), and for the descriptive report also
`NLOZ26-10_Drugsbeleid_gedeeld_261022.csv` and `NLOZ26-10_Drugsbeleid_in progress_gedeeld_261022.csv`. The variable codebook is in `analysis/assets/`.

Conventions used throughout (set in `00_load_and_clean.R`):

- `beleid_voorkeur_<drug>`: preferred policy, 1 = ban … 9 = free sale; 99 ("geen mening") is set to `NA`.
- `w`: the analysis weight, Kieskompas's iterative post-stratification weight trimmed at the 99th
  percentile (`weight_trimmed_0990`); `des` is the matching `survey` design.

## Exploratory analyses

`analysis/R/exploratory/` holds analyses that informed the paper but are not in it. They write to
`analysis/output/` (not tracked) and may need extra packages.

| Script | What it does | Extra packages |
|---|---|---|
| `raw_distributions.R` | Raw answer distributions per drug × demographic, per party, by involvement, and the belief items per party | |
| `party_map_variants.R` | Variants of the party map: other drug pairs, medians, modes, support for the middle options | |
| `factor_model.R` | Ordinal factor models: does a licit factor separate from an illicit factor at the respondent level? | `lavaan`, `semPlot` |
| `latent/lca.R`, `latent/lpa_profiles.R` | Latent class and latent profile analyses of the policy preferences | `poLCA`, `tidyLPA`, `mclust` |
| `modality/` | How many camps (modes) a party's answers have, per drug: hand-rating apps, models and camp maps (see below) | `shiny`, `nnet`, `marginaleffects`, `mclust`, `remode`, `irr`, `ggrepel` |

**Modality workflow.** `modality_rater.R` and `drug_mode_rater.R` are small Shiny apps that show party ×
drug histograms in random order. You mark their modality or local maxima, and the ratings are saved to
`analysis/ratings/` (tracked, since they are manual work). `modality_by_party.R`,
`party_map_drug_modes.R` and `party_map_pair_modes.R` build on these ratings, and `party_map_modes.R`
compares them with automated checks (`remode`).

## Descriptive report

`analysis/reports/descriptive_plots/descriptive_plots.Rmd` is an interactive HTML report on the main
study: every item per demographic split, involvement × bimodality, and exploratory analyses (policy
profiles, correlations, PCA, factor analysis, linearity, linear models, response times). Render it
from the project root:

```r
rmarkdown::render("analysis/reports/descriptive_plots/descriptive_plots.Rmd")
```

The HTML lands next to the Rmd and is not tracked. Structure:

| Path | What it holds |
|---|---|
| `descriptive_plots.Rmd` | the report shell: setup, tabs, styling, final checks |
| `sections/` | one child document per section, in report order |
| `R/data.R` | reads the delivery and the QSF and builds `pilot` (one row per completed interview), the item lists and labels |
| `R/plot_helpers.R` | generic helpers (titles, scales, slider anchors, item selection) |
| `R/<analysis>.R` | one file per analysis section (`involvement.R`, `PCA.R`, `policy_classes.R`, ...) |
| `js/` | the two in-browser chart viewers, inlined into the HTML |
| `cache/` | the ReMoDe stability cache (not tracked; recomputed if missing, about a minute) |

The sections were written for the pilot reports, so the dataset is called `pilot` and some
variable names follow the pilots. Extra packages: `remode`, `psych` (with `GPArotation`), `mclust`,
`jsonlite`, `svglite`, `sessioninfo`, `rmarkdown`. The involvement section includes three helper
functions from Hoffstadt et al. (github.com/mfhstdt/involvement_polarization, MIT).
