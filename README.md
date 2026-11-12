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
    03_fig_party_map.R         Figure 4 (a: party map with standard-deviation ellipses; b: every party's distance
                               from the sample mean on the stylised U-curve of harms)
    07_party_within_between.R  Results: within- and between-party model of the party-map scores (Table 3)
    A1_weights.R               Appendix: weighting diagnostics
    A2_attitudes.R             Appendix: correlations between policy and the attitude items
    utils/                     helpers sourced by other scripts (party logos, party-map scores,
                               the descriptive report's helpers)
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
| Figure 4: (a) party map with standard-deviation ellipses, (b) every party's distance from the sample mean on the U-curve | `out/fig_party_map.tex/.pdf`, `out/fig_party_map_b.pdf` | `03_fig_party_map.R` |
| Results: within- and between-party model (table and numbers in the text) | `out/tab_party_within_between.tex`, `out/party_within_between_numbers.tex` | `07_party_within_between.R` |
| Methods: numbers in the text (macros) | `out/methods_numbers.tex` | `04_methods_numbers.R` |
| Methods: sample composition table | `out/tab_sample_composition.tex` | `05_tab_sample_composition.R` |
| Appendix: Figure 3, detailed categories | `out/fig_gradient_grid_detailed.tex/.pdf` | `02_fig_gradient_grid.R` |
| Appendix: attitude correlations | `out/tab_attitude_correlations.tex` | `A2_attitudes.R` |

The `.tex` snippets contain the figure environment with its caption. Numbers in captions (sample sizes
etc.) are computed in R, so edit captions in the R script, not in `out/`. After rerunning a script,
commit the changed files in `out/` and Overleaf picks them up.

## Reproducing

1. Put the data files in `analysis/data/` (see below).
2. Open `drug_policy_spectrum.Rproj` and run `01`–`05`, `07`, `A1` and `A2` (in any order).

Packages for the paper: `tidyverse`, `haven`, `readxl`, `here`, `survey`, `patchwork`, `png`, `rsvg`, `colorspace`, `ggrepel`, `lmerTest`,
`viridisLite`, `scales`, `RColorBrewer`, `knitr` and `sf` (the COROP map in `A1_weights.R`; it downloads
the CBS boundaries to `analysis/data/coropgebied.geojson` if missing).

## Data

The repository is public, so the survey data are **never committed** (`.gitignore` excludes
`analysis/data/`, `*.sav`, `*.rds` (fitted models contain the data) and csv files under `analysis/`
other than the hand ratings in `analysis/ratings/`). Ask Timo van Ommeren for
`NLOZ26-10_Drugsbeleid_gedeeld_261022.sav`; the paper scripts and the descriptive report both read only
this file. The variable codebook is in `analysis/assets/`.

Conventions used throughout (set in `00_load_and_clean.R`, the only place where data are cleaned or
variables derived, for the paper, the exploratory scripts and the descriptive report alike):

- `df`: the completed interviews that carry a survey weight (n = 1239), with the export's variable
  names and codes (`beleid_voorkeur_<drug>` 1 = ban … 9 = free sale; `gebruik_1–4` 1 = never … 4 = often).
- 99 ("geen mening") is set to NA on every item; on the 0–100 involvement sliders (`betrokkenheid_*`)
  99 is a position and is kept.
- Question wording: the `.sav` variable labels. Answer labels: the agency's codebook
  (`analysis/assets/`), put on the columns, so `haven::as_factor()` uses them.
- Derived variables: `<item>_scored` (belief and behaviour items reversed where agreeing favours
  prohibition, so high = liberal), `betrokkenheid_mean`, `substance_ladder`, `mdma_support`, the emotion
  binaries `emo_<topic>_<feeling>` and their combination per topic `emo_<topic>`, `vote_lr`, `vote_3`, and
  the groupings of Figure 3, `age_group`, `edu_group` and `urb_group` (also the report's split buttons).
- `w`: the analysis weight, Kieskompas's iterative post-stratification weight trimmed at the 99th
  percentile (`weight_trimmed_0990`); `des` is the matching `survey` design. Everything is weighted;
  anything unweighted says so.

## Exploratory analyses

`analysis/R/exploratory/` holds analyses that informed the paper but are not in it. They write to
`analysis/output/` (not tracked) and may need extra packages.

| Script | What it does | Extra packages |
|---|---|---|
| `raw_distributions.R` | Raw answer distributions per drug × demographic, per party, by involvement, and the belief items per party | |
| `party_map_variants.R` | Variants of the party map: other drug pairs, medians, modes, support for the middle options | |
| `party_map_sd.R` | Figure 4a with each party's spread: all parties on one map at their weighted mean, with a weighted standard-deviation ellipse of their voters (`sd_mult` sets the size), coloured by bloc; the paper version is `03_fig_party_map.R` | `ggrepel` |
| `party_distributions.R` | How each party's voters spread over the party map: one panel per party with the regions holding 50% and 80% of its voters (weighted kernel density), the sample's 50% region and the sample means for reference | |
| `respondent_quadrants.R` | Figure 4a with respondents instead of parties: every respondent on the illicit × legal map, dashed lines at the weighted means, weighted quadrant shares | |
| `respondent_quadrants_interactive.R` | Interactive version (HTML): choose the axes (x: cannabis, MDMA, soft drugs, hard drugs or all illicit drugs; y: alcohol, tobacco or both), split into panels by the descriptive report's splits or party, colour or split by any attitude item, involvement slider, own use, the manual groups (two rules at the top of their block in the script: alcohol and tobacco, and cannabis, psychedelics and MDMA) or a latent class/profile solution (reads the saved fits of `latent/lca.R` and `latent/lpa_profiles.R`, run those first); drag a box over dots to list those respondents (demographics, vote, own use, involvement, their nine policy answers) with the group's weighted profile (categories 10+ points over or under the whole map marked). Fills `respondent_quadrants_template.html` with the data; the HTML contains respondent-level data, so it is written to `analysis/output/` and never committed or shared | `jsonlite` |
| `factor_model.R` | Ordinal factor models: does a licit factor separate from an illicit factor at the respondent level? | `lavaan`, `semPlot` |
| `latent/lca.R`, `latent/lpa_profiles.R` | Latent class and latent profile analyses of the policy preferences | `poLCA`, `tidyLPA`, `mclust` |
| `latent/lpa_profile_items.R` | The four profiles of the chosen LPA solution (model 1, 4 profiles) across every questionnaire item in survey order: one line per profile, one panel per block (PDF), and as an HTML page with the blocks stacked, a button per profile and a reverse-scoring button (aggregates only). Reads the fits of `lpa_profiles.R` | `patchwork` |
| `modality/` | How many camps (modes) a party's answers have, per drug: hand-rating apps, models and camp maps (see below) | `shiny`, `nnet`, `marginaleffects`, `mclust`, `remode`, `irr`, `ggrepel` |

**Modality workflow.** `modality_rater.R` and `drug_mode_rater.R` are small Shiny apps that show party ×
drug histograms in random order. You mark their modality or local maxima, and the ratings are saved to
`analysis/ratings/` (tracked, since they are manual work). `modality_by_party.R`,
`party_map_drug_modes.R` and `party_map_pair_modes.R` build on these ratings, and `party_map_modes.R`
compares them with automated checks (`remode`).

## Descriptive report

`analysis/reports/descriptive_plots/descriptive_plots.Rmd` is an interactive HTML report on the main
study: every item per demographic split, involvement × bimodality, and within-block correlations and
PCA. Render it from the project root:

```r
rmarkdown::render("analysis/reports/descriptive_plots/descriptive_plots.Rmd")
```

The HTML lands next to the Rmd and is not tracked. It reads `df` from `analysis/R/00_load_and_clean.R`
and its helper functions from `analysis/R/utils/report_helpers.R`, and is weighted throughout:
counts are weighted counts and every *n* beside them is the unweighted number of respondents.

| File | What it holds |
|---|---|
| `descriptive_plots.Rmd` | the report shell: setup, styling, the three tabs, final checks |
| `report.Rmd` | tab 1, descriptive charts per item (with the code behind the counts cube) |
| `involvement.Rmd` | tab 2, involvement × bimodality (ReMoDe on weighted counts rounded to whole respondents) |
| `analyses.Rmd` | tab 3, weighted within-block correlations and PCA per block |
| `browsers.js` | the two in-browser chart viewers, inlined into the HTML |

Each tab's own analysis code sits in hidden chunks (`include=FALSE`) inside that tab's Rmd. The knitr
cache folder `descriptive_plots_cache/` (not tracked) also holds the ReMoDe stability cache. Extra
packages: `remode`, `psych` (with `GPArotation`), `jsonlite`, `svglite`, `sessioninfo`, `rmarkdown`,
`readxl`.

