library(tidyverse)
library(ggrepel)

source(here::here("analysis", "R", "00_load_and_clean.R"))
source(here::here("analysis", "R", "utils", "party_map_scores.R"))

# Figure 4a: every party by the weighted mean preferred policy of its voters for the illicit drugs (x) and for
# alcohol and tobacco (y), with an ellipse of sd_mult weighted standard deviations of its voters: the weighted
# covariance of x and y, so the ellipse's width and height are sd_mult standard deviations either side of the
# mean on each axis, and its tilt is the correlation of the two scores within the party. Scores from
# utils/party_map_scores.R. Dashed lines at the sample means split the map into four quadrants. Colour: the
# party's bloc.
# Figure 4b: the same party means on a stylised U-curve of harms by regulatory regime (after the Transform Drug
# Policy Foundation), drawn over the same 1-9 scale: each party sits on the curve twice, at its mean for the
# illicit drugs and for alcohol and tobacco, so its height is the harm the stylised curve implies for that regime.
# Six largest parties. Each arrow starts on the U at the status quo (current Dutch policy, coded below) and runs
# horizontally to the party mean; width = number of voters. The curve and the regime labels are illustrative.
# Output: out/fig_party_map.pdf (a), out/fig_party_map_b.pdf (b) and out/fig_party_map.tex (panel letters in LaTeX).
# Variants of this map (logos, other drug pairs, medians, modes, middle options): exploratory/party_map_variants.R

sd_mult <- 0.2   # ellipse size in standard deviations of the party's voters; the caption follows

# tick labels: the answer options (as in figure 2) with their number
option_ticks <- c("Ban (1)", "Research only (2)", "Treatment only (3)", "Prescription only (4)",
                  "Specialist shop,\nnon-profit (5)", "Specialist shop,\nfor-profit (6)",
                  "Sold everywhere,\nno advertising (7)", "Sold everywhere,\nwith advertising (8)", "Free sale (9)")

# quadrants around the sample means: liberal on the legal drugs (top) or not, on the illicit drugs (right) or not
quadrants <- tibble(
  qx = c("left", "right", "left", "right"), qy = c("top", "top", "bottom", "bottom"),
  label = c("Double standards (status quo)", "Liberals", "Abstainers", "Reformers"),
  fill = c("#E69F00", "#009E73", "#56B4E9", "#CC79A7")
)
bloc_colours <- c(Left = "#b2182b", Centre = "#6a3d9a", Right = "#08306b")   # dark, to stand out on the tints

# all parties with respondents are on the map
stopifnot(setequal(party_means$party, setdiff(na.omit(as.character(unique(df$vote_recall_rec))), excluded)))

# per party: weighted mean and covariance of its voters' two scores, and the ellipse (the unit circle through
# the covariance's Cholesky factor, times sd_mult)
bloc <- df |> distinct(party = as.character(vote_recall_rec), bloc = vote_3) |> drop_na()
parties <- resp |>
  summarise(n = n(), cov = list(cov.wt(cbind(x, y), w)$cov), x = weighted.mean(x, w), y = weighted.mean(y, w),
            .by = party) |>
  left_join(bloc, by = "party") |>
  mutate(bloc = factor(bloc, c("left", "middle", "right"), names(bloc_colours)))
circle <- cbind(cos(seq(0, 2 * pi, length.out = 121)), sin(seq(0, 2 * pi, length.out = 121)))
ellipses <- parties |>
  mutate(pts = pmap(list(x, y, cov), \(x, y, cov) {
    e <- sd_mult * circle %*% chol(cov)
    tibble(ex = x + e[, 1], ey = y + e[, 2])
  })) |>
  select(party, bloc, pts) |>
  unnest(pts)

# axis ranges: the ellipses, with room for the corner labels
pad <- \(r) r + c(-1, 1) * 0.16 * diff(r)
xlim <- pad(range(c(ellipses$ex, ref[["x"]])))
ylim <- pad(range(c(ellipses$ey, ref[["y"]])))

# tinted quadrants, each named in its outer corner
quad <- quadrants |>
  mutate(xmin = if_else(qx == "left", xlim[1], ref[["x"]]), xmax = if_else(qx == "left", ref[["x"]], xlim[2]),
         ymin = if_else(qy == "bottom", ylim[1], ref[["y"]]), ymax = if_else(qy == "bottom", ref[["y"]], ylim[2]),
         lx = if_else(qx == "left", xlim[1] + 0.01 * diff(xlim), xlim[2] - 0.01 * diff(xlim)),
         ly = if_else(qy == "bottom", ylim[1] + 0.015 * diff(ylim), ylim[2] - 0.015 * diff(ylim)),
         hjust = if_else(qx == "left", 0, 1), vjust = if_else(qy == "bottom", 0, 1))

p_map <- ggplot(parties, aes(x, y, colour = bloc)) +
  geom_rect(data = quad, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = quad$fill,
            alpha = 0.1, inherit.aes = FALSE) +
  geom_text(data = quad, aes(lx, ly, label = label, hjust = hjust, vjust = vjust),
            colour = colorspace::darken(quad$fill, 0.35), fontface = "bold", size = 2.6, inherit.aes = FALSE) +
  geom_hline(yintercept = ref[["y"]], linetype = "dashed", linewidth = 0.3, colour = "grey50") +
  geom_vline(xintercept = ref[["x"]], linetype = "dashed", linewidth = 0.3, colour = "grey50") +
  geom_polygon(data = ellipses, aes(ex, ey, group = party, fill = bloc), alpha = 0.12, linewidth = 0.4) +
  geom_point(aes(size = n)) +
  geom_label_repel(aes(label = party), size = 2.3, fontface = "bold", fill = alpha("white", 0.8), label.size = NA,
                   label.padding = 0.1, min.segment.length = 0, segment.size = 0.2, box.padding = 0.25, seed = 1,
                   show.legend = FALSE) +
  scale_colour_manual(values = bloc_colours, aesthetics = c("colour", "fill"), name = NULL) +
  scale_size_area(max_size = 3, breaks = c(25, 100, 200), name = "Voters (n)") +
  scale_x_continuous(breaks = 1:9, labels = option_ticks) +
  scale_y_continuous(breaks = 1:9, labels = option_ticks) +
  coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
  labs(x = "Mean preferred policy: illicit drugs", y = "Mean preferred policy:\nalcohol and tobacco") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size = 8),
        axis.text = element_text(size = 7, lineheight = 0.9),
        legend.position = "right", legend.key.size = unit(0.4, "cm"),
        plot.margin = margin(4, 8, 4, 4))

ggsave(here::here("out", "fig_party_map.pdf"), p_map, width = 6.5, height = 4.8, device = cairo_pdf)

# panel b: the stylised U, lowest at 5 (specialist shop, non-profit: responsible legal regulation), with a narrow
# rounded bottom so that positions near it still differ in height; on_arm() gives the x on an arm at a height
harm <- \(x) sqrt(((x - 5) / 4)^2 + 0.01)
on_arm <- \(h, side) 5 + side * 4 * sqrt(pmax(h^2 - 0.01, 0))
largest <- c("GroenLinks-PvdA", "D66", "VVD", "PVV", "CDA", "JA21")
min_gap <- 0.03   # smallest height difference between two arrows

# current Dutch policy on the same 1-9 scale (authors' coding; check): cannabis 6 (coffeeshop), psychedelics 6
# (truffles in smartshops), MDMA, speed and cocaine 1 (ban), heroin 3 (heroin-assisted treatment), alcohol 7
# (sold everywhere, advertising restricted), tobacco 6 (no supermarket sales, no advertising). Averaged as the
# party scores: illicit over the six illicit drugs, legal over alcohol and tobacco
status_quo <- c(illicit = mean(c(cannabis = 6, psychedelica = 6, mdma = 1, speed = 1, cocaine = 1, heroine = 3)),
                legal = mean(c(alcohol = 7, tabak_en_vapes = 6)))

# per party and drug group: the arrow starts on the U at the status quo and runs horizontally to the party mean.
# The start points are spread around the status quo, min_gap apart in height, along the curve, so every arrow
# starts on the U; the party heading furthest along the arm starts highest, so arrows do not cross
arrows <- bind_rows(
  transmute(filter(parties, party %in% largest), party, bloc, n, drugs = "Illicit drugs", sq = status_quo[["illicit"]],
            x),
  transmute(filter(parties, party %in% largest), party, bloc, n, drugs = "Alcohol and tobacco",
            sq = status_quo[["legal"]], x = y)) |>
  mutate(side = sign(sq - 5), lane = rank(side * x, ties.method = "first"), .by = drugs) |>
  mutate(h = harm(sq) + min_gap * (lane - (max(lane) + 1) / 2), x0 = on_arm(h, side), x1 = x, .by = drugs)
sq_points <- tibble(drugs = c("illicit drugs", "alcohol and tobacco"), x = status_quo[c("illicit", "legal")], y = harm(x))
regimes <- tibble(x = c(0.95, 3, 5, 7.5, 9.05), hjust = c(0, 0.5, 0.5, 0.5, 1),
                  label = c("Prohibition", "Medical access\n& harm reduction", "Responsible\nlegal regulation",
                            "Lax regulation", "Unrestricted\naccess"))

p_curve <- ggplot() +
  geom_line(data = tibble(x = seq(1, 9, 0.01), y = harm(x)), aes(x, y), colour = "grey35", linewidth = 0.6) +
  geom_segment(data = sq_points, aes(x = x, xend = x, y = -0.06, yend = y), linetype = "dashed", colour = "grey55",
               linewidth = 0.3) +
  geom_text(data = sq_points, aes(x, -0.09, label = paste("Status quo,", drugs)), size = 2.3, colour = "grey30") +
  geom_segment(data = arrows, aes(x = x0, xend = x1, y = h, yend = h, colour = bloc, linewidth = n),
               arrow = arrow(length = unit(1.6, "mm"), type = "closed")) +
  geom_point(data = arrows, aes(x0, h, colour = bloc), size = 1.2) +
  geom_text(data = arrows, aes(x1 + sign(x1 - x0) * 0.08, h, label = party, colour = bloc,
                               hjust = if_else(x1 > x0, 0, 1)), size = 2.3, fontface = "bold", show.legend = FALSE) +
  geom_text(data = regimes, aes(x, 1.05, label = label, hjust = hjust), size = 2.3, lineheight = 0.85,
            colour = "grey30") +
  annotate("text", x = c(1.5, 6.9), y = c(0.9, 0.62), label = c("Illicit drugs", "Alcohol and tobacco"),
           fontface = "bold", size = 2.8, hjust = 0) +
  scale_colour_manual(values = bloc_colours, name = NULL) +
  scale_linewidth_continuous(range = c(0.4, 1.5), limits = c(0, 250), breaks = c(100, 200), name = "Voters (n)") +
  scale_x_continuous(breaks = 1:9, labels = c("Ban (1)", "Research\nonly (2)", "Treatment\nonly (3)",
                                              "Prescription\nonly (4)", "Shop,\nnon-profit (5)", "Shop,\nfor-profit (6)",
                                              "Everywhere,\nno ads (7)", "Everywhere,\nads (8)", "Free sale (9)")) +
  scale_y_continuous(breaks = NULL) +
  coord_cartesian(xlim = c(0.9, 9.1), ylim = c(-0.12, 1.1), expand = FALSE) +
  labs(x = "Mean preferred policy", y = "Harms, social and health (stylised)") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), text = element_text(size = 8),
        axis.text = element_text(size = 6.5, lineheight = 0.9), legend.position = "right",
        legend.key.size = unit(0.4, "cm"), plot.margin = margin(4, 8, 4, 4))

ggsave(here::here("out", "fig_party_map_b.pdf"), p_curve, width = 6.5, height = 4.6, device = cairo_pdf)

caption <- paste0(
  r"((a) Parties by the mean preferred policy of their voters for the illicit drugs (horizontal) and for alcohol )",
  r"(and tobacco (vertical), from ban (1) to free sale (9). Per respondent the mean answer over the substances )",
  r"(answered: horizontal over cannabis, psychedelics, MDMA, speed, cocaine and heroin, vertical over alcohol and )",
  r"(tobacco. Dot: the party's weighted mean; ellipse: )",
  sprintf(r"($\pm %s$ standard deviation of its voters (weighted covariance), scaled down so that the parties )", sd_mult),
  r"(can be told apart: at full size the ellipses overlap heavily. Their tilt shows the correlation of the two )",
  r"(scores within the party. Colour: bloc. Dashed lines are the sample means and split the map into four )",
  r"(quadrants: more liberal than average on the legal drugs only (double standards, the status quo), on both )",
  r"((liberals), on neither (abstainers) or on the illicit drugs only (reformers). )",
  sprintf(r"(Point size reflects $n$ ($n = %d$ in total); DENK ($n = 1$), other parties, blank and )",
          sum(parties$n)),
  r"(non-voters are left out. Vote: TK 2025. )",
  r"((b) The six largest parties on a stylised U-curve of harms by regulatory regime, after the Transform Drug )",
  r"(Policy Foundation, drawn over the same scale as (a). Each arrow starts on the curve at the status quo (dashed: )",
  r"(current Dutch policy coded on the same scale and averaged as the party scores; the starts are spread a little )",
  r"(along the curve) and runs to the party's mean, for the illicit drugs and for alcohol and tobacco; arrow width )",
  r"(reflects the number of voters. The curve and the placement of the regime labels on the scale are illustrative, )",
  r"(not measured.)"
)

writeLines(c(
  r"(\begin{figure}[H])",
  r"(  \centering)",
  r"(  \makebox[\linewidth][l]{\textbf{a}})",
  r"(  \includegraphics[width=\linewidth]{out/fig_party_map.pdf}\\[4pt])",
  r"(  \makebox[\linewidth][l]{\textbf{b}})",
  r"(  \includegraphics[width=\linewidth]{out/fig_party_map_b.pdf})",
  paste0(r"(  \caption{)", caption, "}"),
  r"(  \label{fig:party_map})",
  r"(\end{figure})"
), here::here("out", "fig_party_map.tex"))
