library(tidyverse)
library(ggrepel)

source(here::here("analysis", "R", "00_load_and_clean.R"))
source(here::here("analysis", "R", "utils", "party_map_scores.R"))

# Figure 4a: every party by the weighted mean preferred policy of its voters for the illicit drugs (x) and for
# alcohol and tobacco (y), with an ellipse of sd_mult weighted standard deviations of its voters: the weighted
# covariance of x and y, so the ellipse's width and height are sd_mult standard deviations either side of the
# mean on each axis, and its tilt is the correlation of the two scores within the party. Scores from
# utils/party_map_scores.R. Dashed lines at the sample means split the map into four quadrants. Colour: the
# party's bloc. Output: out/fig_party_map.pdf and out/fig_party_map.tex (panel b,
# figures/fig_regulation_quadrants.pdf, is drawn by hand; the panel letters are set in LaTeX).
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
  r"((b) The four quadrants on the U-curve of harms by regulatory regime, adapted from the Transform Drug )",
  r"(Policy Foundation: the arrows show the direction of change each quadrant prefers, for the illicit drugs )",
  r"((left) and for alcohol, tobacco and vapes (right).)"
)

writeLines(c(
  r"(\begin{figure}[H])",
  r"(  \centering)",
  r"(  \makebox[\linewidth][l]{\textbf{a}})",
  r"(  \includegraphics[width=\linewidth]{out/fig_party_map.pdf}\\[4pt])",
  r"(  \makebox[\linewidth][l]{\textbf{b}})",
  r"(  \includegraphics[width=0.72\linewidth]{figures/fig_regulation_quadrants.pdf})",
  paste0(r"(  \caption{)", caption, "}"),
  r"(  \label{fig:party_map})",
  r"(\end{figure})"
), here::here("out", "fig_party_map.tex"))
