library(tidyverse)

source(here::here("analysis", "R", "00_load_and_clean.R"))
source(here::here("analysis", "R", "utils", "party_logos.R"))
source(here::here("analysis", "R", "utils", "party_map_scores.R"))

# Figure 4a: every party, with its logo, by mean preferred policy for the illicit drugs (x) and for
# alcohol and tobacco (y); scores from utils/party_map_scores.R. Dashed lines at the sample means split
# the map into four quadrants. Output: out/fig_party_map.pdf and out/fig_party_map.tex (panel b,
# figures/fig_regulation_quadrants.pdf, is drawn by hand; the panel letters are set in LaTeX).
# Variants of this map (other drug pairs, medians, modes, middle options): exploratory/party_map_variants.R

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

# all parties with respondents are on the map
stopifnot(setequal(party_means$party, setdiff(na.omit(as.character(unique(df$vote_recall_rec))), excluded)))

# axis ranges with room for the corner labels
pad <- \(r) r + c(-1, 1) * 0.16 * diff(r)
xlim <- pad(range(c(party_means$x, ref[["x"]])))
ylim <- pad(range(c(party_means$y, ref[["y"]])))

# logos moved apart on the panel (width, height in mm), with a line from the party's own position to its
# logo when it moved
logo_scale <- 0.5
data <- repel_logos(party_means, xlim, ylim, panel_mm = c(5.45, 3.95) * 25.4, scale = logo_scale)
moved <- filter(data, abs(x - x0) / diff(xlim) * 5.45 * 25.4 > 1 | abs(y - y0) / diff(ylim) * 3.95 * 25.4 > 1)

# tinted quadrants, each named in its outer corner
quad <- quadrants |>
  mutate(xmin = if_else(qx == "left", xlim[1], ref[["x"]]), xmax = if_else(qx == "left", ref[["x"]], xlim[2]),
         ymin = if_else(qy == "bottom", ylim[1], ref[["y"]]), ymax = if_else(qy == "bottom", ref[["y"]], ylim[2]),
         lx = if_else(qx == "left", xlim[1] + 0.01 * diff(xlim), xlim[2] - 0.01 * diff(xlim)),
         ly = if_else(qy == "bottom", ylim[1] + 0.015 * diff(ylim), ylim[2] - 0.015 * diff(ylim)),
         hjust = if_else(qx == "left", 0, 1), vjust = if_else(qy == "bottom", 0, 1))

p_map <- ggplot(data, aes(x, y)) +
  geom_rect(data = quad, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = quad$fill,
            alpha = 0.1, inherit.aes = FALSE) +
  geom_text(data = quad, aes(lx, ly, label = label, hjust = hjust, vjust = vjust),
            colour = colorspace::darken(quad$fill, 0.35), fontface = "bold", size = 2.6, inherit.aes = FALSE) +
  geom_hline(yintercept = ref[["y"]], linetype = "dashed", linewidth = 0.3, colour = "grey50") +
  geom_vline(xintercept = ref[["x"]], linetype = "dashed", linewidth = 0.3, colour = "grey50") +
  geom_segment(data = moved, aes(x0, y0, xend = x, yend = y), colour = "grey55", linewidth = 0.25) +
  annotation_custom(logo_grob(data, xlim, ylim, logo_scale)) +
  geom_point(data = filter(data, !party %in% names(logos)), size = 2.5, colour = "#3b528b") +
  scale_x_continuous(breaks = 1:9, labels = option_ticks, limits = xlim, oob = scales::oob_keep) +
  scale_y_continuous(breaks = 1:9, labels = option_ticks, limits = ylim, oob = scales::oob_keep) +
  coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
  labs(x = "Mean preferred policy: illicit drugs", y = "Mean preferred policy:\nalcohol and tobacco") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size = 8),
        axis.text = element_text(size = 7, lineheight = 0.9),
        plot.margin = margin(4, 8, 4, 4))

ggsave(here::here("out", "fig_party_map.pdf"), p_map, width = 6.5, height = 4.6, device = cairo_pdf)

caption <- paste0(
  r"((a) Parties by mean preferred policy for the illicit drugs (horizontal) and for alcohol and tobacco )",
  r"((vertical), from ban (1) to free sale (9). Per respondent the mean answer over the substances answered, )",
  r"(then the weighted mean per party: horizontal over cannabis, psychedelics, MDMA, speed, cocaine and heroin, )",
  r"(vertical over alcohol and tobacco. Dashed lines are the sample means and split the map into four )",
  r"(quadrants: more liberal than average on the legal drugs only (double standards, the status quo), on both )",
  r"((liberals), on neither (abstainers) or on the illicit drugs only (reformers). )",
  sprintf(r"(Logo size reflects $n$ ($n = %d$ in total); DENK ($n = 1$), other parties, blank and )",
          sum(party_means$n)),
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
