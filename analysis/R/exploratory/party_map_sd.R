library(tidyverse)
library(ggrepel)   # geom_label_repel

source(here::here("analysis", "R", "00_load_and_clean.R"))
source(here::here("analysis", "R", "utils", "party_map_scores.R"))

# Figure 4a with the spread of each party: every party at its weighted mean preferred policy for the illicit drugs
# (x) and for alcohol and tobacco (y), with the sd_mult standard-deviation ellipse of its voters: the weighted
# covariance of x and y, so the ellipse's width and height are sd_mult standard deviations on each axis either side
# of the mean, and its tilt is the correlation of the two scores within the party. Colour: the party's bloc (left,
# centre, right). Dashed lines: the sample means that split figure 4a into quadrants. Scores from
# utils/party_map_scores.R.
# Output: analysis/output/party_map_sd.pdf

sd_mult <- 0.1   # ellipse size in standard deviations; the caption and the zoom follow

bloc <- df |> distinct(party = as.character(vote_recall_rec), bloc = vote_3) |> drop_na()
party_sd <- resp |>
  summarise(n = n(), mx = weighted.mean(x, w), my = weighted.mean(y, w),
            cov = list(cov.wt(cbind(x, y), w)$cov), .by = party) |>
  left_join(bloc, by = "party") |>
  mutate(bloc = factor(bloc, c("left", "middle", "right"), c("Left", "Centre", "Right")))

# the ellipse: the unit circle through the covariance's square root (its Cholesky factor), times sd_mult
circle <- cbind(cos(seq(0, 2 * pi, length.out = 121)), sin(seq(0, 2 * pi, length.out = 121)))
ellipses <- party_sd |>
  mutate(pts = pmap(list(mx, my, cov), \(mx, my, cov) {
    e <- sd_mult * circle %*% chol(cov)
    tibble(ex = mx + e[, 1], ey = my + e[, 2])
  })) |>
  select(party, bloc, n, pts) |>
  unnest(pts)

p <- ggplot(party_sd, aes(mx, my, colour = bloc)) +
  geom_hline(yintercept = ref[["y"]], linetype = "dashed", linewidth = 0.3, colour = "grey50") +
  geom_vline(xintercept = ref[["x"]], linetype = "dashed", linewidth = 0.3, colour = "grey50") +
  geom_polygon(data = ellipses, aes(ex, ey, group = party, fill = bloc), alpha = 0.15, linewidth = 0.45,
               show.legend = FALSE) +
  geom_point(aes(size = n)) +
  geom_label_repel(aes(label = party), size = 2.6, fontface = "bold", fill = alpha("white", 0.85), label.size = NA,
                   label.padding = 0.12, min.segment.length = 0, segment.size = 0.2, box.padding = 0.3, seed = 1,
                   show.legend = FALSE) +
  scale_colour_manual(values = c(Left = "#CC79A7", Centre = "#E69F00", Right = "#0072B2"), name = NULL,
                      aesthetics = c("colour", "fill")) +
  scale_size_area(max_size = 4, breaks = c(25, 100, 200), name = "Voters (n)") +
  scale_x_continuous(breaks = 1:9) +
  scale_y_continuous(breaks = 1:9) +
  coord_fixed(xlim = range(ellipses$ex) + c(-0.3, 0.3), ylim = range(ellipses$ey) + c(-0.3, 0.3)) +  # zoomed to the ellipses
  labs(x = "Mean preferred policy: illicit drugs (1 = ban ... 9 = free sale)",
       y = "Mean preferred policy: alcohol and tobacco",
       caption = str_wrap(sprintf(paste(
         "Dot: weighted mean of the party's voters; ellipse: %s %s (weighted covariance, so the tilt shows",
         "the correlation of the two scores within the party). The map is zoomed to the ellipses (scale 1-9).",
         "Dashed lines: the sample means of figure 4a. Point size: number of voters (n = %d in total). DENK (n = 1),",
         "other parties, blank and non-voters left out. Vote: TK 2025."),
         if (sd_mult == 1) "±1" else paste0("±", sd_mult), "standard deviation of its voters",
         sum(party_sd$n)), 115)) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), legend.position = "right", plot.caption = element_text(hjust = 0, colour = "grey40"))

dir.create(here::here("analysis", "output"), showWarnings = FALSE)
ggsave(here::here("analysis", "output", "party_map_sd.pdf"), p, width = 7.5, height = 7, device = cairo_pdf)
