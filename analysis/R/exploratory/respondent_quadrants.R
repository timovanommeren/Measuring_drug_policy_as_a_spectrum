library(tidyverse)

source(here::here("analysis", "R", "00_load_and_clean.R"))
source(here::here("analysis", "R", "utils", "party_map_scores.R"))

# Figure 4a with respondents instead of parties: every respondent by mean preferred policy for the illicit
# drugs (x) and for alcohol and tobacco (y), scores from utils/party_map_scores.R. Dashed lines at the
# weighted means of all respondents (not of the party voters only, as in figure 4a). Points are jittered
# (x steps of 1/6, y of 1/2: a mean over six or two answers takes few values). Output: analysis/output/respondent_quadrants.pdf

option_ticks <- c("Ban (1)", "Research only (2)", "Treatment only (3)", "Prescription only (4)",
                  "Specialist shop,\nnon-profit (5)", "Specialist shop,\nfor-profit (6)",
                  "Sold everywhere,\nno advertising (7)", "Sold everywhere,\nwith advertising (8)", "Free sale (9)")

means <- c(x = weighted.mean(scores$x, scores$w), y = weighted.mean(scores$y, scores$w))

# weighted share of respondents per quadrant, named in its outer corner
quad <- tibble(right = c(FALSE, TRUE, FALSE, TRUE), top = c(TRUE, TRUE, FALSE, FALSE),
               label = c("Double standards (status quo)", "Liberals", "Abstainers", "Reformers"),
               colour = c("#E69F00", "#009E73", "#56B4E9", "#CC79A7")) |>
  left_join(count(scores, right = x > means[["x"]], top = y > means[["y"]], wt = w), by = c("right", "top")) |>
  mutate(label = sprintf("%s: %.0f%%", label, 100 * n / sum(n)),
         lx = if_else(right, 9.4, 0.6), ly = if_else(top, 9.4, 0.6))

p <- ggplot(scores, aes(x, y)) +
  geom_point(position = position_jitter(width = 0.07, height = 0.15, seed = 1), alpha = 0.25, size = 0.8,
             colour = "#3b528b") +
  geom_hline(yintercept = means[["y"]], linetype = "dashed", linewidth = 0.3, colour = "grey40") +
  geom_vline(xintercept = means[["x"]], linetype = "dashed", linewidth = 0.3, colour = "grey40") +
  geom_text(data = quad, aes(lx, ly, label = label, hjust = right, vjust = top),
            colour = colorspace::darken(quad$colour, 0.35), fontface = "bold", size = 2.6) +
  scale_x_continuous(breaks = 1:9, labels = option_ticks) +
  scale_y_continuous(breaks = 1:9, labels = option_ticks) +
  coord_cartesian(xlim = c(0.6, 9.4), ylim = c(0.6, 9.4), expand = FALSE) +
  labs(x = "Mean preferred policy: illicit drugs", y = "Mean preferred policy:\nalcohol and tobacco",
       caption = sprintf("n = %d respondents; dashed lines at the weighted means (x = %.2f, y = %.2f); quadrant shares weighted.",
                         nrow(scores), means[["x"]], means[["y"]])) +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), text = element_text(size = 8),
        axis.text = element_text(size = 7, lineheight = 0.9),
        axis.text.x = element_text(angle = 40, hjust = 1), plot.caption = element_text(colour = "grey40"))

dir.create(here::here("analysis", "output"), showWarnings = FALSE)
ggsave(here::here("analysis", "output", "respondent_quadrants.pdf"), p, width = 6.5, height = 5.5, device = cairo_pdf)
