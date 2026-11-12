library(tidyverse)

source(here::here("analysis", "R", "00_load_and_clean.R"))
source(here::here("analysis", "R", "utils", "party_map_scores.R"))

# How the voters of each party are spread over the party map (figure 4a): one panel per party, same axes as the
# map (x = mean preferred policy for the illicit drugs, y = for alcohol and tobacco; 1 = ban ... 9 = free sale).
# Shaded: the smallest regions holding 50% and 80% of the party's voters (weighted), from a weighted kernel
# density. Grey outline: the same 50% region for all respondents on the map. Dot: the party mean; dashed lines:
# the sample means that split figure 4a into quadrants. Panels ordered by the party mean on the illicit drugs.
# The density uses one bandwidth for every party, so the shapes are comparable, and reflects the data at 1 and 9
# so the mass piled on "ban" stays inside the scale.
# Output: analysis/output/party_distributions.pdf

bw <- 0.45                                # kernel sd in scale points, same for every party
grid <- seq(1, 9, length.out = 161)

# weighted 2-d kernel density on the grid; a kernel per axis, with the data reflected at 1 and 9
kernel <- \(v) dnorm(outer(grid, v, "-"), sd = bw) + dnorm(outer(grid, 2 - v, "-"), sd = bw) +
  dnorm(outer(grid, 18 - v, "-"), sd = bw)
density_grid <- function(x, y, w) {
  z <- kernel(x) %*% (w * t(kernel(y)))   # rows = x grid, columns = y grid
  expand_grid(x = grid, y = grid) |> mutate(z = as.vector(t(z)) / sum(z))
}

# density level that encloses a share p of the mass (highest-density region)
hdr_level <- \(z, p) { s <- sort(z, decreasing = TRUE); s[which(cumsum(s) >= p)[1]] }

party_density <- resp |>
  nest(.by = party) |>
  mutate(n = map_int(data, nrow), mx = map_dbl(data, \(d) weighted.mean(d$x, d$w)),
         dens = map(data, \(d) density_grid(d$x, d$y, d$w))) |>
  select(-data) |>
  unnest(dens) |>
  mutate(band = case_when(z >= hdr_level(z, 0.5) ~ "50%", z >= hdr_level(z, 0.8) ~ "80%"), .by = party) |>
  mutate(panel = fct_reorder(sprintf("%s (n = %d)", party, n), mx))

sample_density <- density_grid(resp$x, resp$y, resp$w)
sample_50 <- hdr_level(sample_density$z, 0.5)
means <- party_means |> left_join(distinct(party_density, party, panel), by = "party")

p <- ggplot(party_density, aes(x, y)) +
  geom_tile(data = filter(party_density, !is.na(band)), aes(fill = band)) +
  geom_contour(data = sample_density, aes(z = z), breaks = sample_50, colour = "grey45", linewidth = 0.3) +
  geom_hline(yintercept = ref[["y"]], linetype = "dashed", linewidth = 0.25, colour = "grey40") +
  geom_vline(xintercept = ref[["x"]], linetype = "dashed", linewidth = 0.25, colour = "grey40") +
  geom_point(data = means, size = 1.6, colour = "black") +
  scale_fill_manual(values = c(`50%` = "#3b528b", `80%` = "#a9b7dc"), name = "Share of the party's voters") +
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9), expand = expansion()) +
  scale_y_continuous(breaks = c(1, 3, 5, 7, 9), expand = expansion()) +
  coord_fixed() +
  facet_wrap(~panel, ncol = 5) +
  labs(x = "Mean preferred policy: illicit drugs (1 = ban ... 9 = free sale)",
       y = "Mean preferred policy: alcohol and tobacco",
       caption = str_wrap(paste("Shaded: smallest regions holding 50% and 80% of the party's voters (weighted kernel",
                                "density, bandwidth 0.45). Grey line: the same 50% region for all party voters. Dot: party",
                                "mean; dashed: the sample means of figure 4a. Parties with fewer than 40 voters give",
                                "patchy shapes. Vote: TK 2025."), 150)) +
  theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank(), legend.position = "top", strip.text = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, colour = "grey40"))

dir.create(here::here("analysis", "output"), showWarnings = FALSE)
ggsave(here::here("analysis", "output", "party_distributions.pdf"), p, width = 9, height = 6.6, device = cairo_pdf)
