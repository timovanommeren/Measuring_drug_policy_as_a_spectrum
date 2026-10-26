library(tidyverse)
library(patchwork)
library(sf)

source(here::here("analysis", "R", "load_and_clean.R"))

# raw and 99% trimmed weights in long format, so every plot can show both side by side
weight_versions <- c(weight = "Raw", weight_trimmed_0990 = "Trimmed (99%)")

# (drop the analysis weight `w` from load_and_clean.R; here `w` is the weight of each version)
wdf <- df |>
  select(-any_of("w")) |>
  pivot_longer(all_of(names(weight_versions)), names_to = "version", values_to = "w") |>
  mutate(version = factor(weight_versions[version], levels = weight_versions))

# overall distribution of weights (log scale)
w_stats <- wdf |>
  summarise(n = n(), min = min(w), median = median(w), max = max(w),
            deff = n() * sum(w^2) / sum(w)^2, .by = version) |>
  mutate(label = sprintf("n = %d\nmin = %.2f\nmedian = %.2f (red)\nmax = %.2f\nKish design effect = %.2f",
                         n, min, median, max, deff))

ggplot(wdf, aes(w)) +
  geom_histogram(bins = 40, fill = "grey60", colour = "white") +
  geom_vline(xintercept = 1, linetype = 2) +
  geom_vline(data = w_stats, aes(xintercept = median), colour = "firebrick") +
  geom_text(data = w_stats, aes(x = Inf, y = Inf, label = label), hjust = 1.05, vjust = 1.1, size = 3) +
  scale_x_log10() +
  facet_wrap(~version) +
  labs(x = "weight (log scale)", y = "respondents")


# boxplot van weging per demografie
long <- wdf |>
  select(version, w, all_of(vars)) |>
  pivot_longer(all_of(vars), names_to = "var", values_to = "cat", values_transform = as.character)

ggplot(long, aes(w, cat)) +
  geom_boxplot(outlier.alpha = .2) +
  geom_vline(xintercept = 1, linetype = 2) +
  scale_x_log10() +
  facet_grid(var ~ version, scales = "free_y", space = "free_y") +
  labs(x = "weight (log scale)", y = NULL)



# how demographics shift after weighting
shares <- long |>
  summarise(unweighted = n(), weighted = sum(w), .by = c(version, var, cat)) |>
  mutate(across(c(unweighted, weighted), \(x) x / sum(x)), .by = c(version, var))

ggplot(shares, aes(y = cat)) +
  geom_segment(aes(x = unweighted, xend = weighted), colour = "grey70") +
  geom_point(aes(x = unweighted), colour = "grey40") +
  geom_point(aes(x = weighted), colour = "firebrick") +
  facet_grid(var ~ version, scales = "free_y", space = "free_y") +
  labs(x = "share (grey = sample, red = weighted)", y = NULL)



heat <- combn(vars, 2, simplify = FALSE) |>
  map(\(p) wdf |>
        summarise(lw = mean(log(w)), n = n(), .by = c(version, all_of(p))) |>
        transmute(version, x = as.character(.data[[p[1]]]), y = as.character(.data[[p[2]]]),
                  lw, n, pair = paste(p, collapse = " × "))) |>
  list_rbind()

# one page per pair of variables, same colour scale on every page
heat_plot <- function(p) {
  heat |>
    filter(pair == p) |>
    ggplot(aes(x, y, fill = lw)) +
    geom_tile() +
    geom_text(aes(label = n), size = 2.5) +
    scale_fill_gradient2(midpoint = 0, limits = range(heat$lw), labels = \(x) round(exp(x), 2),
                         name = "geom. mean\nweight") +
    facet_wrap(~version) +
    labs(title = p, x = NULL, y = NULL) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

walk(unique(heat$pair), \(p) print(heat_plot(p)))



# what do the people with highest and lowest weight_trimmed_0990 s look like?
profiles <- df |>
  summarise(weight_trimmed_0990  = mean(weight_trimmed_0990 ), n = n(), .by = all_of(vars))

extremes <- bind_rows(
  highest = slice_max(profiles, weight_trimmed_0990 , n = 20, with_ties = FALSE),
  lowest  = slice_min(profiles, weight_trimmed_0990 , n = 20, with_ties = FALSE),
  .id = "group"
) |>
  mutate(profile = fct_reorder(sprintf("%.2f (n=%d) #%d", weight_trimmed_0990 , n, row_number()), weight_trimmed_0990 ))

extremes |>
  pivot_longer(all_of(vars), values_transform = as.character) |>
  ggplot(aes(name, profile, label = value)) +
  geom_tile(fill = "white", colour = "grey80") +
  geom_text(size = 3) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  labs(x = NULL, y = "weight_trimmed_0990  (n)") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))



# map: weight per COROP region
corop_file <- here::here("analysis", "data", "coropgebied.geojson")
if (!file.exists(corop_file)) {
  download.file(paste0("https://service.pdok.nl/cbs/gebiedsindelingen/2023/wfs/v1_0?request=GetFeature",
                       "&service=WFS&version=2.0.0&typeName=gebiedsindelingen:coropgebied_gegeneraliseerd",
                       "&outputFormat=json"), corop_file, mode = "wb")
}
corop <- st_read(corop_file, quiet = TRUE)

# mean weight = weighted share / sample share, so 1 means the region is represented correctly
corop_w <- wdf |>
  filter(!is.na(Corop_regio_rec)) |>
  mutate(statcode = sprintf("CR%02d", as.integer(Corop_regio_rec)),
         region = haven::as_factor(Corop_regio_rec)) |>
  summarise(n = n(), mean_w = mean(w), sd_log = sd(log(w)),
            .by = c(version, statcode, region))

corop_map <- corop |>
  inner_join(corop_w, by = "statcode", relationship = "one-to-many") |>
  mutate(low_n = n < 10)

# codes and names should match between survey and CBS boundaries (survey uses a curly apostrophe)
stopifnot(nrow(corop_map) == 40 * length(weight_versions), !anyNA(corop_map$n),
          all(corop_map$statnaam == str_replace_all(corop_map$region, "’", "'")))

map_base <- function(fill) {
  ggplot(corop_map) +
    geom_sf(aes(fill = if_else(low_n, NA_real_, {{ fill }})), colour = "white", linewidth = .2) +
    geom_sf_text(aes(label = n), size = 2) +
    facet_wrap(~version) +
    theme_void()
}

p_mean <- map_base(log(mean_w)) +
  scale_fill_gradient2(midpoint = 0, labels = \(x) round(exp(x), 2), na.value = "grey85",
                       name = "mean\nweight") +
  labs(title = "Average weight")

p_spread <- map_base(sd_log) +
  scale_fill_viridis_c(na.value = "grey85", name = "SD of\nlog weight") +
  labs(title = "Spread of weights")

p_mean / p_spread +
  plot_annotation(caption = "Numbers = respondents per COROP region; grey = n < 10")

# full spread per region
wdf |>
  filter(!is.na(Corop_regio_rec)) |>
  mutate(region = haven::as_factor(Corop_regio_rec)) |>
  add_count(version, region) |>
  mutate(region = fct_reorder(sprintf("%s (n=%d)", region, n), w, median)) |>
  ggplot(aes(w, region)) +
  geom_boxplot(outlier.alpha = .3) +
  geom_vline(xintercept = 1, linetype = 2) +
  scale_x_log10() +
  facet_wrap(~version) +
  labs(x = "weight (log scale)", y = NULL)
