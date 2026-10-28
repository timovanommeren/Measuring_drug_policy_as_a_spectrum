# party logos for the party maps (party_map.R, party_map_drugs.R)

# logos (SVG, Wikimedia Commons / Wikipedia) in analysis/assets/party_logos/<party>.svg;
# a party without one gets a dot
logo_dir <- here::here("analysis", "assets", "party_logos")
logos <- list.files(logo_dir, pattern = "\\.svg$", full.names = TRUE) |>
  set_names(\(f) tools::file_path_sans_ext(basename(f))) |>
  map(\(f) {
    # crop to the visible content (some SVGs, e.g. CDA, lack a viewBox and leave empty canvas)
    b <- rsvg::rsvg(f, width = 800)
    rows <- range(which(rowSums(b[, , 4] > 0) > 0))
    cols <- range(which(colSums(b[, , 4] > 0) > 0))
    b[rows[1]:rows[2], cols[1]:cols[2], ]
  })

# logos and their "n = " labels as one grob for annotation_custom(); `data` has columns party, x, y
# and n. Positioned in panel coordinates (npc) and sized in mm, so logos keep their shape; every
# logo gets the same area for a given n
logo_grob <- function(data, xlim, ylim, scale = 1) {
  grobs <- pmap(filter(data, party %in% names(logos)), \(party, x, y, n, ...) {
    img <- logos[[party]]
    aspect <- ncol(img) / nrow(img)
    side <- scale * (7 + 9 * sqrt(n / max(data$n)))
    w <- side * sqrt(aspect)
    h <- side / sqrt(aspect)
    px <- grid::unit((x - xlim[1]) / diff(xlim), "npc")
    py <- grid::unit((y - ylim[1]) / diff(ylim), "npc")
    grid::gList(
      grid::rasterGrob(img, x = px, y = py, width = grid::unit(w, "mm"), height = grid::unit(h, "mm"),
                       interpolate = TRUE),
      grid::textGrob(sprintf("n = %d", n), x = px, y = py - grid::unit(h / 2 + 1.5, "mm"),
                     gp = grid::gpar(fontsize = 6.5 * sqrt(scale), col = "grey30"))
    )
  })
  grid::gTree(children = do.call(grid::gList, grobs))
}
