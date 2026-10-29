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
# logo width and height in mm: area grows with n, same area for wide and tall logos
logo_size <- function(party, n, max_n, scale = 1) {
  aspect <- ncol(logos[[party]]) / nrow(logos[[party]])
  side <- scale * (7 + 9 * sqrt(n / max_n))
  c(w = side * sqrt(aspect), h = side / sqrt(aspect))
}

# move overlapping logos (each with its n label below) apart on a panel of `panel_mm` (width, height);
# returns the data with the new x and y and the party's own position in x0 and y0
repel_logos <- function(data, xlim, ylim, panel_mm, scale = 1, gap = 1, iterations = 2000) {
  stopifnot(all(data$party %in% names(logos)))
  size <- do.call(rbind, map2(data$party, data$n, \(p, n) logo_size(p, n, max(data$n), scale)))
  hw <- size[, "w"] / 2 + gap / 2
  # the n label adds about 4 mm below the logo: the box reaches down further than up
  hh <- (size[, "h"] + 4) / 2 + gap / 2
  off <- -2
  px <- (data$x - xlim[1]) / diff(xlim) * panel_mm[1]
  py <- (data$y - ylim[1]) / diff(ylim) * panel_mm[2]
  for (it in seq_len(iterations)) {
    moved <- FALSE
    for (i in seq_along(px)) for (j in seq_along(px)) if (i < j) {
      dx <- px[j] - px[i]
      dy <- (py[j] + off) - (py[i] + off)
      ox <- hw[i] + hw[j] - abs(dx)
      oy <- hh[i] + hh[j] - abs(dy)
      if (ox > 0 && oy > 0) {
        moved <- TRUE
        # push apart along the axis that needs the smaller move, the smaller logo moves more
        share <- hw[i] * hh[i] / (hw[i] * hh[i] + hw[j] * hh[j])
        if (ox < oy) {
          s <- if (dx == 0) 1 else sign(dx)
          px[i] <- px[i] - s * ox * (1 - share); px[j] <- px[j] + s * ox * share
        } else {
          s <- if (dy == 0) 1 else sign(dy)
          py[i] <- py[i] - s * oy * (1 - share); py[j] <- py[j] + s * oy * share
        }
      }
    }
    px <- pmin(pmax(px, hw), panel_mm[1] - hw)
    py <- pmin(pmax(py, hh - off), panel_mm[2] - hh - off)
    if (!moved) break
  }
  mutate(data, x0 = x, y0 = y,
         x = xlim[1] + px / panel_mm[1] * diff(xlim), y = ylim[1] + py / panel_mm[2] * diff(ylim))
}

logo_grob <- function(data, xlim, ylim, scale = 1) {
  grobs <- pmap(filter(data, party %in% names(logos)), \(party, x, y, n, ...) {
    img <- logos[[party]]
    size <- logo_size(party, n, max(data$n), scale)
    w <- size[["w"]]
    h <- size[["h"]]
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
