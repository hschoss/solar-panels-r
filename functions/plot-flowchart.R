# plot_flowchart.R
# author: hannes
# last edited: 2026-05-31 14:47

plot_flowchart <- function(x, main_title = "Total Energy Flows") {
  fmt_mwh <- function(v) paste0(round(v, 2), " MWh")
  fmt_pct <- function(v) paste0(round(100 * v, 1), "%")
  
  # percentages for arrows
  p_prod_to_feedin   <- x$grid_feedin / x$production
  p_prod_to_direct   <- x$direct_consumption / x$production
  p_prod_to_battery  <- x$battery_charge / x$production
  
  p_cons_from_direct  <- x$direct_consumption / x$consumption
  p_cons_from_battery <- x$battery_discharge / x$consumption
  p_cons_from_grid    <- x$grid_consumption / x$consumption
  
  draw_box <- function(x0, y0, label, w = 0.26, h = 0.14, cex = 0.95) {
    rect(
      x0 - w / 2, y0 - h / 2,
      x0 + w / 2, y0 + h / 2,
      border = "black",
      lwd = 1.4
    )
    text(x0, y0, label, cex = cex)
  }
  
  draw_arrow <- function(x0, y0, x1, y1, label,
                         label_pos = 0.5, offset_x = 0, offset_y = 0.03, cex = 0.85) {
    arrows(x0, y0, x1, y1, length = 0.08, lwd = 1.4)
    text(
      x0 + (x1 - x0) * label_pos + offset_x,
      y0 + (y1 - y0) * label_pos + offset_y,
      label,
      cex = cex
    )
  }
  
  par(mar = c(0.5, 1, 1.2, 1))
  
  plot(
    0, 0,
    type = "n",
    xlim = c(0, 1),
    ylim = c(0.02, 0.86),
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = ""
  )
  
  title(
    main = main_title,
    line = 0.1,
    cex.main = 0.95
  )
  
  # boxes
  draw_box(
    0.13, 0.56,
    paste0("Solar\nproduction\n", fmt_mwh(x$production)),
    w = 0.26, h = 0.14
  )
  
  draw_box(
    0.43, 0.77,
    paste0("Grid\nfeed-in\n", fmt_mwh(x$grid_feedin)),
    w = 0.24, h = 0.14
  )
  
  draw_box(
    0.43, 0.56,
    paste0("Direct\nconsumption\n", fmt_mwh(x$direct_consumption)),
    w = 0.24, h = 0.14
  )
  
  draw_box(
    0.43, 0.28,
    paste0(
      "Battery\n",
      "charge: ", fmt_mwh(x$battery_charge), "\n",
      "discharge: ", fmt_mwh(x$battery_discharge)
    ),
    w = 0.30, h = 0.18
  )
  
  draw_box(
    0.74, 0.56,
    paste0("Household\nconsumption\n", fmt_mwh(x$consumption)),
    w = 0.30, h = 0.16
  )
  
  draw_box(
    0.74, 0.18,
    paste0("Grid\nconsumption\n", fmt_mwh(x$grid_consumption)),
    w = 0.28, h = 0.14
  )
  
  # arrows from solar production
  draw_arrow(
    0.26, 0.61, 0.31, 0.72,
    fmt_pct(p_prod_to_feedin),
    label_pos = 0.5, offset_x = -0.01, offset_y = 0.03
  )
  
  draw_arrow(
    0.26, 0.56, 0.31, 0.56,
    fmt_pct(p_prod_to_direct),
    label_pos = 0.5, offset_y = 0.04
  )
  
  draw_arrow(
    0.26, 0.50, 0.31, 0.35,
    fmt_pct(p_prod_to_battery),
    label_pos = 0.5, offset_x = -0.01, offset_y = -0.03
  )
  
  # arrows to household consumption
  draw_arrow(
    0.55, 0.56, 0.61, 0.56,
    fmt_pct(p_cons_from_direct),
    label_pos = 0.5, offset_y = 0.04
  )
  
  draw_arrow(
    0.58, 0.34, 0.61, 0.48,
    fmt_pct(p_cons_from_battery),
    label_pos = 0.5, offset_x = 0.01, offset_y = -0.02
  )
  
  draw_arrow(
    0.74, 0.25, 0.74, 0.48,
    fmt_pct(p_cons_from_grid),
    label_pos = 0.5, offset_x = 0.06, offset_y = 0
  )
  
  text(
    0.5, 0.08,
    "Left-side percentages are shares of solar production; right-side percentages are shares of household consumption.",
    cex = 0.75
  )
}
