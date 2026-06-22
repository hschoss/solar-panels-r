# plot-month-by-year.R
# author: hannes
# last edited: 2026-05-26 22:08

plot_month_by_year <- function(data, variable, ylab = NULL, main = NULL) {
  
  daily <- aggregate(
    data[[variable]],
    by = list(date = as.Date(data$time)),
    FUN = sum,
    na.rm = TRUE
  )
  
  names(daily)[2] <- variable 
  
  daily$year <- as.integer(format(daily$date, "%Y"))
  daily$month <- as.integer(format(daily$date, "%m"))
  
  agg <- aggregate(
    daily[[variable]],
    by = list(year = daily$year, month = daily$month),
    FUN = mean,
    na.rm = TRUE
  )

  names(agg)[3] <- variable

  years <- sort(unique(agg$year))
  cols <- gray.colors(length(years), start = 0.8, end = 0.2)
  
  plot(
    NA,
    xlim = c(1, 12),
    ylim = range(agg[[variable]] / 1000, na.rm = TRUE),
    xlab = "Month",
    ylab = ylab,
    main = main,
    xaxt = "n"
  )
  
  axis(1, at = 1:12, labels = month.abb)
  
  for (i in seq_along(years)) {
    tmp <- subset(agg, year == years[i])
    tmp <- tmp[order(tmp$month), ]
    
    lines(
      tmp$month,
      tmp[[variable]] / 1000,       # wh -> kwh
      col = cols[i],
      lwd = 2
    )
  }
  
  legend(
    "topright",
    legend = years,
    col = cols,
    lwd = 2,
    bty = "n"
  )
}
