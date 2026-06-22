# data-cleaner.R

dat_clean <- dat

# variables that should be approximated
energy_vars <- c(
  "production",
  "consumption",
  "battery_charge",
  "battery_discharge",
  "grid_feedin",
  "grid_consumption",
  "direct_consumption"
)

# create complete grid for every 3-hour time interval from April 2023
start_date <- as.Date("2023-04-01")
end_month <- as.Date(
  sprintf("%d-%02d-01",
    max(dat_clean$year),
    max(dat_clean$month[dat_clean$year == max(dat_clean$year)])
  )
)

next_month <- if (as.integer(format(end_month, "%m")) == 12) {
  as.Date(sprintf("%d-01-01", as.integer(format(end_month, "%Y")) + 1))
} else {
  as.Date(sprintf( "%d-%02d-01",
      as.integer(format(end_month, "%Y")),
      as.integer(format(end_month, "%m")) + 1
    )
  )
}

end_date <- next_month - 1
all_dates <- seq(start_date, end_date, by = "day")
all_hours <- c(0, 3, 6, 9, 12, 15, 18, 21)

full_grid <- expand.grid(
  date = all_dates,
  hour = all_hours
)

full_grid$time <- as.POSIXct(
  paste(full_grid$date, sprintf("%02d:00:00", full_grid$hour)),
  tz = "Europe/Berlin"
)

full_grid$year <- as.integer(format(full_grid$time, "%Y"))
full_grid$month <- as.integer(format(full_grid$time, "%m"))
full_grid$day <- as.integer(format(full_grid$time, "%d"))

full_grid <- full_grid[order(full_grid$time), ]

# identify missing time slots
dat_key <- format(dat_clean$time, "%Y-%m-%d %H:%M:%S")
grid_key <- format(full_grid$time, "%Y-%m-%d %H:%M:%S")

missing_slots <- full_grid[!(grid_key %in% dat_key), ]

# helper: average from previous years for same month, day and hour
approximate_value <- function(var, year, month, day, hour) {
  ref <- dat_clean[
    dat_clean$year < year &
      dat_clean$month == month &
      dat_clean$day == day &
      dat_clean$hour == hour,
  ]
  
  value <- mean(ref[[var]], na.rm = TRUE)
  
  if (is.nan(value)) {
    ref <- dat_clean[
      dat_clean$year < year &
        dat_clean$month == month &
        dat_clean$hour == hour,
    ]
    
    value <- mean(ref[[var]], na.rm = TRUE)
  }
  
  value
}

# build approximated rows
approximated_rows <- missing_slots

for (v in energy_vars) {
  approximated_rows[[v]] <- mapply(
    function(y, m, d, h) approximate_value(v, y, m, d, h),
    approximated_rows$year,
    approximated_rows$month,
    approximated_rows$day,
    approximated_rows$hour
  )
}

# battery_state_of_charge is a state, not an energy flow
approximated_rows$battery_state_of_charge <- mapply(
  function(y, m, d, h) {
    approximate_value("battery_state_of_charge", y, m, d, h)
  },
  approximated_rows$year,
  approximated_rows$month,
  approximated_rows$day,
  approximated_rows$hour
)

approximated_rows$daytime <- NA
approximated_rows$daytime[approximated_rows$hour %in% c(21, 0, 3)] <- "night"
approximated_rows$daytime[approximated_rows$hour == 6] <- "morning"
approximated_rows$daytime[approximated_rows$hour == 9] <- "late_morning"
approximated_rows$daytime[approximated_rows$hour == 12] <- "midday"
approximated_rows$daytime[approximated_rows$hour == 15] <- "afternoon"
approximated_rows$daytime[approximated_rows$hour == 18] <- "evening"

# mark approximated rows
dat_clean$approximated <- FALSE
approximated_rows$approximated <- TRUE

# keep only columns that exist in both data frames
common_cols <- intersect(names(dat_clean), names(approximated_rows))

dat_balanced <- rbind(
  dat_clean[, common_cols],
  approximated_rows[, common_cols]
)

dat_balanced <- dat_balanced[order(dat_balanced$time), ]

dat <- dat_balanced
