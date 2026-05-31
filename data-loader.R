# data-loader.R
# author: hannes
# date: 2026-05-26 21:16

# setwd("/home/hannes/gh/solar")

dat23 <- read.csv("data/sonnen_power_data_2023.csv", header = TRUE, sep = ",")
dat24 <- read.csv("data/sonnen_power_data_2024.csv", header = TRUE, sep = ",")
dat25 <- read.csv("data/sonnen_power_data_2025.csv", header = TRUE, sep = ",")
dat26 <- read.csv("data/sonnen_power_data_2026.csv", header = TRUE, sep = ",")

dat <- rbind(dat23, dat24, dat25, dat26)

## factorization

dat$timestamp_clean <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", dat$timestamp)

dat$time <- as.POSIXct(
  dat$timestamp_clean,
  format = "%Y-%m-%dT%H:%M:%S%z"
)
dat$timestamp <- NULL
dat$timestamp_clean <- NULL

dat$time <- as.POSIXct(dat$time, tz = "Europe/Berlin")
dat$year  <- as.integer(format(dat$time, "%Y"))
dat$month <- as.integer(format(dat$time, "%m"))
dat$day   <- as.integer(format(dat$time, "%d"))
dat$hour  <- as.integer(format(dat$time, "%H"))
dat$date  <- as.Date(dat$time)


