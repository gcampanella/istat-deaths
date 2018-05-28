library("hts")
library("lubridate")
library("tidyverse")
library("timetk")
library("zoo")
requireNamespace("magrittr")

istat_demo <- read_csv("../istat-demographics/istat-demographics.csv.gz")

region_names <- read_csv("region_names.csv") %>%
                rename(region_name = name)

# Last month of training data
train_end <- c(2016, 6)

tbl_to_ts <- function(tbl) {
    start <- min(tbl$dt)
    tk_ts(tbl, select = -dt, start = c(year(start), month(start)),
          frequency = 12)
}

deaths <- istat_demo %>%
          mutate(
              dt = as.Date(yearmon(year + (month - 1) / 12), frac = 1),
              sex = parse_factor(sex, levels = c("M", "F"))
          ) %>%
          group_by(region, dt, sex) %>%           # Aggregate at region level
          summarize_at(vars(deaths), sum) %>%
          ungroup %>%
          mutate(
              deaths = deaths / days_in_month(dt) # Calendar adjustment
          ) %>%
          group_by(region, sex) %>%
          nest(.key = "tbl") %>%                  # Nest, convert, and split
          mutate(
              ts = map(tbl, tbl_to_ts),
              train = map(ts, window, end = train_end),
              test = map(ts, window, start = train_end + c(0, 1))
          ) %>%
          arrange(region, sex)

# Prepare hierarchical time series
ts_names <- with(deaths, sprintf("R%02d%s", region, sex))
n_regions <- n_distinct(deaths$region)

deaths_train <- reduce(deaths$train, cbind) %>%
                magrittr::set_colnames(ts_names) %>%
                hts(nodes = list(n_regions, rep(2, n_regions)),
                    characters = c(3, 1))

deaths_test <- reduce(deaths$test, cbind) %>%
               magrittr::set_colnames(ts_names) %>%
               hts(nodes = list(n_regions, rep(2, n_regions)),
                   characters = c(3, 1))

