library("bsts")
library("forecast")
library("sweep")
library("tidyverse")

source("load_data.R")
source("bsts_wrapper.R")
source("prophet_wrapper.R")

# Number of months to forecast (*after* test set)
h <- 12

# Confidence levels for prediction intervals
levels <- c(80, 95)

# State specification for BSTS models
bsts_state_spec <- function(ts) {
    state_spec <- AddLocalLinearTrend(NULL, ts)
    state_spec <- AddSeasonal(state_spec, ts, nseasons = 12)
    state_spec
}

# Estimate ARIMA and ETS models
models <- deaths %>%
          mutate(
              arima = map(train, auto.arima),
              ets = map(train, ets)
          )

# Collect all forecasts
forecasts <- models %>%
             transmute(
                 region,
                 sex,
                 test,
                 meanf = map2(train, test,
                              ~ meanf(.x,
                                      h = length(.y) + h,
                                      level = levels)),
                 naive = map2(train, test,
                              ~ naive(.x,
                                      h = length(.y) + h,
                                      level = levels)),
                 snaive = map2(train, test,
                               ~ snaive(.x,
                                        h = length(.y) + h,
                                        level = levels)),
                 rwf_drift = map2(train, test,
                                  ~ rwf(.x,
                                        h = length(.y) + h,
                                        drift = TRUE,
                                        level = levels)),
                 arima = map2(arima, test,
                              ~ forecast(.x,
                                         h = length(.y) + h,
                                         level = levels)),
                 bsts = map2(train, test,
                             ~ bsts_forecast(.x, .y,
                                             state_spec = bsts_state_spec(.x),
                                             h = h,
                                             levels = levels)),
                 ets = map2(ets, test,
                            ~ forecast(.x,
                                       h = length(.y) + h,
                                       level = levels)),
                 prophet = map2(train, test,
                                ~ prophet_forecast(.x, .y,
                                                   h = h,
                                                   levels = levels))
             )

write_rds(forecasts, "models/univariate.rds")

sw_forecast <- function(x, test) {
    test_tbl <- tk_tbl(test, rename_index = "dt") %>%
                mutate(
                    dt = as.Date(dt, frac = 1),
                    key = "actual"
                )
    x %>% sw_sweep(timetk_idx = TRUE, rename_index = "dt") %>%
          mutate(dt = as.Date(dt, frac = 1)) %>%
          bind_rows(test_tbl) %>%
          arrange(dt, key)
}

# Extract forecasts into a tibble
forecasts_tbl <- forecasts %>%
                 mutate_at(
                     vars(-region, -sex, -test),
                     funs(map2(., test, sw_forecast))
                 ) %>%
                 select(-test) %>%
                 gather(model, forecasts, -region, -sex) %>%
                 unnest(forecasts)

# Compute accuracy metrics
accuracies <- forecasts %>%
              mutate_at(
                  vars(-region, -sex, -test),
                  funs(map2(., test, ~ accuracy(.x, .y) %>%
                                       as_tibble(rownames = "set") %>%
                                       filter(set == "Test set") %>%
                                       select(-set) %>%
                                       gather(metric, value)))
              ) %>%
              select(-test) %>%
              gather(model, accuracies, -region, -sex) %>%
              unnest(accuracies)

phi <- (1 + sqrt(5)) / 2

# Box plot of MAPE by model
(accuracies %>%
     filter(metric == "MAPE") %>%
     ggplot(mapping = aes(x = fct_reorder(model, value, .desc = TRUE),
                          y = value)) +
     geom_boxplot(color = "darkgray") +
     geom_jitter(width = 0.25, height = 0) +
     labs(x = "Model", y = "MAPE") +
     theme(axis.text.x = element_text(angle = 45, hjust = 1))) %>%
    ggsave(filename = "plots/mape_by_model.pdf",
           width = 5 * phi, height = 5)

# Box plot of MAPE by region
(accuracies %>%
     inner_join(region_names, by = "region") %>%
     filter(metric == "MAPE") %>%
     ggplot(mapping = aes(x = fct_reorder(region_name, value, .desc = TRUE),
                          y = value)) +
     geom_boxplot(color = "darkgray") +
     geom_jitter(mapping = aes(color = model), width = 0.25, height = 0) +
     scale_color_brewer(type = "qual", palette = "Dark2") +
     labs(x = "Region", y = "MAPE") +
     theme(axis.text.x = element_text(angle = 45, hjust = 1))) %>%
    ggsave(filename = "plots/mape_by_region.pdf",
           width = 5 * phi, height = 5)

# Forecasts by model
plot_forecast <- function(tbl) {
    cutoff <- as.Date(as.yearmon(str_c(train_end, collapse = "-")), frac = 1) -
              years(2)
    tbl %>%
        filter(dt >= cutoff) %>%
        inner_join(region_names, by = "region") %>%
        ggplot(mapping = aes(x = dt, y = deaths, color = key, group = key)) +
        geom_ribbon(mapping = aes(ymin = lo.95, ymax = hi.95),
                    fill = "#C8E6C9", color = NA, size = 0) +
        geom_ribbon(mapping = aes(ymin = lo.80, ymax = hi.80),
                    fill = "#A5D6A7", color = NA, size = 0) +
        geom_line() +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none") +
        labs(x = "", y = "Deaths") +
        scale_color_manual(values = c("actual" = "#B71C1C",
                                      "forecast" = "#1B5E20")) +
        facet_wrap(~ region_name + sex, ncol = 8, scales = "free_y")
}

forecast_plots <- forecasts_tbl %>%
                  group_by(model) %>%
                  do(plot = plot_forecast(.)) %>%
                  mutate(
                      output_file = sprintf("plots/forecasts_%s.pdf", model)
                  )

with(forecast_plots,
     walk2(output_file, plot,
           ~ ggsave(.x, .y, width = 10 * phi, height = 10)))

