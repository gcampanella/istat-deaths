library("tidyverse")
library("hts")

source("load_data.R")

# Number of months to forecast (*after* test set)
h <- 12

# Collect all forecasts
forecasts <- crossing(
                 fmethod = c("arima", "ets", "rw"),
                 method = c("bu", "comb", "tdfp", "tdgsa", "tdgsf"),
                 weights = c("mint", "nseries", "ols", "wls")
             ) %>%
             mutate(
                 forecast = pmap(
                     list(fmethod, method, weights),
                     ~ forecast(deaths_train,
                                h = nrow(deaths_test$bts) + h,
                                method = ..2,
                                weights = ..3,
                                fmethod = ..1)
                     )
             )

write_rds(forecasts, "models/hts.rds")

hts_forecast_to_tbl <- function(forecast, level) {
    forecast_level <- aggts(forecast, levels = level)
    forecast_level %>%
        as_tibble %>%
        mutate(dt = as.Date(yearmon(tk_index(forecast_level)), frac = 1)) %>%
        gather(ts_name, deaths, -dt) %>%
        extract(ts_name, c("region", "sex"),
                regex = "R([0-9]{2})([M|F])", convert = TRUE)
}

add_actual_to_forecast_tbl <- function(forecast_tbl) {
    forecast_tbl %>%
        mutate(
            key = "forecast",
            sex = parse_factor(sex, levels = c("M", "F"))
        ) %>%
        bind_rows(
            deaths %>%
                unnest(tbl) %>%
                mutate(key = "actual")
            ) %>%
        arrange(region, sex, dt, key)
}

# Extract bottom-level forecasts into a tibble
forecasts_tbl <- forecasts %>%
                 mutate(
                     forecast = map(forecast,
                                    ~ hts_forecast_to_tbl(., level = 2) %>%
                                      add_actual_to_forecast_tbl),
                 ) %>%
                 unnest(forecast)

hts_accuracy <- function(forecast, test, level) {
    forecast_level <- forecast %>% hts_forecast_to_tbl(level = 2) %>%
                                   mutate(dt = as.yearmon(dt)) %>%
                                   group_by(region, sex) %>%
                                   nest(.key = "tbl") %>%
                                   mutate(ts = map(tbl, tbl_to_ts))
    test_level <- test %>% hts_forecast_to_tbl(level = 2) %>%
                           mutate(dt = as.yearmon(dt)) %>%
                           group_by(region, sex) %>%
                           nest(.key = "tbl") %>%
                           mutate(ts = map(tbl, tbl_to_ts))

    inner_join(forecast_level, test_level,
               by = c("region", "sex"),
               suffix = c("_forecast", "_test")) %>%
        mutate(
            accuracies = map2(ts_forecast, ts_test,
                              ~ accuracy(.x, .y) %>%
                                as_tibble(rownames = "set") %>%
                                filter(set == "Test set") %>%
                                select(-set) %>%
                                gather(metric, value))
        ) %>%
        unnest(accuracies)
}

# Compute accuracy metrics at bottom level
accuracies <- forecasts %>%
              mutate(
                  accuracies = map(forecast, hts_accuracy,
                                             test = deaths_test,
                                             level = 2)
              ) %>%
              unnest(accuracies)

phi <- (1 + sqrt(5)) / 2

# Box plot of MAPE by forecasting method
(accuracies %>%
     filter(metric == "MAPE") %>%
     ggplot(mapping = aes(x = fct_reorder(fmethod, value, .desc = TRUE),
                          y = value)) +
     geom_boxplot() +
     labs(x = "Forecasting method", y = "MAPE") +
     theme(axis.text.x = element_text(angle = 45, hjust = 1))) %>%
    ggsave(filename = "plots/hts_mape_by_fmethod.pdf",
           width = 5, height = 5)

# Box plot of MAPE by method
(accuracies %>%
     filter(metric == "MAPE") %>%
     ggplot(mapping = aes(x = fct_reorder(method, value, .desc = TRUE),
                          y = value)) +
     geom_boxplot() +
     labs(x = "Method", y = "MAPE") +
     theme(axis.text.x = element_text(angle = 45, hjust = 1))) %>%
    ggsave(filename = "plots/hts_mape_by_method.pdf",
           width = 5, height = 5)

# Box plots of MAPE by region and forecasting method
(accuracies %>%
     inner_join(region_names, by = "region") %>%
     filter(metric == "MAPE") %>%
     ggplot(mapping = aes(x = fct_reorder(region_name, value, .desc = TRUE),
                          y = value)) +
     geom_boxplot(color = "darkgray") +
     geom_boxplot(mapping = aes(color = fmethod)) +
     scale_color_brewer(type = "qual", palette = "Dark2") +
     labs(x = "Region", y = "MAPE") +
     theme(axis.text.x = element_text(angle = 45, hjust = 1))) %>%
    ggsave(filename = "plots/hts_mape_by_region.pdf",
           width = 5 * phi, height = 5)

# Forecasts by model
plot_forecast <- function(tbl) {
    cutoff <- as.Date(as.yearmon(str_c(train_end, collapse = "-")), frac = 1) -
              years(2)
    tbl %>%
        filter(dt >= cutoff) %>%
        inner_join(region_names, by = "region") %>%
        ggplot(mapping = aes(x = dt, y = deaths, color = key, group = key)) +
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
                  group_by(fmethod, method, weights) %>%
                  do(plot = plot_forecast(.)) %>%
                  mutate(
                      output_file = sprintf("plots/forecasts_hts_%s_%s_%s.pdf",
                                            fmethod, method, weights)
                  )

with(forecast_plots,
     walk2(output_file, plot,
           ~ ggsave(.x, .y, width = 10 * phi, height = 10)))

