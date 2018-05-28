library("magrittr")
library("prophet")
library("timetk")

make_future_tbl <- function(ts, h = 12) {
    tibble(ds = ts %>%
                tk_index %>%
                tk_make_future_timeseries(n_future = h) %>%
                yearmon %>%
                as.Date(frac = 1))
}

ts_to_tbl <- function(ts) {
    tk_tbl(ts, rename_index = "ds") %>%
        transmute(
            ds = as.Date(ds, frac = 1),
            y = deaths
        )
}

prophet_fit <- function(ts, ...) {
    ts_to_tbl(ts) %>% prophet(...)
}

prophet_fitted <- function(model) {
    start <- model$history$ds %>% min %>% as.yearmon
    predict(model) %>%
        transmute(
            dt = as.yearmon(ds),
            deaths = yhat
        ) %>%
        tk_ts(select = -dt,
              start = c(year(start), month(start)),
              frequency = 12)
}

prophet_predict <- function(model, ts, h) {
    ts_tbl <- bind_rows(
                  ts_to_tbl(ts),
                  make_future_tbl(ts, h)
              )
    predict(model, ts_tbl) %>%
        mutate(ds = as.yearmon(ds)) %>%
        select(
            dt = ds,
            mean = yhat,
            lower = yhat_lower,
            upper = yhat_upper
        )
}

prophet_forecast <- function(train, test, h, levels) {
    models <- map(levels / 100,
                  ~ prophet_fit(train, interval.width = ., seed = 42))
    fitted <- prophet_fitted(models[[1]])
    predictions <- map(models, prophet_predict, ts = test, h = h)
    start <- predictions[[1]]$dt %>% min
    mean <- predictions[[1]]$mean %>%
            tk_ts(start = c(year(start), month(start)), frequency = 12)
    lower <- bind_cols(map(predictions, `[`, "lower")) %>%
             as.matrix %>%
             set_colnames(sprintf("%d%%", levels)) %>%
            tk_ts(start = c(year(start), month(start)), frequency = 12)
    upper <- bind_cols(map(predictions, `[`, "upper")) %>%
             as.matrix %>%
             set_colnames(sprintf("%d%%", levels)) %>%
             tk_ts(start = c(year(start), month(start)), frequency = 12)
    structure(list(
        method = "Prophet",
        level = levels,
        x = train,
        fitted = fitted,
        mean = mean,
        lower = lower,
        upper = upper
    ), class = "forecast")
}

