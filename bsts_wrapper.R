library("bsts")
library("timetk")
library("zoo")

bsts_forecast <- function(train, test, state_spec, h, levels) {
    model <- bsts(train, state.specification = state_spec,
                  niter = 1000, ping = 0, seed = 42)
    burn <- SuggestBurn(proportion = 0.1, model)
    start_train <- train %>% tk_index %>% yearmon %>% min
    fitted <- tk_ts(
        with(model,
            original.series - colMeans(one.step.prediction.errors[-(1:burn), ])
        ),
        start = c(year(start_train), month(start_train)),
        frequency = 12
    )
    quantiles <- (levels / 100) %>% map(~ 0.5 + c(-., .) / 2) %>% unlist
    predictions <- predict(model, horizon = length(test) + h, burn = burn,
                           quantiles = quantiles)
    start_test <- test %>% tk_index %>% yearmon %>% min
    mean <- predictions$mean %>%
            tk_ts(start = c(year(start_test), month(start_test)),
                  frequency = 12)
    lower <- t(predictions$interval[seq(1, length(quantiles), 2), ]) %>%
             magrittr::set_colnames(sprintf("%d%%", levels)) %>%
             tk_ts(start = c(year(start_test), month(start_test)),
                   frequency = 12)
    upper <- t(predictions$interval[seq(2, length(quantiles), 2), ]) %>%
             magrittr::set_colnames(sprintf("%d%%", levels)) %>%
             tk_ts(start = c(year(start_test), month(start_test)),
                   frequency = 12)
    structure(list(
        method = "BSTS",
        level = levels,
        x = train,
        fitted = fitted,
        mean = mean,
        lower = lower,
        upper = upper
    ), class = "forecast")
}

