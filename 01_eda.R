library("tidyverse")

source("load_data.R")

deaths_total <- deaths %>%
                unnest(tbl) %>%
                group_by(
                    year = year(dt),
                    month = month(dt)
                ) %>%
                summarize_at(vars(deaths), sum) %>%
                ungroup

# Total deaths by month and year
(deaths_total %>%
     mutate(
         month = parse_factor(month.abb[month], levels = month.abb),
         year = parse_factor(year, full_seq(year, 1), ordered = TRUE)
     ) %>%
     ggplot(mapping = aes(x = month, y = deaths, group = year)) +
     geom_line(mapping = aes(color = year)) +
     labs(x = "", y = "Deaths")) %>%
    ggsave(filename = "plots/total_deaths_by_month_and_year.pdf",
           width = 5, height = 5)

# Total cumulative deaths by month (only for complete years)
(deaths_total %>%
     semi_join(
         deaths_total %>%
             group_by(year) %>%
             summarize(n_months = n_distinct(month)) %>%
             filter(n_months == 12),
         by = "year"
     ) %>%
     mutate(
         month = parse_factor(month.abb[month], levels = month.abb)
     ) %>%
     ggplot(mapping = aes(x = month, y = deaths, group = year)) +
     geom_area(mapping = aes(fill = year)) +
     labs(x = "", y = "Deaths")) %>%
    ggsave(filename = "plots/total_cumulative_deaths_by_month.pdf",
           width = 5, height = 5)

