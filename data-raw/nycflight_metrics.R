library(dplyr)
library(tidymetrics)
library(nycflights13)

flight_summary <- flights %>%
  mutate(date = as.Date(ISOdate(year, month, day))) %>%
  cross_by_dimensions(origin, carrier) %>%
  cross_by_periods() %>%
  summarize(nb_flights = n(),
            avg_arr_delay = mean(arr_delay, na.rm = TRUE))

rmd <- system.file("extdata", "metrics_nycflight_stats.Rmd", package = "tidymetrics")
nycflight_metrics <- create_metrics(flight_summary, rmd_file = rmd)

flight_avg_arr_delay <- nycflight_metrics$nycflight_stats_avg_arr_delay

usethis::use_data(flight_avg_arr_delay, overwrite = TRUE)
