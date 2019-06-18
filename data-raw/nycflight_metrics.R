library(dplyr)
library(tidymetrics)
library(nycflights13)

flight_summary <- flights %>%
  mutate(date = as.Date(ISOdate(year, month, day))) %>%
  cross_by_dimensions(origin, carrier) %>%
  cross_by_periods() %>%
  summarize(nb_flights = n(),
            avg_arr_delay = mean(arr_delay, na.rm = TRUE))

rmd <- system.file("extdata", "metrics_flights_nyc.Rmd", package = "tidymetrics")
flights_nyc <- create_metrics(flight_summary, rmd_file = rmd)

flights_nyc_avg_arr_delay <- nycflight_metrics$flights_nyc_avg_arr_delay

usethis::use_data(flights_nyc_avg_arr_delay, overwrite = TRUE)
