library(lubridate)
library(tidyr)
library(dplyr)

PORT <- if (identical(Sys.getenv("TRAVIS"), "true")) {
  5432
} else {
  port <- Sys.getenv('POSTGRES_PORT')
  ifelse(port == '', 5433, port)
}
USER <- if (identical(Sys.getenv("TRAVIS"), "true")) {
  'postgres'
} else {
  Sys.info()[['user']]
}


# Connect to DB and add small_flights data ----

dbcon <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
      dbname = "postgres",
      user = USER,
      password = "",
      host = "localhost",
      port = PORT
    )

con <- dbplyr::src_dbi(dbcon, auto_disconnect = TRUE)

small_flights <-
  nycflights13::flights %>% filter(month < 3) %>%
  mutate(date = as.Date(ISOdate(year, month, day))) %>%
  select(date, carrier, origin, dep_delay, arr_delay, distance)


tbl_small_flights <- copy_to(con, small_flights, "small_flights", overwrite = TRUE)


# Copy in calendar dates to db ----

dates <- seq(as.Date("2013-01-01"),
             today() + years(2),
             by = 1)
calendar_periods <- tibble(date_original = dates) %>%
  crossing(period = c("day", "week", "month", "quarter", "year")) %>%
  group_by(period) %>%
  mutate(date = floor_date(date_original, period[1], week_start = 1)) %>%
  ungroup() %>%
  select(period, date, date_original)


tbl_remote_date_periods <-
  copy_to(con, calendar_periods, "remote_date_periods", overwrite = TRUE)

# set global remote_date_periods option
options(remote_date_periods = function() {tbl_remote_date_periods})
