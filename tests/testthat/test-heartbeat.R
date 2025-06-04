
test_that("append_date creates log file", {

  # Do a heartbeat
  heartbeat()

  # Log file should exist
  log_file <- here::here("inst", "log.txt")
  expect_true(fs::file_exists(log_file))

  # We expect the last run to be between 0 and 10 seconds old
  run_dates <- readr::read_lines(log_file) |> readr::parse_datetime()
  last_run_age <- min(difftime(Sys.time(), run_dates, units = "secs"))
  expect_gt(last_run_age, 0)
  expect_lt(last_run_age, 10)
})
