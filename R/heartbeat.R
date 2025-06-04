
#' Append Current Date to Log File
#'
#' Appends the current system time to 'inst/log.txt'.
#' @export
heartbeat <- function() {

  # Variables
  timestamp <- Sys.time()
  log_dir  <- here::here("inst")
  log_file <- fs::path(log_dir, "log.txt")

  # Output
  fs::dir_create(log_dir)
  cat(format(timestamp), "\n", file = log_file, append = TRUE)

  # Good hygiene to return NULL
  invisible(NULL)
}
