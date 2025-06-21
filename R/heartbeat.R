
#' Append Current Date to Log File
#'
#' Appends the current system time to 'inst/log.txt'.
#' @export
heartbeat <- function() {

  # Variables
  timestamp <- Sys.time()
  output_dir   <- here::here("inst")
  log_file     <- fs::path(output_dir, "log.txt")
  sysinfo_file <- fs::path(output_dir, "sysinfo.txt")

  # Ensure output directory exists
  fs::dir_create(output_dir)

  # Append date to log file
  cat(format(timestamp), "\n", file = log_file, append = TRUE)

  # Prepare system info to write to sysinfo file
  system_info <- tibble::tribble(
    ~Attribute,           ~Value,
    "Time:",              as.character(Sys.time()),
    "User:",              Sys.info()[["user"]],
    "Machine:",           Sys.info()[["machine"]],
    "OS Type:",           .Platform$OS.type,
    "System:",            Sys.info()[["sysname"]],
    "Release:",           Sys.info()[["release"]],
    "Platform:",          R.version$platform,
    "Version:",           Sys.info()[["version"]],
    "R Version:",         R.version.string,
    "Working Directory:", getwd(),
    "Files - ls(.):",     list.files(".") |> paste(collapse = ":"),
    "Files - ls(..):",    list.files("..") |> paste(collapse = ":"),
  )

  # Write to file (write_fwf is an internally defined function)
  write_fwf(system_info, sysinfo_file)

  # Good hygiene to return NULL
  invisible(NULL)
}
