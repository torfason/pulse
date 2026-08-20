
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
  tmpinfo_file <- fs::path(output_dir, "tmpinfo.txt")

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
    "Files - ls(../..):", list.files(fs::path("..", "..")) |> paste(collapse = ":"),
    "Tempfile:",          tempfile(),
  )

  # Write to file (write_fwf is an internally defined function)
  write_fwf(system_info, sysinfo_file)

  # Call mirai_beat() to get info about tempfile behavior, then write it to file
  tempfile_info <- mirai_beat()
  write_fwf(tempfile_info, tmpinfo_file)

  # Good hygiene to return NULL
  invisible(NULL)
}

mirai_beat <- function() {

  # Set up 3 mirai daemons to get tempfile names in three processes
  with(mirai::daemons(3, .compute = tempfile()), {
    mirai::everywhere({
      pidgetter <<- function(x) {
        tibble::tibble(run = x, pid = Sys.getpid(), tmp = tempfile())
      }
    })
    l.tempfile_info <- mirai::mirai_map(1:6, \(x){pidgetter(x)})[]
  })
  tempfile_info <- do.call(rbind, l.tempfile_info)
  tempfile_info

}
