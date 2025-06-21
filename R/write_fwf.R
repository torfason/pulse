
write_fwf <- function(d, file) {

  # Determine column widths: max of column name and max string in column + 2
  col_widths <- sapply(d, \(x){max(nchar(x))+2})

  # Create format string for sprintf
  fmt <- paste0(mapply(function(w) paste0("%-", w, "s"), col_widths), collapse = "")

  # Format header line and data lines
  header_line <- do.call(sprintf, c(fmt, as.list(names(d))))
  data_lines <- apply(d, 1, function(row) {
    do.call(sprintf, c(fmt, as.list(as.character(row))))
  })

  # Combine and write
  writeLines(c(header_line, data_lines), file)
}
