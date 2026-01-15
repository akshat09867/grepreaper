#' grep_read: Efficiently read and filter lines from one or more files using grep,
#' returning a data.table.
#'
#' @param files Character vector of file paths to read.
#' @param path Optional. Directory path to search for files.
#' @param file_pattern Optional. A pattern to filter filenames when using the
#'   `path` argument. Passed to `list.files`.
#' @param pattern Pattern to search for within files (passed to grep).
#' @param invert Logical; if TRUE, return non-matching lines.
#' @param ignore_case Logical; if TRUE, perform case-insensitive matching (default: TRUE).
#' @param fixed Logical; if TRUE, pattern is a fixed string, not a regular
#'   expression.
#' @param show_cmd Logical; if TRUE, return the grep command string instead of
#'   executing it.
#' @param recursive Logical; if TRUE, search recursively through directories.
#' @param word_match Logical; if TRUE, match only whole words.
#' @param show_line_numbers Logical; if TRUE, include line numbers from source
#'   files. Headers are automatically removed and lines renumbered.
#' @param only_matching Logical; if TRUE, return only the matching part of the
#'   lines.
#' @param nrows Integer; maximum number of rows to read.
#' @param skip Integer; number of rows to skip.
#' @param header Logical; if TRUE, treat first row as header.  Note that using FALSE means that the first row will be included as a row of data in the reading process.
#' @param col.names Character vector of column names.
#' @param include_filename Logical; if TRUE, include source filename as a column.
#' @param show_progress Logical; if TRUE, show progress indicators.
#' @param ... Additional arguments passed to fread.
#' @return A data.table with different structures based on the options:
#'   - Default: Data columns with original types preserved
#'   - show_line_numbers=TRUE: Additional 'line_number' column (integer) with source file line numbers
#'   - include_filename=TRUE: Additional 'source_file' column (character)
#'   - only_matching=TRUE: Single 'match' column with matched substrings
#'   - show_cmd=TRUE: Character string containing the grep command
#' @importFrom data.table fread setnames data.table as.data.table rbindlist setorder setcolorder ":=" .N
#' @importFrom stats setNames 
#' @importFrom methods as
#' @importFrom utils globalVariables
#' @export
#' @note When searching for literal strings (not regex patterns), set
#'   `fixed = TRUE` to avoid regex interpretation. For example, searching for
#'   "3.94" with `fixed = FALSE` will match "3894" because "." is a regex
#'   metacharacter.
#'
#' Header rows are automatically handled:
#'   - With show_line_numbers=TRUE: Headers (line_number=1) are removed and
#'     lines renumbered
#'   - Without line numbers: Headers matching column names are removed
#'   - Empty rows and all-NA rows are automatically filtered out


grep_read <- function(files = NULL, path = NULL, file_pattern = NULL, 
                      pattern = "", invert = FALSE, ignore_case = FALSE, 
                      fixed = FALSE, show_cmd = FALSE, recursive = FALSE, 
                      word_match = FALSE, show_line_numbers = FALSE, 
                      only_matching = FALSE, nrows = Inf, skip = 0, 
                      header = TRUE, col.names = NULL, include_filename = FALSE, 
                      show_progress = FALSE, ...) {
  
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required but not installed.")
  }
  
  # 1. Initialize variables and handle files
  if (is.null(include_filename)) include_filename <- FALSE
  if (is.null(files) && !is.null(path)) {
    files <- list.files(path = path, pattern = file_pattern, full.names = TRUE, 
                        recursive = recursive, ignore.case = ignore_case)
  }
  if (length(files) == 0) stop("No files found to read.")
  if (is.na(pattern[1])) pattern <- ""
  
  # 2. Build options string
  need_metadata <- (show_line_numbers == TRUE) || (include_filename == TRUE) || (length(files) > 1)
  
  # Initialize options safely to avoid "length zero" errors
  grep_opts <- c("-v", "-i", "-F", "-r", "-w", "-n", "-o", "-H")[
    c(invert, ignore_case, fixed, recursive, word_match, show_line_numbers, only_matching, need_metadata)
  ]
  options_str <- paste(grep_opts, collapse = " ")
  
  # 3. Build the command
  cmd <- build_grep_cmd(pattern = pattern, files = files, options = options_str, fixed = fixed)
  
  if (show_cmd == TRUE) return(cmd)

  # --- START OF SAFETY CHECK (Prevents Exit 127 / Crash) ---
  if (cmd == "" || !nzchar(Sys.which("grep"))) {
    if (.Platform$OS.type == "windows") {
      warning("grep utility not found. Returning empty data.table.")
      # Create an empty table based on the first file's structure if possible
      shallow <- data.table::fread(input = files[1], nrows = 0, header = header)
      return(shallow)
    }
    stop("System command 'grep' not found.")
  }
  # --- END OF SAFETY CHECK ---

  # 4. Execute the command
  # Use a shallow copy to get column types/names
  shallow.copy <- data.table::fread(input = files[1], nrows = 10, header = header)
  
  dat <- tryCatch({
    data.table::fread(cmd = cmd, header = FALSE, ...)
  }, error = function(e) {
    # If grep finds nothing, it might throw an error or return empty
    data.table::data.table()
  })
  
  # 5. Handle empty results
  if (nrow(dat) == 0) {
    return(shallow.copy[0, ])
  }
  
  # 6. Metadata and Column Processing
  setnames(x = dat, old = names(dat), new = c("V1", names(shallow.copy)[2:ncol(shallow.copy)]), skip_absent = TRUE)
  
  if (need_metadata == TRUE) {
    column.names <- c(c("file", "line_number")[c((include_filename == TRUE | length(files) > 1), show_line_numbers == TRUE)], names(shallow.copy)[1])
    
    # Assuming split_columns is a helper function in your package
    additional.columns <- split_columns(x = dat[, V1], column.names = column.names, resulting.columns = length(column.names))
    
    dat <- data.table::data.table(dat, additional.columns)
    dat[, V1 := NULL]
    data.table::setcolorder(x = dat, neworder = names(shallow.copy), skip_absent = TRUE)
    
    if (include_filename == FALSE && "file" %in% names(dat)) dat[, file := NULL]
    if (show_line_numbers == FALSE && "line_number" %in% names(dat)) dat[, line_number := NULL]
  } else {
    data.table::setnames(x = dat, old = "V1", new = names(shallow.copy)[1], skip_absent = TRUE)
  }
  
  # 7. Header cleaning (Removing repeated headers from multi-file grep)
  if (header == TRUE) {
    header_contains_pattern <- length(grep(pattern = paste(pattern, collapse = "|"), x = names(dat))) > 0
    cond1 <- (length(pattern) == 1 && pattern == "")
    cond2 <- (all(pattern != "") && header_contains_pattern)
    cond3 <- invert
    
    if (cond1 || cond2 || cond3) {
      if (length(files) == 1) {
        dat <- dat[2:.N, ]
      } else {
        # Assuming grep_count is a helper function in your package
        counts <- grep_count(files = files, pattern = pattern, invert = invert, 
                             ignore_case = ignore_case, fixed = fixed, 
                             recursive = recursive, word_match = word_match, 
                             only_matching = only_matching, header = header)
        dat <- dat[-c(1, 1 + counts[1:(.N - 1), cumsum(1 + count)])]
      }
    }
  }

  if (header == TRUE && "line_number" %in% names(dat)) {
    dat[, line_number := as.numeric(line_number) - 1]
  }
   
  # 8. Type Conversion (Match types to shallow.copy)
  data.types <- data.table::as.data.table(t(shallow.copy[, lapply(.SD, class)]), keep.rownames = TRUE)
  unique.types <- data.types[V1 != "character", unique(V1)]
  
  if (length(unique.types) > 0) {
    for (type in unique.types) {
      cols_to_fix <- data.types[V1 == type, rn]
      dat[, (cols_to_fix) := lapply(.SD, function(x) as(x, type)), .SDcols = cols_to_fix]
    }
  }
  
  # 9. Apply nrows constraint
  if (nrows < Inf) {
    dat <- dat[1:min(c(.N, nrows)), ]
  }
  
  return(dat[])
}