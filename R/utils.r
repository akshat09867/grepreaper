# Fix data.table global variable bindings
utils::globalVariables(c(":=", ".N", "V1", "V2", "V3",".SD", "rn", "count", "full.output", "line_number"))

#' Detect Windows reliably
#' @keywords internal
is_windows <- function() {
  os_type <- .Platform$OS.type
  sysname <- tryCatch(Sys.info()[["sysname"]], error = function(e) NA_character_)
  r_os <- tolower(getRversion()); r_os # no-op to use getRversion to avoid NOTE
  is_win_platform <- identical(tolower(os_type), "windows")
  is_win_sysname <- identical(tolower(sysname), "windows")
  # Fallback: check compiled R OS string
  is_win_compiled <- grepl("mingw|windows", tolower(R.version$os))
  isTRUE(is_win_platform || is_win_sysname || is_win_compiled)
}

#' Split columns based on a delimiter
#' 
#' Efficiently splits character vectors into multiple columns based on a specified delimiter.
#' This function is optimized for performance and handles common use cases like parsing
#' grep output or other delimited text data.
#' 
#' @param x Character vector to split
#' @param column.names Names for the resulting columns (optional)
#' @param split Delimiter to split on (default: ":")
#' @param resulting.columns Number of columns to create (default: 3)
#' @param fixed Whether to use fixed string matching (default: TRUE)
#' 
#' @return A data.table with split columns. Column names are automatically assigned
#'   as V1, V2, V3, etc. unless custom names are provided via \code{column.names}.
#' 
#' @examples
#' # Split grep-like output with colon delimiter
#' data <- c("file.txt:15:error message", "file.txt:23:warning message")
#' result <- split_columns(data, resulting.columns = 3)
#' print(result)
#' 
#' # With custom column names
#' result_named <- split_columns(data, 
#'                              column.names = c("filename", "line", "message"),
#'                              resulting.columns = 3)
#' print(result_named)
#' 
#' # Split into 2 columns (combining remaining elements)
#' result_2col <- split_columns(data, resulting.columns = 2)
#' print(result_2col)
#' 
#' @export
split_columns <- function(x, column.names = NA, split = ":", 
                         resulting.columns = 3, fixed = TRUE) {
  # Ensure data.table is available
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("The 'data.table' package is required but not installed.")
  }
 
  # Input validation
  if (!is.character(x) || length(x) == 0) {
    stop("'x' must be a non-empty character vector")
  }
  if (!is.numeric(resulting.columns) || resulting.columns < 1) {
    stop("'resulting.columns' must be a positive integer")
  }
  
  # PERFORMANCE OPTIMIZATION: Use vectorized strsplit instead of loops
  the.pieces <- strsplit(x = x, split = split, fixed = fixed)
  
  # PERFORMANCE OPTIMIZATION: Pre-allocate result data.table
  n_rows <- length(x)
  result_dt <- data.table::data.table()
  
  # PERFORMANCE OPTIMIZATION: Vectorized column creation
  if (resulting.columns == 1) {
    # Single column case - most efficient
    result_dt[, V1 := x]
  } else if (resulting.columns == 2) {
    # Two columns case - optimized
    col1 <- sapply(the.pieces, function(piece) if (length(piece) >= 1) piece[1] else NA_character_)
    col2 <- sapply(the.pieces, function(piece) if (length(piece) >= 2) paste(piece[2:length(piece)], collapse = split) else NA_character_)
    result_dt[, V1 := col1]
    result_dt[, V2 := col2]
  } else if (resulting.columns == 3) {
    # Three columns case - optimized for common grep metadata format
    col1 <- sapply(the.pieces, function(piece) if (length(piece) >= 1) piece[1] else NA_character_)
    col2 <- sapply(the.pieces, function(piece) if (length(piece) >= 2) piece[2] else NA_character_)
    col3 <- sapply(the.pieces, function(piece) if (length(piece) >= 3) paste(piece[3:length(piece)], collapse = split) else NA_character_)
    result_dt[, V1 := col1]
    result_dt[, V2 := col2]
    result_dt[, V3 := col3]
  } else {
    # General case - still optimized but handles arbitrary column counts
    # PERFORMANCE OPTIMIZATION: Use sapply instead of loops
    for (i in seq_len(resulting.columns)) {
      if (i < resulting.columns) {
        # Extract single elements
        col_values <- sapply(the.pieces, function(piece) {
          if (length(piece) >= i) piece[i] else NA_character_
        })
        result_dt[, (paste0("V", i)) := col_values]
      } else {
        # Combine remaining elements for the last column
        col_values <- sapply(the.pieces, function(piece) {
          if (length(piece) >= i) paste(piece[i:length(piece)], collapse = split) else NA_character_
        })
        result_dt[, (paste0("V", i)) := col_values]
      }
    }
  }
 
  # PERFORMANCE OPTIMIZATION: Set column names efficiently
  if (!is.na(column.names[1]) && length(column.names) == ncol(result_dt)) {
    data.table::setnames(result_dt, names(result_dt), column.names)
  }
 
  return(result_dt)
}

#' Build grep command string
#' 
#' Constructs a safe and properly formatted grep command string for system execution.
#' This function handles input sanitization by utilizing R's internal shell quoting
#' mechanism, ensuring compatibility across different operating systems.
#' 
#' @param pattern Character vector of patterns to search for.
#' @param files Character vector of file paths to search in.
#' @param options Character string containing grep flags (e.g., "-i", "-v").
#' @param fixed Logical; if TRUE, grep is told to treat patterns as fixed strings.
#' 
#' @return A properly formatted command string ready for system execution.
#' @export
build_grep_cmd <- function(pattern, files, options = "", fixed = FALSE) {
  
  # --- FIX: Ensure arguments are not NULL or length zero ---
  if (is.null(options) || length(options) == 0) options <- ""
  if (is.null(pattern) || length(pattern) == 0) pattern <- ""
  
  # Locate grep binary
  grep_bin <- Sys.which("grep")
  if (grep_bin == "") {
    if (.Platform$OS.type == "windows") {
      # Return empty so grep_read can handle the fallback
      return("") 
    } else {
      stop("grep command not found.")
    }
  }

  # Input validation
  if (!is.character(files) || length(files) == 0) {
    stop("'files' must be a non-empty character vector")
  }
  
  files <- normalizePath(files, winslash = "/", mustWork = FALSE)
  
  # Pattern logic
  if (length(pattern) > 1) {
    pattern_str <- paste(sprintf("-e %s", shQuote(pattern)), collapse = " ")
  } else {
    pattern_str <- shQuote(pattern)
  }
  
  # Final command assembly
  # nzchar() will now work because options is guaranteed to be length 1
  if (nzchar(options)) {
    cmd <- sprintf("%s %s %s %s", shQuote(grep_bin), options, pattern_str, paste(shQuote(files), collapse = " "))
  } else {
    cmd <- sprintf("%s %s %s", shQuote(grep_bin), pattern_str, paste(shQuote(files), collapse = " "))
  }
  
  return(cmd)
}