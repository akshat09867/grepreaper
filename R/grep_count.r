#' grep_count: Efficiently count the number of relevant records from one or more files using grep
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
#' @param only_matching Logical; if TRUE, return only the matching part of the
#'   lines.
#' @param skip Integer; number of rows to skip.
#' @param header Logical; if TRUE, treat first row as header.
#' @param include_filename Logical; if TRUE, include source filename as a column.
#' @param show_progress Logical; if TRUE, show progress indicators.
#' @param ... Additional arguments passed to fread.
#' @return A data.table containing file names and counts.
#' @importFrom data.table fread setnames data.table as.data.table rbindlist setorder setcolorder ":=" .N
#' @export

grep_count <- function(files = NULL, path = NULL, file_pattern = NULL, 
                       pattern = "", invert = FALSE, ignore_case = FALSE, 
                       fixed = FALSE, recursive = FALSE, word_match = FALSE, 
                       only_matching = FALSE, skip = 0, header = TRUE, 
                       include_filename = FALSE, show_cmd = FALSE, 
                       show_progress = FALSE, ...){
  
  header.name.file <- "file"
  header.name.count <- "count"
  
  if (is.null(files) && !is.null(path)) {
    files <- list.files(path = path, pattern = file_pattern, full.names = TRUE, 
                        recursive = recursive, ignore.case = ignore_case)
  }
  if (!is.null(files)) {
    files <- normalizePath(files, winslash = "/", mustWork = FALSE)
  }

  count_only_flag <- TRUE 
  need_metadata <- TRUE 
  

  opts_vec <- c("-v", "-i", "-F", "-r", "-w", "-o", "-c", "-H")
  active_opts <- c(invert, ignore_case, fixed, recursive, word_match, only_matching, count_only_flag, need_metadata)
  options_str <- paste(opts_vec[active_opts], collapse = " ")
  
  cmd <- build_grep_cmd(pattern = pattern, files = files, options = options_str, fixed = fixed)

  if(show_cmd == TRUE){
    return(cmd)
  }
  
  if (cmd == "") {
    if (.Platform$OS.type == "windows") {
      warning("grep utility not found. Returning 0 counts.")
      return(data.table::data.table(file = files, count = 0))
    }
    stop("System command 'grep' not found.")
  }

  dat <- tryCatch({
    data.table::fread(cmd = cmd, header = FALSE, skip = skip, showProgress = show_progress, ...)
  }, warning = function(w) {
    data.table::data.table()
  }, error = function(e) {
    data.table::data.table()
  })
  

  if (nrow(dat) == 0) {
    res <- data.table::data.table(file = files, count = 0)
    if (!include_filename) res[, file := NULL]
    return(res[])
  }
  
  new.names <- sprintf("V%d", 1:ncol(dat))
  data.table::setnames(x = dat, old = names(dat), new = new.names)
  
  cmd.paste.text <- sprintf("paste(%s)", paste(sprintf("%s", new.names), collapse = ","))
  dat[, full.output := eval(expr = parse(text = cmd.paste.text))]
  
  column.names <- c(header.name.file, header.name.count)
  
  res <- split_columns(x = dat[, full.output], column.names = column.names, resulting.columns = length(column.names))
  res[, count := as.numeric(count)]
  
  if(header == TRUE){

    subtract_header <- tryCatch({
       if (length(files) > 0) {
         first_line <- data.table::fread(files[1], nrows = 0, header = TRUE) 
         col_names <- names(first_line)
         
         if(pattern == "") {
           1 
         } else {
           if (any(grepl(pattern, col_names, ignore.case = ignore_case, fixed = fixed))) 1 else 0
         }
       } else { 0 }
    }, error = function(e) 0) 
    
    res[, count := count - subtract_header]
    res[count < 0, count := 0]
  }

  if(include_filename == FALSE & header.name.file %in% names(res)){
    res[, file := NULL]
  }
  
  return(res[])
}