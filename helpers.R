
new_df = function(...) {
  data.frame(
    ...,
    row.names = NULL,
    check.rows = FALSE,
    check.names = TRUE,
    fix.empty.names = TRUE,
    stringsAsFactors = FALSE
  )
}


safely = function(expr) {
  warnings_ = list()
  error_ = NULL
  result = NULL
  
  result = withCallingHandlers({
    tryCatch({ 
      expr
    }, error = \(e) {
      error_ <<- e
    })
  }, warning = \(w) {
    warnings_[[length(warnings_) + 1L]] <<- w
  })
  
  if (length(warnings_) == 0L)
    warnings_ = NULL
  
  if (!is.null(error_))
    result = NULL
  
  list(
    error = error_,
    warnings = warnings_,
    result = result
  )
}


create_folder_if_it_doesnt_exist = function(folder = "simulations") {
  stopifnot(exprs = {
    vek::is_chr_vec_xb1(folder)
    !endsWith(folder, "/")
    !endsWith(folder, "\\")
    !endsWith(folder, "\\\\")
  })
  
  if (!dir.exists(folder)) {
    dir.create(folder, showWarnings = TRUE, recursive = FALSE)
    cli::cli_alert_info(sprintf("Created folder: '%s'", folder))
  }
}


elipsis = function(x, max_len = 64L) {
  stopifnot(exprs = {
    vek::is_int_vec_x1(max_len)
    max_len > 0L
  })
  
  if (nchar(x, "chars", FALSE, NA) > max_len)
    return(sprintf("%s...", substr(x, 1L, max_len)))
  else
    return(x)
}