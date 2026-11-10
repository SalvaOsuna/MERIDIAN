# Results remain available only for the exact data and model specification fitted.
analysis_event <- function(event, key, value, cache_size = 2L) {
  event_expr <- substitute(event); key_expr <- substitute(key); value_expr <- substitute(value)
  caller <- parent.frame()
  cache <- list()
  current_key <- shiny::reactive(eval(key_expr, caller))
  stored <- shiny::eventReactive(eval(event_expr, caller), {
    k <- current_key()
    cache_id <- digest::digest(k, algo = "xxhash64")
    if (cache_size > 0L && !is.null(cache[[cache_id]])) return(cache[[cache_id]])
    result <- eval(value_expr, caller)
    if (!is.null(result)) {
      attr(result, "analysis_key") <- k
      attr(result, "analysis_time") <- Sys.time()
    }
    entry <- list(key = k, value = result)
    if (cache_size > 0L && !is.null(result)) {
      cache[[cache_id]] <<- entry
      cache <<- utils::tail(cache, cache_size)
    }
    entry
  })
  shiny::reactive({
    k <- current_key()
    old <- tryCatch(stored(), shiny.silent.error = function(e) NULL)
    if (is.null(old) || !identical(old$key, k)) return(NULL)
    old$value
  })
}

require_valid_bundle <- function(db) {
  shiny::req(db)
  check <- validate_met_data(db$data, db$gen_col, db$env_col, db$rep_col, db$traits, db$block_col)
  shiny::validate(shiny::need(check$valid, paste(check$errors, collapse = " ")))
  db
}

frozen_builder <- function(value) {
  force(value)
  function() value
}

native_kernel_available <- function(name) {
  if (!exists(name, mode = "function", inherits = TRUE)) return(FALSE)
  is.loaded(paste0("_MERIDIAN_", name), PACKAGE = "MERIDIAN")
}
