# =============================================================================
# MERIDIAN - Report Registry Helpers
# Lightweight, user-driven report items for Module 7
# =============================================================================

make_report_item_id <- function(module, item_type, trait, name) {
  parts <- c(module, item_type, trait %||% "global", name)
  parts <- tolower(gsub("[^A-Za-z0-9]+", "_", parts))
  parts <- gsub("^_+|_+$", "", parts)
  paste0(paste(parts[nzchar(parts)], collapse = "__"), "__", substr(digest::digest(list(module, item_type, trait, name)), 1, 8))
}

make_dataset_signature <- function(db) {
  if (is.null(db) || is.null(db$data)) return(NULL)
  payload <- db[intersect(c("data", "gen_col", "env_col", "rep_col", "block_col", "row_col", "col_col", "traits", "design", "augmented_checks", "env_data"), names(db))]
  digest::digest(payload, algo = "xxhash64", serialize = TRUE)
}

register_report_plot <- function(registry, id, label, module, trait, plot_builder, metadata = list()) {
  item <- list(
    id = id,
    label = label,
    type = "plot",
    module = module,
    trait = trait %||% "Global",
    builder = frozen_builder(shiny::isolate(plot_builder())),
    metadata = metadata %||% list(),
    created_at = Sys.time(),
    input_snapshot = metadata$input_snapshot %||% list()
  )
  validate_report_item(item)
  registry[[id]] <- item
  invisible(item)
}

register_report_table <- function(registry, id, label, module, trait, table_builder, metadata = list()) {
  item <- list(
    id = id,
    label = label,
    type = "table",
    module = module,
    trait = trait %||% "Global",
    builder = frozen_builder(as.data.frame(shiny::isolate(table_builder()))),
    metadata = metadata %||% list(),
    created_at = Sys.time(),
    input_snapshot = metadata$input_snapshot %||% list()
  )
  validate_report_item(item)
  registry[[id]] <- item
  invisible(item)
}

unregister_report_item <- function(registry, id) {
  registry[[id]] <- NULL
  invisible(TRUE)
}

get_report_registry_items <- function(registry) {
  items <- tryCatch(
    shiny::reactiveValuesToList(registry),
    error = function(e) shiny::isolate(shiny::reactiveValuesToList(registry))
  )
  items[vapply(items, is.list, logical(1))]
}

get_report_plot_items <- function(registry) {
  items <- get_report_registry_items(registry)
  items[vapply(items, function(x) identical(x$type, "plot"), logical(1))]
}

get_report_table_items <- function(registry) {
  items <- get_report_registry_items(registry)
  items[vapply(items, function(x) identical(x$type, "table"), logical(1))]
}

validate_report_item <- function(item) {
  req_names <- c("id", "label", "type", "module", "trait", "builder", "metadata", "created_at")
  missing <- setdiff(req_names, names(item))
  if (length(missing) > 0) stop("Report item is missing: ", paste(missing, collapse = ", "))
  if (!item$type %in% c("plot", "table")) stop("Report item type must be 'plot' or 'table'.")
  if (!is.function(item$builder)) stop("Report item builder must be a zero-argument function.")
  TRUE
}

assert_report_item_dataset <- function(item, current_signature) {
  item_signature <- item$metadata$dataset_signature %||% NULL
  if (!is.null(item_signature) && !is.null(current_signature) && !identical(item_signature, current_signature)) {
    stop("This report item was created from a previous dataset. Please regenerate it.")
  }
  TRUE
}

clear_report_registry <- function(registry, type = c("all", "plot", "table")) {
  type <- match.arg(type)
  items <- get_report_registry_items(registry)
  for (id in names(items)) {
    if (identical(type, "all") || identical(items[[id]]$type, type)) {
      registry[[id]] <- NULL
    }
  }
  invisible(TRUE)
}
