# Check the installed package over HTTP without relying on a Unix shell.
smoke_installed_app <- function() {
  log <- tempfile("meridian-startup-", fileext = ".log")
  port <- httpuv::randomPort()
  process <- callr::r_bg(function(port) {
    shiny::runApp(MERIDIAN::run_app(), host = "127.0.0.1", port = port,
                  launch.browser = FALSE)
  }, args = list(port = port), stdout = log, stderr = log, supervise = TRUE)
  on.exit(process$kill(), add = TRUE)
  base_url <- paste0("http://127.0.0.1:", port)
  fetch <- function(path) {
    dest <- tempfile()
    status <- suppressWarnings(tryCatch(utils::download.file(
      paste0(base_url, path), dest, quiet = TRUE, mode = "wb", method = "libcurl"
    ), error = function(e) 1L))
    if (!identical(status, 0L) || !file.exists(dest)) return(NULL)
    readBin(dest, "raw", n = file.info(dest)$size)
  }
  deadline <- Sys.time() + 60
  html <- NULL
  repeat {
    html <- fetch("/")
    if (!is.null(html)) break
    if (!process$is_alive() || Sys.time() >= deadline) {
      stop(paste(c("Installed app did not start.", readLines(log, warn = FALSE)), collapse = "\n"))
    }
    Sys.sleep(0.25)
  }
  stopifnot(grepl("MERIDIAN", rawToChar(html), fixed = TRUE))
  css <- fetch("/assets/styles.css")
  logo <- fetch("/assets/logo.png")
  stopifnot(length(css) > 100L, length(logo) > 8L,
    identical(logo[1:8], as.raw(c(137, 80, 78, 71, 13, 10, 26, 10))))
  message("Installed MERIDIAN app, stylesheet and logo passed on ", Sys.info()[["sysname"]], ".")
}
smoke_installed_app()
