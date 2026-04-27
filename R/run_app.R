#' Run MERIDIAN
#'
#' Launches the MERIDIAN Shiny application.
#'
#' @param ... Options passed to [golem::with_golem_options()].
#'
#' @return A Shiny application object.
#' @export
run_app <- function(...) {
  golem::with_golem_options(
    app = shiny::shinyApp(ui = app_ui, server = app_server),
    golem_opts = list(...)
  )
}
