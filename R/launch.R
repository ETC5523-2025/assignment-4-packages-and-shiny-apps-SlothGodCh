#' Launch the Penguin-Dash Shiny App
#'
#' This function launches the interactive Shiny dashboard for exploring the Palmer Penguins data.
#'
#' @return This function does not return a value; it is called for its side effect of
#'   launching the Shiny application.
#' @export
#'
#' @examples
#' \dontrun{
#'   launch_dashboard()
#' }
launch_dashboard <- function() {
  appDir <- system.file("shiny-app", package = "penguinDash")
  if (appDir == "") {
    stop("Could not find shiny-app directory. Try re-installing `penguinDash`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
