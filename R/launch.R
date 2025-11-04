#' Launch the Penguin-Dash Shiny App
#'
#' Launches an interactive Shiny dashboard for exploring the Palmer Penguins dataset.
#'
#' @return No return value; opens the app in a browser.
#' @importFrom shiny runApp
#' @importFrom dplyr filter pull
#' @importFrom ggplot2 ggplot aes geom_point
#' @importFrom shinythemes shinytheme
#' @export
#'
#' @examples
#' \dontrun{
#' launch_dashboard()
#' }
launch_dashboard <- function() {
  appDir <- system.file("shiny-app", package = "penguinDash")
  if (appDir == "") {
    stop("Shiny app directory not found. Reinstall penguinDash.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
