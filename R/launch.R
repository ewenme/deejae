#' Run contained Shiny app in a web browser
#'
#' \code{deejae} provides an interactive tool to explore DJ
#' sets visually. The tool will be launched in a web browser.
#' @export
launch <- function() {
  shiny::runApp(system.file("shiny", package = "deejae"),
                display.mode = "normal",
                launch.browser = TRUE)
}
