#' @title An example component
#' @description  Run an example app using \code{shinydetails}
#' @importFrom shiny runApp
#' @export
run_example_app <- function() {
  runApp(system.file("example-app", package = "shinyKrysia"))
}
