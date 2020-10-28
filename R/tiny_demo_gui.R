#' @title An example component
#' @description  Run an example app using \code{shinydetails}
#' @importFrom shiny runApp
#' @export
run_tiny_demo <- function() {
  runApp(system.file("tiny_demo", package = "shinydetails"))
}
