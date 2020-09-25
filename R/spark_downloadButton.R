#' @title Download button
#' @description  Generates download button.
#' @param id Id name of tabset panel.
#' @param plot_out Reactive. Plot to save.
#' @param device Parameter from \code{\link[ggplot2]{ggsave}}.
#' @return \code{generate_downloadButton} returns the output of \code{downloadHandler}
#' for given plot and device.
#' @details This function uses \code{\link[shiny]{downloadHandler}}.
#' @export
#' @importFrom ggplot2 ggsave

spark_downloadButton <- function(id, plot_out, device) {
  downloadHandler(filename = paste0(id, "_plot.", device),
                  content = function(file){
                    ggsave(file, plot_out(), device = device, height = 300,
                           width = 400, units = "mm")})
}
