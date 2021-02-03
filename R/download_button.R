#' @title Download button for plot
#' @description  Generates download button for a plot with dimensions of 300x400mm.
#' @param id Unique id of download button.
#' @param plot_out Plot to save.
#' @inheritParams ggplot2::ggsave
#' @return \code{generate_downloadButton} returns the output of \code{downloadHandler}
#' for given plot and device.
#' @details This function uses \code{\link[shiny]{downloadHandler}} and \code{\link[ggplot2]{ggsave}}.
#' @export
#' @importFrom ggplot2 ggsave

beam_downloadButton <- function(id, plot_out, device) {
  downloadHandler(filename = paste0(id, "_plot.", device),
                  content = function(file){
                    ggsave(file, plot_out, device = device, height = 300,
                           width = 400, units = "mm")})
}
