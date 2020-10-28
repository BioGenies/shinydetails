
#' @title Tooltip data
#' @description  Prepares data in order that it may be displayed in tooltip.
#' @param hv Hoover.
#' @param plot_obj ggplot object
#' @param plot_data data on which the ggplot is based
#' @param plot_type Type of plot. Accepts either \code{'geom_point'}, \code{'geom_segment'}
#' or \code{'geom_col'}. Default \code{'geom_point'}.
#' @param tt_range A number denoting maximum distance between hoover and objects on the plot.
#' The row, whose distance to the hoover is less than \code{tt_range} will be selected from the whole data. Default 5.
#' @return \code{prepare_tt_data} returns prepared data frame containing one record.
#' @details This function filters one row of the imputed data in order that it
#' corresponds the most to the hoover coordinates.
#' @export


produce_tt_data <- function(hv, plot_obj, plot_data, plot_type = "geom_point", tt_range = 5) {

  plot_data_info <- ggplot_build(plot_obj)[["data"]]

  if(length(plot_data_info) == 2) {
    plot_data_info <- plot_data_info[[2]]
  }else {
    plot_data_info <- plot_data_info[[1]]
  }

  switch(plot_type,
         geom_col = {
           hv_data <- data.frame(x = hv[["x"]],
                                 y = hv[["y"]],
                                 xmin = plot_data_info[["xmin"]],
                                 xmax = plot_data_info[["xmax"]],
                                 plot_data)
           tt_df <- hv_data[hv_data[["x"]] < hv_data[["xmax"]] &
                              hv_data[["x"]] >= hv_data[["xmin"]],
                            !(colnames(hv_data) %in% c("x", "y", "xmax", "xmin"))]
         },
         geom_point = {
           tt_df <- nearPoints(df = plot_data, coordinfo = hv, maxpoints = 1, threshold = tt_range)
         },
         geom_segment = {
           x_start <- plot_data_info[["x"]]
           x_end <- plot_data_info[["xend"]]
           y_start <- plot_data_info[["y"]]
           y_end <- plot_data_info[["yend"]]
           A <- y_start - y_end
           B <- x_end - x_start
           C <- y_end*(x_start - x_end) - x_end*(y_start - y_end)

           hv_data <- data.frame(dist = abs((hv[["x"]]*A +  hv[["y"]]*B + C))/sqrt(A^2 + B^2),
                                 plot_data)

           tt_df <- hv_data[x_start - tt_range <= hv[["x"]] &
                              x_end + tt_range >= hv[["x"]] &
                              y_start - tt_range <= hv[["y"]] &
                              y_end + tt_range >= hv[["y"]] &
                              hv_data[["dist"]] <= tt_range &
                              hv_data[["dist"]] == min(hv_data[["dist"]]),
                            !(colnames(hv_data) %in% c("dist"))]
         }
  )
  tt_df
}


#' @title Generate tooltip
#' @description  Generates tooltip .
#' @inheritParams produce_tt_data
#' @param tt_content Content
#' @export
#'

beam_tooltip <- function(hv, plot_obj, plot_data, plot_type, tt_content, tt_range = 5) {

  tt_df <- produce_tt_data(hv, plot_obj, plot_data, plot_type, tt_range)

  if(nrow(tt_df) != 0) {
    tt_pos_adj <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
                         "left", "right")

    tt_pos <- ifelse(hv[["coords_img"]][["x"]]/hv[["range"]][["right"]] < 0.5,
                     hv[["coords_css"]][["x"]],
                     hv[["range"]][["right"]]/hv[["img_css_ratio"]][["x"]] - hv[["coords_css"]][["x"]])

    style <- paste0("position:absolute; z-index:1000; background-color: rgba(245, 245, 245, 1); pointer-events: none;",
                    tt_pos_adj, ":", tt_pos,
                    "px; top:", hv[["coords_css"]][["y"]], "px; padding: 0px;")

    if(is.null(tt_content)) {
      content <- paste(colnames(tt_df), ": ", t(tt_df ), c(rep("<br/>", ncol(tt_df)-1), ""))
    }else {
      text <- paste(tt_content[["row_text"]],
                    c(rep(" <br/> ", length(tt_content[["row_text"]]) - 1), ""),
                    sep = "", collapse = "")
      content <- do.call(sprintf, c(list(text), lapply(tt_content[["chosen_cols"]], function(i) tt_df[[i]])))
    }

    div(style = style,
        p(HTML(content))
    )
  }
}

