
#' @title Tooltip data
#' @description  Prepares data in order that it may be displayed in tooltip.
#' @param hv Hoover.
#' @param plot_obj Plot to extract data from.
#' @param plot_type Type of plot. Accepts either \code{'geom_point'}, \code{'geom_segment'}
#' or \code{'geom_col'}. Default \code{'geom_point'}.
#' @return \code{prepare_tt_data} returns prepared data frame containing one record.
#' @details This function filters one row of the imputed data in order that it
#' coresponds the most to the hoover coordinates.
#' @export


produce_tt_data <- function(hv, plot_obj, plot_type = "geom_point") {

  plot_data <- as.data.frame(plot_obj[["data"]])
  plot_data_info <- ggplot_build(plot_obj)[["data"]][[1]]

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
           tt_df <- nearPoints(df = plot_data, coordinfo = hv, maxpoints = 1)
         },
         geom_segment = {
           hv_data <- data.frame(x = hv[["x"]],
                                 y = hv[["y"]],
                                 x_start = plot_data[[hv[["mapping"]][["x"]]]],
                                 x_end = plot_data[[hv[["mapping"]][["xend"]]]],
                                 y_plot = plot_data[[hv[["mapping"]][["y"]]]],
                                 plot_data)
           tt_df <- hv_data[hv_data[["x"]] > hv_data[["x_start"]] &
                              hv_data[["x"]] < hv_data[["x_end"]] &
                              abs(hv_data[["y_plot"]] - hv_data[["y"]]) < 10 &
                              abs(hv_data[["y_plot"]] - hv_data[["y"]]) == min(abs(hv_data[["y_plot"]] - hv_data[["y"]])),
                            !(colnames(hv_data) %in% c("x", "y"))]
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

spark_tooltip <- function(hv, plot_obj, plot_type, tt_content) {

  tt_df <- produce_tt_data(hv, plot_obj, plot_type)

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

