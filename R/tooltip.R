
#' @title Tooltip data
#' @description  Prepares data in order that it may be displayed in tooltip.
#' The row from the data which fits to the hover the most will be selected.
#' @param hv Hoover.
#' @param plot_info chosen mapping from the \code{data} object from the output of \code{\link[ggplot2]{ggplot_build}} for displayed plot.
#' @param plot_data data on which the ggplot is based
#' @param plot_type Type of plot corresponding to the data from first mapping.
#' Accepts either \code{'geom_point'}, \code{'geom_segment'}
#' or \code{'geom_col'}. Default \code{'geom_point'}.
#' @param tt_range A number denoting maximum distance between hoover and objects on the plot.
#' The row, whose distance to the hoover is less than \code{tt_range} will be selected from the whole data. Default 5.
#' @return \code{prepare_tt_data} returns prepared data frame containing one record.
#' @details This function filters one row of the imputed data in order that it
#' corresponds the most to the hoover coordinates.
#' @export


extract_tt_data_row <- function(hv, plot_info, plot_data, plot_type = "geom_point", tt_range = 5) {

  match.arg(plot_type,  c("geom_col", "geom_point", "geom_segment"), several.ok = FALSE)

  switch(plot_type,
         geom_col = {
           hv_data <- data.frame(x = hv[["x"]],
                                 y = hv[["y"]],
                                 xmin = plot_info[["xmin"]],
                                 xmax = plot_info[["xmax"]],
                                 plot_data)
           tt_df <- hv_data[hv_data[["x"]] < hv_data[["xmax"]] &
                              hv_data[["x"]] >= hv_data[["xmin"]],
                            !(colnames(hv_data) %in% c("x", "y", "xmax", "xmin"))]
         },
         geom_point = {
           tt_df <- nearPoints(df = plot_data, coordinfo = hv, maxpoints = 1, threshold = tt_range)
         },
         geom_segment = {
           x_start <- plot_info[["x"]]
           x_end <- plot_info[["xend"]]
           y_start <- plot_info[["y"]]
           y_end <- plot_info[["yend"]]
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
#' @description  Generates tooltip for plot.
#' @inheritParams extract_tt_data_row
#' @param tt_content Optional. If \code{NULL} in the tooltip will be displayed names of
#' columns with corresponding values from data. One can customize tooltip content
#' adding parameter \code{tt_content} here. It should be a list of \code{chosen_cols}
#' and \code{row_text}. To display some values from the data in the content one
#' should reference to the relevant column from the \code{chosen_cols} and in
#' \code{row_text} write appropriate type of that variable like in the function
#' \code{\link[base]{sprintf}} (for example \code{'\%s'} in case when chosen variable
#' is a character string).
#' @details This function uses \code{\link[shinydetails]{extract_tt_data_row}}.
#' @export
#'

beam_tooltip <- function(hv, plot_info, plot_data, plot_type, tt_content, tt_range = 5) {

  tt_df <- extract_tt_data_row(hv, plot_info, plot_data, plot_type, tt_range)

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

