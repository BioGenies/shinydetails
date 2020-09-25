
#' @title Table widget
#' @description  \code{dt_format} is a function for creating a graphical
#' widget containing a table with download buttons (Excel and CSV).
#' @param dat Dataset. A matrix or data frame.
#' @param cols Names of columns to display.
#' @details This function uses \code{\link[DT]{datatable}}.
#' @export
#'

dt_format <- function(dat, cols = colnames(dat)) {
  datatable(data = dat,
            colnames = cols,
            class = "table-bordered table-condensed",
            extensions = "Buttons",
            options = list(pageLength = 10,
                           dom = "tBip",
                           autoWidth = TRUE,
                           buttons = c("excel", "csv")),
            filter = "bottom",
            rownames = FALSE)
}


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

generate_downloadButton <- function(id, plot_out, device) {
  downloadHandler(filename = paste0(id, "_plot.", device),
                  content = function(file){
                    ggsave(file, plot_out(), device = device, height = 300,
                           width = 400, units = "mm")})
}


#' @title Tooltip data
#' @description  Prepares data in order that it may be displayed in tooltip.
#' @param hv Hoover.
#' @param plot_out Reactive. Plot to extract data from.
#' @param plot_type Type of plot. Accepts either \code{'geom_point'}, \code{'geom_segment'}
#' or \code{'geom_col'}. Default \code{'geom_point'}.
#' @return \code{prepare_tt_data} returns prepared data frame containing one record.
#' @details This function filters one row of the imputed data in order that it
#' coresponds the most to the hoover coordinates.
#' @export


prepare_tt_data <- function(hv, plot_out, plot_type = "geom_point") {

  plot_data <- as.data.frame(plot_out()[["data"]])
  plot_data_info <- ggplot_build(plot_out())[["data"]][[1]]

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
#' @inheritParams prepare_tt_data
#' @param tt_content Content
#' @export
#'

generate_tooltip <- function(hv, plot_out, plot_type, tt_content) {

  tt_df <- prepare_tt_data(hv, plot_out, plot_type)

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

