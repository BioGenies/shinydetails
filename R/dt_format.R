
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
