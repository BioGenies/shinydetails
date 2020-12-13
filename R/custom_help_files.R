
#' @title Custom help files
#' @description  This function creates help files with custom content.
#' @inheritParams shinyhelper::create_help_files
#' @param content A character string to display in the automatically created help file.
#' @details This function is based on \code{\link[shinyhelper]{create_help_files}}.
#'



create_help_files_custom <- function (files, help_dir = "helpfiles", content = "This helpfile is not finished yet.")
{
  if (!interactive()) {
    message("Must be called from an interactive session")
    return(invisible(files))
  }
  if (!dir.exists(help_dir)) {
    message("* Creating directory `", help_dir, "`.")
    dir.create(help_dir)
  }
  files <- file.path(help_dir, paste0(files, ".md"))
  new_file <- function(file) {
    if (!file.exists(file)) {
      file.create(file)
      writeLines(text = c(paste("###", file, "- Under Development\n"),
                          "***\n\n", content, "\n"),
                 con = file)
    }
    else {
      message("* `", file, "` already exists - not overwriting")
    }
  }
  lapply(files, new_file)
  return(invisible(files))
}
