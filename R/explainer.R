#' Show details of the data frame
#'
#' \code{explainer} shows detail of all the columns of the data
#'
#' This function uses \code{explain} on each column.
#'
#' @param df A data.frame
#' @param ... parameters for explain
#' @return Prints details of the dataset which includes: dataset name,
#' type, number of columns, rows and unique rows. Also prints output of
#' \code{explain} for all the columns. Returns nothing.
#'
#' @examples
#' explainer(mtcars)
#'
#' @export

explainer <- function(df, ...) {
  if (requireNamespace("data.table", quietly = TRUE)) {
    uniqRow <- data.table::uniqueN(df)
  } else {
    uniqRow <- nrow(unique(df))
  }
  consolewidth <- getOption("width")
  dataname     <- deparse(substitute(df))
  cat(paste0("Data: ", dataname,
             "\nType: ", paste0(class(df), collapse = ", "),
             "\n\nNumber of columns: ", ncol(df),
             "\nNumber of rows: ", nrow(df),
             "\nNumber of distinct rows: ", uniqRow,
             "\n"))
  linedivider(consolewidth)

  df <- as.data.frame(df)
  # showing each variable
  summ <- lapply(names(df), function(x, df, linedivider, consolewidth) {
    X <- df[,x]
    explain(X, x, ...)
    linedivider(consolewidth)
  }, df, linedivider, consolewidth)

}


