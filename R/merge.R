#' Analyze data for merging
#'
#' \code{mergeAnalyzer} analyzes the data drop after merge
#'
#' Prints the summary of data retained after merge and returns the merged data.
#' This function uses data.table (if the package is installed) for faster data
#' merge
#'
#' @param x left data to merge
#' @param y right data to merge
#' @param round.digit integer indicating the number of decimal places to be used
#' @param ... other parameters needs to be passed to merge function
#'
#' @return Returns merged data with same class as that of input data.
#' Prints summary of data retained after merging.
#' The summary has 6 columns:
#' \itemize{
#'  \item Column: Number of rows and union of column names of numeric columns
#'  in both the data
#'  \item x, y: Sum of columns in both table
#'  \item Merged: Sum of columns in merged data
#'  \item remainingWRTx: ratio of remaining data in merged data after merging.
#'  example - 0.5 means that 50% data was dropped after merging (probably in case
#'  of inner join). 1.5 means value became 150% of original sum because of some
#'  duplicates present in data
#'  \item remainingWRTy: same as above, but for y table
#' }
#'
#' @examples
#' # Creating two tables to merge
#' A <- data.frame(id = c("A", "A", "B", "D", "B"),
#'                 valA = c(30, 83, 45, 2, 58))
#'
#' B <- data.frame(id = c("A", "C", "A", "B", "C", "C"),
#'                 valB = c(10, 20, 30, 40, 50, 60))
#'
#' mergeAnalyzer(A, B, allow.cartesian = TRUE, all = FALSE)
#'
#' @export
mergeAnalyzer <- function(x,
                          y,
                          round.digit = 2,
                          ...) {

  cl <- class(x)

  if (!requireNamespace("data.table", quietly = TRUE)) {
    warning("Please install data.table for this function. Returning NULL")
    return(NULL)
  }

  if (!"data.table" %in% cl) {
    x <- data.table::as.data.table(x)
    y <- data.table::as.data.table(y)
  }

  args <- list(...)
  byx <- intersect(colnames(x), colnames(y))
  byy <- byx
  if ("by.x" %in% names(args)) {
    byx <- unlist(args["by.x"],  use.names = FALSE)
  }
  if ("by.y" %in% names(args)) {
    byy <- unlist(args["by.y"],  use.names = FALSE)
  }
  if ("by" %in% names(args)) {
    byx <- unlist(args["by"],  use.names = FALSE)
    byy <- byx
  }

  checkduplicatedeformerge(x, byx)
  checkduplicatedeformerge(y, byy)

  mergedO <- merge(x, y, ...)

  # finding the summary
  x <- getSummaryformerge(x, "x", round.digit)
  y <- getSummaryformerge(y, "y", round.digit)
  merged <- getSummaryformerge(mergedO, "Merged", round.digit)

  out <-
    Reduce(function(x, y)
      merge(x, y, by = "Column", all = T), list(x, y, merged))

  remainingWRTx <- round(out$Merged / out$x, round.digit)
  remainingWRTy <- round(out$Merged / out$y, round.digit)

  out$remainingWRTx <-
    ifelse(remainingWRTx > 1, paste0(remainingWRTx, " *"), remainingWRTx)
  out$remainingWRTy <-
    ifelse(remainingWRTy > 1, paste0(remainingWRTy, " *"), remainingWRTy)

  out[is.na(out)] <- "-"
  # print(out)
  message(paste0(capture.output(out), collapse = "\n"))

  if (any(remainingWRTx > 1 | remainingWRTy > 1)) {
    message(
      "\n*:increased because either x, y or both has
      duplicates at the level of data merging\n\n"
    )
  }
  message(
    "   'remainingWRTx' shows the proportion of total sum of column values
    left after the data merge with respect to x.
    See the documentation (?mergeAnalyzer) for more details\n"
  )

  if (!"data.table" %in% cl) {
    mergedO <- as.data.frame(mergedO)
  }
  return(mergedO)
}


getSummaryformerge <- function(tb,
                               name,
                               round.digit = 2) {
  numrows <- nrow(tb)
  numvars <- which(sapply(tb, is.numeric))

  if (length(numvars) > 0) {
    if (length(numvars) == 1) {
      tb <- data.table::as.data.table(tb[, numvars, with = FALSE])
      colnames(tb) <- names(numvars)
    } else {
      tb <- tb[, numvars, with = FALSE]
    }
    tb <- data.frame(sapply(tb, function(x){sum(x, na.rm = T)}))
    numvars <- row.names(tb)
    tb <- data.table::as.data.table(tb)
    tb$Column <- numvars
    colnames(tb)[1] <- "V1"
  } else {
    tb <- as.data.table(matrix(NA, nrow=0, ncol=2, dimnames = list(NULL, c("V1", "Column"))))
  }
  tb <- base::rbind(data.table::as.data.table(data.frame(V1 = numrows, Column = "No. of Rows")), tb)
  colnames(tb)[1] <- name
  tb[,1] <- round(tb[,1], round.digit)
  return(tb)
}


checkduplicatedeformerge <- function(tb,
                                     by) {
  uniqCount <- data.table::uniqueN(tb, by = by)

  if (uniqCount < nrow(tb)) {
    mssg <- paste0(
      deparse(substitute(tb)),
      " is not unique (when checked for uniqueness using the combinations
      of columns which are being used for data merge - ",
      paste0(by, collapse = ", "),
      "). ",
      "This will repeat the values present in the other table and increase
      the total sum of values present in the columns ",
      "(which are marked using '*' in columns 'remainingWRTx' and 'remainingWRTy')"
    )
    warning(mssg)
  }

}
