

mergeAnalyzer <- function(x, y, ...) {

  cl <- class(x)

  if (!"data.table" %in% cl) {
    x <- data.table::as.data.table(x)
    y <- data.table::as.data.table(y)
  }

  args <- list(...)
  byx <- intersect(colnames(x), colnames(y))
  byy <- byx
  if ("by.x" %in% names(args)) { byx <- unlist(args["by.x"],  use.names=FALSE) }
  if ("by.y" %in% names(args)) { byy <- unlist(args["by.y"],  use.names=FALSE) }
  if ("by" %in% names(args)) {
    byx <- unlist(args["by"],  use.names=FALSE)
    byy <- byx
  }

  checkduplicatedeformerge(x, byx)
  checkduplicatedeformerge(y, byy)

  mergedO <- merge(x, y, ...)

  # finding the summary
  x <- getSummaryformerge(x, "x")
  y <- getSummaryformerge(y, "y")
  merged <- getSummaryformerge(mergedO, "Merged")

  out <- Reduce(function(x, y) merge(x, y, by = "Column", all=T), list(x, y, merged))

  remainingWRTx <- round(out$Merged/out$x, 3)
  remainingWRTy <- round(out$Merged/out$y, 3)

  out$remainingWRTx <- ifelse(remainingWRTx > 1, paste0(remainingWRTx, " *"), remainingWRTx)
  out$remainingWRTy <- ifelse(remainingWRTy > 1, paste0(remainingWRTy, " *"), remainingWRTy)

  out[is.na(out)] <- "-"
  print(out)

  if(any(remainingWRTx > 1 | remainingWRTy > 1)) {
    cat("\n*: increased because either x, y or both has duplicates at the level of data merging\n")
  }
  cat("'remainingWRTx' shows the proportion of total sum of column values left after the data merge with respect to x.
      See the documentation (?mergeAnalyzer) for more details\n")

  if (!"data.table" %in% cl){
    mergedO <- as.data.frame(mergedO)
  }
  return(mergedO)
}


getSummaryformerge <- function(tb, name) {

  numrows = nrow(tb)
  numvars <- which(sapply(tb, is.numeric))
  tb <- tb[, numvars, with = FALSE]
  tb <- t(tb[, lapply(.SD, sum, na.rm=TRUE)])
  numvars <- row.names(tb)
  tb <- data.table(tb)
  tb$Column <- numvars
  tb <- rbind(data.table(V1 = numrows, Column = "# Rows"), tb)
  colnames(tb)[1] <- name
  return(tb)

}


checkduplicatedeformerge <- function(tb, by) {

  uniqCount <- data.table::uniqueN(tb, by = by)

  if (uniqCount < nrow(tb)) {
    mssg <- paste0(deparse(substitute(tb)),
               " is not unique (when checked for uniqueness using the combinations of columns which are being used for data merge - ",
               paste0(by, collapse = ", "), "). ",
               "This will repeat the values present in the other table and increase the total sum of values present in the columns ",
               "(which are marked using '*' in columns 'remainingWRTx' and 'remainingWRTy')")
    warning(mssg)
  }

}

