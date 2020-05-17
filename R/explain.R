#' Show details of the data frame
#'
#' \code{explainer} shows detail of all the columns of the data
#'
#' This function uses \code{explain} on each column.
#'
#' @param df A data.frame
#' @param ... parameters for explain
#' @return Prints details of the dataset which includes: dataset name,
#' type, number of columns, rows and unique rows. After this output of
#' \code{explain} is printed for all the columns
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

    # cat_ <- paste0(x, " (type: ", paste0(class(X), collapse = ", "), ")")
    # cat(paste0(cat_, "\n",
    #            paste0(rep(".", nchar(cat_)),collapse = ""),
    #            "\n"))

    explain(X, x, ...)

    linedivider(consolewidth)
  }, df, linedivider, consolewidth)

}

#' Draws a horizontal line on console
#'
#' @param consolewidth a integer
#' @param st a character or symbol of length to be used for creating the line
#' @return Prints a horizontal line of width 'consolewidth'
#' @examples
#' linedivider(20)
#' @export

linedivider <- function(consolewidth = getOption("width"), st = 'x'){
  if (nchar(st) != 1 | length(st) != 1) stop("'st' should of length 1")
  cat("\n")
  cat(paste0(rep(st, consolewidth),collapse = ""))
  cat("\n")
}


explain <- function(X, xname = NULL, ...) {
  if (is.null(xname)) xname <- deparse(substitute(X))
  cat_ <- paste0(xname, " (type: ", paste0(class(X), collapse = ", "), ")")
  cat(paste0(cat_, "\n",
             paste0(rep(".", nchar(cat_)),collapse = ""),
             "\n"))
  UseMethod("explain", X)
}

explain.numeric <- function(x, include.numeric = NULL, round.digit = 2, quant.seq = seq(0,1,0.2)) {
  # printing summary
  quant <- round(quantile(x, probs = quant.seq, names = T, na.rm = T), round.digit)
  uniqx <- length(unique(x))
  medi  <- median(x, na.rm = T)
  out <- data.frame(distinct = uniqx,
                    missing = sum(is.na(x)),
                    mean   = round(mean(x, na.rm = T), round.digit),
                    sd     = round(sd(x, na.rm = T), round.digit),
                    median = round(medi, round.digit),
                    t(quant)
                    , check.names = F, stringsAsFactors = F)

  # Adding extra information based on input
  if ("trimmed.means" %in% include.numeric) {
    out$`trimmed mean5%` <- round(mean(x, trim = 0.05, na.rm = T), round.digit)
  }
  if ("skewness" %in% include.numeric) {
    out$skewness <- round(skewness(x, na.rm = T), round.digit)
  }
  if ("kurtosis" %in% include.numeric) {
    out$kurtosis <- round(kurtosis(x, na.rm = T), round.digit)
  }

  blanks_to_add <- 1+max(0, max(nchar(out) - nchar(colnames(out))))
  names(out) <- paste0(paste0(rep(" ", blanks_to_add), collapse = ""), names(out))
  print(out, row.names = F)
  if (uniqx <= 10) {
    cat("\n Showing frequency table because variable has less distinct values: \n")
    print(freqTable(x), row.names = F)
  } else {
    consoleBoxplot(x)
  }
}

explain.character <- function(x, ...) {
  explain.factor(x, ...)
}

explain.factor <- function(x, ...) {
  out <- data.frame(distinct = length(unique(x)),
                    missing = sum(is.na(x))
                    , check.names = F, stringsAsFactors = F)
  print(out, row.names = F)
  cat("\n Frequency table: \n")
  print(freqTable(x), row.names = F)
}

freqTable <- function(Value) {
  out <- data.frame(table(Value), stringsAsFactors = F)
  out$Proportion <- out$Freq/length(Value)*100

  maxperc <- max(out$Proportion)
  if (maxperc < 50){
    maxperc <- ceiling(maxperc/25)*25
  } else {
    maxperc <- 100
  }

  bars <- unlist(lapply(out$Proportion, function(x, maxperc) {
    count = 100*round(x/2)/maxperc
    return(paste0("|",
                  paste0(rep("*", count),collapse = ""),
                  paste0(rep(".", 50-count),collapse = ""),
                  "|")
    )
  }, maxperc))

  out$Proportion <- round(out$Proportion/100, 3)
  out$' ' <- paste0(bars, " (", maxperc, "%)")

  return(out)
}


consoleBoxplot <- function(x) {
  cat("Box plot: \n")
  if (length(unique(x)) < 3) {
    cat("No boxplot for this as unique values are < 3.")
  } else {
    quant <- quantile(x, na.rm = T)
    box <- round((quant-quant[1])*round(0.9*getOption("width"))/(quant[5]-quant[1]))
    IQR <- box[4]-box[2]

    cat(paste0("|",
               paste0(rep(".", max(0, box[2]-2)),collapse = ""), "<",
               paste0(rep("=", max(0, box[3]-box[2]-1)),collapse = ""), "*",
               paste0(rep("=", max(0, box[4]-box[3]-1)),collapse = ""), ">",
               paste0(rep(".", max(0, box[5]-box[4]-1)),collapse = ""), "|\n"))

    cat("Legends: | min and max, <==  ==> IQR, * median \n")
    if (box[5] > box[4]+1.5*IQR) {
      cat("\nPotential outliers present in this variable\n")
    }
  }

}
