#' Show details of the data frame
#'
#' @param df A data.frame
#' @return Prints summary of the dataset
#' @examples
#' explainer(mtcars)
#' @export

explainer <- function(df) {
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
             "\nNumber of unique rows: ", uniqRow,
             "\n"))
  linedivider(consolewidth)

  df <- as.data.frame(df)
  # showing each variable
  summ <- lapply(names(df), function(x, df, linedivider, consolewidth) {
    X <- df[,x]

    cat_ <- paste0(x, " (type: ", paste0(class(X), collapse = ", "), ")")
    cat(paste0(cat_, "\n",
               paste0(rep(".", nchar(cat_)),collapse = ""),
               "\n"))

    explain(X)

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


explain <- function(X, ...) {
  UseMethod("explain", X)
}

explain.numeric <- function(x) {
  # printing summary
  quant <- round(quantile(x, probs = seq(0,1,0.2), names = T, na.rm = T),4)
  uniqx <- length(unique(x))
  medi  <- median(x, na.rm = T)
  out <- data.frame(unique = uniqx,
                    missing = sum(is.na(x)),
                    mean   = round(mean(x, na.rm = T),4),
                    sd     = round(sd(x, na.rm = T), 4),
                    'trimmed_mean5%' = round(mean(x, trim = 0.05, na.rm = T), 4),
                    median = round(medi, 4),
                    t(quant)
                    , check.names = F, stringsAsFactors = F)

  names(out) <- paste0("     ", names(out))
  print(out, row.names = F)
  if (uniqx <= 10) {
    cat("\n Showing frequency table because variable has less unique values: \n")
    print(freqTable(x), row.names = F)
  } else {
    consoleBoxplot(x)
  }
}

explain.character <- function(x) {
  print("This is a character")
}

explain.factor <- function(x) {
  out <- data.frame(unique = length(unique(x)),
                    missing = sum(is.na(x))
                    , check.names = F, stringsAsFactors = F)
  print(out, row.names = F)
  cat("\n Frequency table: \n")
  print(freqTable(x), row.names = F)
}

freqTable <- function(Value) {
  out <- data.frame(table(Value), stringsAsFactors = F)
  out$Percent <- out$Freq/length(Value)*100

  maxperc <- max(out$Percent)
  if (maxperc < 50){
    maxperc <- ceiling(maxperc/25)*25
  } else {
    maxperc <- 100
  }

  bars <- unlist(lapply(out$Percent, function(x, maxperc) {
    count = 100*round(x/2)/maxperc
    return(paste0("|",
                  paste0(rep("*", count),collapse = ""),
                  paste0(rep(".", 50-count),collapse = ""),
                  "|")
    )
  }, maxperc))

  out$Percent <- paste0(round(out$Percent, 3), "%")
  out$' ' <- paste0(bars, " (", maxperc, "%)")

  return(out)
}


consoleBoxplot <- function(x) {
  cat("Box plot: \n")
  quant <- quantile(x, na.rm = T)
  box <- round((quant-quant[1])*round(0.9*getOption("width"))/(quant[5]-quant[1]))
  IQR <- box[4]-box[2]

  cat(paste0("|",
         paste0(rep(".", max(0, box[2]-2)),collapse = ""), "<",
         paste0(rep("=", max(0, box[3]-box[2]-1)),collapse = ""), "*",
         paste0(rep("=", max(0, box[4]-box[3]-1)),collapse = ""), ">",
         paste0(rep(".", max(0, box[5]-box[4]-1)),collapse = ""), "|\n"))

  cat("Legends: | min and max, <==...==> IQR, * median \n")
  if (box[5] > box[4]+1.5*IQR) {
    cat("\nPotential outliers present in this variable\n")
  }
}
