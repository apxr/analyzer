#' Skewness
#'
#' \code{skewness} calculates the skewness
#'
#' This function calculates the skewness of data which is a measure of the
#' asymmetry of the probability distribution of a real-valued random variable
#' about its mean. The formula used is:
#' \deqn{\frac{E[(X-\mu)^{3}]}{(E[(X-\mu)^2])^\frac{3}{2}}}. This formula is the
#' typical definition used in many older textbooks and wikipedia
#'
#' @param x a numeric vector, matrix or a data.frame
#' @param na.rm (logical) Should missing values be removed?
#'
#' @return returns a single value if \code{x} is a vector, otherwise a named
#'   vector of size \code{= ncol(x)}.
#'
#' @examples
#' # for a single vector
#' skewness(mtcars$mpg)
#'
#' # for a dataframe
#' skewness(mtcars)
#'
#' @export

skewness <- function(x,
                     na.rm = T) {

  if (is.matrix(x))
    apply(x, 2, skewness, na.rm = na.rm)
  else if (is.vector(x)) {
    if (any(ina <- is.na(x))) {
      if (na.rm)
        x <- x[!ina]
      else return(NA)
    }
    n <- length(x)
    x <- x - mean(x)
    sqrt(n) * sum(x^3)/(sum(x^2)^(3/2))

  }  else if (is.data.frame(x)) {
    sapply(x, skewness, na.rm = na.rm)
  } else {
    skewness(as.vector(x), na.rm = na.rm)
  }
}

#' Kurtosis
#'
#' \code{kurtosis} calculates the Kurtosis
#'
#' This function calculates the kurtosis of data which is a measure of the
#' "tailedness" of the probability distribution of a real-valued random
#' variable. Like skewness, kurtosis describes the shape of a probability
#' distribution. The formula used is: \deqn{\frac{E[(X-\mu)^{4}]}{(
#' E[(X-\mu)^2])^{2}}}.
#' This formula is the typical definition
#' used in many older textbooks and wikipedia
#'
#' @param x a numeric vector, matrix or a data.frame
#' @param na.rm (logical) Should missing values be removed?
#'
#' @return returns a single value if \code{x} is a vector, otherwise a named
#'   vector of size \code{= ncol(x)}.
#'
#' @examples
#' # for a single vector
#' kurtosis(mtcars$mpg)
#'
#' # for a dataframe
#' kurtosis(mtcars)
#'
#' @export

kurtosis <- function(x,
                     na.rm = T) {

  if (is.matrix(x))
    apply(x, 2, kurtosis, na.rm = na.rm)
  else if (is.vector(x)) {
    if (any(ina <- is.na(x))) {
      if (na.rm)
        x <- x[!ina]
      else return(NA)
    }
    n <- length(x)
    x <- x - mean(x)
    n * sum(x^4)/(sum(x^2)^2) - 3

  } else if (is.data.frame(x)) {
    sapply(x, kurtosis, na.rm = na.rm)
  } else {
    kurtosis(as.vector(x), na.rm = na.rm)
  }

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
  message("\n")
  message(paste0(rep(st, consolewidth),collapse = ""))
  message("\n")
}

#' Frequency table and Histogram
#'
#' \code{freqTable} prints a frequency table and histogram of a vector.
#'
#' This function works for all type of vector type. But calling \code{freqTable}
#' for vector with many unique values will print a very long table. If the
#' limit parameter is left blank, then the limit of
#' histogram is adjusted automatically and is shown at the end in brackets
#' (eg. 50% means full bar is equal to 50% frequency).
#' This function is used in the \code{explainer}.
#'
#' @param Value a vector of any type
#' @param limit Upper limit of the bars in histogram. Default is NULL,
#' for which the function will automatically find the suitable limit.
#' This value should be in fraction (between 0 to 1)
#'
#' @return Prints a table with columns \itemize{
#'  \item \code{Value} Value. Each row has a unique value in this table
#'  \item \code{Freq} The frequency count of the Value
#'  \item \code{Proportion} Proportion of the Value \code{(= Freq / length(x))}
#' }
#' This table is followed by a histogram with bars for each of the unique
#' values present in the data.
#'
#' @examples
#' freqTable(mtcars$cyl)
#' freqTable(mtcars$mpg, limit = 0.08)
#'
#'@export
freqTable <- function(Value,
                      limit = NULL) {
  out <- data.frame(table(Value), stringsAsFactors = F)
  out$Proportion <- out$Freq/length(Value)*100

  if (is.null(limit)) {
    maxperc <- max(out$Proportion)
    maxperc <- ceiling(maxperc/25)*25
  } else {
    maxperc <- limit*100
  }

  if (maxperc < max(out$Proportion)){
    warning("Increasing limit to fit the bars")
    maxperc <- ceiling(max(out$Proportion))
  }
  if (maxperc > 100) {
    warning("limit can't be greater than 1. Setting this as 1")
    maxperc <- 100
  }

  bars <- unlist(lapply(out$Proportion, function(x, maxperc) {
    count = round((100*x)/(2.5*maxperc))
    return(paste0("|",
                  paste0(rep("*", count),collapse = ""),
                  paste0(rep(".", 40-count),collapse = ""),
                  "|")
    )
  }, maxperc))

  out$Proportion <- round(out$Proportion/100, 3)
  out$' ' <- paste0(bars, "(", maxperc, "%)")

  message(paste0(capture.output(out), collapse = "\n"))
}

#' Boxplot on the console
#'
#' \code{consoleBoxplot} prints the boxplot on console.
#'
#' This function is for the numeric vectors. It prints a boxplot in a single line
#' on the console. It automatically adjusts for the width of the console.
#' The input vector must have a length of three, otherwise the function
#' will throw a warning and not print any plot.
#'
#' In case of any potential outliers (based on 1.5*IQR criteria), this wil
#' give a warning.
#' This function is used in the \code{explainer}.
#'
#' @param x a numeric vector of length at least 3
#'
#' @return prints a boxplot on the console which has:
#' \itemize{
#'  \item \code{|} at start and end means the minimum and maximum value respectively
#'  \item \code{<==*==>} The IQR region
#'  \item \code{*} shows the median
#'  \item \code{...} everything else in between
#' }
#' Gives a warning of potential outliers (if present)
#'
#' @examples
#' consoleBoxplot(mtcars$mpg)
#'
#' @export
consoleBoxplot <- function(x) {
  message("Box plot: \n")
  if (length(unique(x)) < 3) {
    message("No boxplot for this as unique values are < 3.")
  } else {
    quant <- quantile(x, na.rm = T)
    box <- round((quant-quant[1])*
                   round(0.9*getOption("width"))/
                   (quant[5]-quant[1]))
    IQR <- box[4]-box[2]

    message(paste0("|",
                   paste0(rep(".", max(0, box[2]-2)),collapse = ""), "<",
                   paste0(rep("=", max(0, box[3]-box[2]-1)),collapse = ""), "*",
                   paste0(rep("=", max(0, box[4]-box[3]-1)),collapse = ""), ">",
                   paste0(rep(".", max(0, box[5]-box[4]-1)),collapse = ""), "|\n"))

    message("Legends: | min and max, <==  ==> IQR, * median")
    if (box[5] > box[4]+1.5*IQR) {
      message("Potential outliers present in this variable")
    }
    message("\n")
  }

}
