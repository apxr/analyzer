#' Generic explain
#'
#' Generic function for printing the details of a vector. Based on
#' the data type of vector, this calls the appropriate method.
#'
#' Current methods for \code{explain} are for numeric, integer, character
#' and factor vectors. To get the list of all available methods type the
#' command \code{methods(explain)}.
#'
#' @param X a vector
#' @param xname name of the data to be printed. If missing then the
#' name of variable passed as \code{X} will be used
#' @param ... other parameters required for other methods of \code{explain}
#' To see the parameters for numeric methods, use \code{?explain.numeric} and
#' similarly for other methods (\code{?explain.factor} etc.)
#'
#' @return Prints the information on the console. For print information for
#' the individual methods, see their documentation. Returns nothing.
#'
#' @examples
#' # for numeric
#' explain(mtcars$mpg) # same as explain.numeric(mtcars$mpg)
#' # for factor
#' explain(as.factor(mtcars$cyl)) # same as explain.factor(as.factor(mtcars$cyl))
#'
#' @export
explain <- function(X, xname = NULL, ...) {
  if (is.null(xname)) xname <- deparse(substitute(X))
  cat_ <- paste0(xname, " (type: ", paste0(class(X), collapse = ", "), ")")
  cat(paste0(cat_, "\n",
             paste0(rep(".", nchar(cat_)),collapse = ""),
             "\n"))
  UseMethod("explain", X)
}

#' Explain method for numeric data types
#'
#' This is a \code{explain} method for numeric vector.
#'
#' This method removes all the missing values in \code{x} before computing the
#' summaries.
#'
#' @param X a numeric (or integer) data type
#' @param xname a placeholder for variable name
#' @param include.numeric a vector having strings which is also required along
#' with the default output. Can have values:
#' \itemize{
#'  \item \code{trimmed.means} for printing the trimemd mean after removing
#'  \code{trim} fraction of data from each side of x. \code{trim} can be passed
#'  as an parameter
#'  \item \code{skewness} for printing the skewness of the data.
#'  Use \code{?skreness} for more information
#'  \item \code{kurtosis} for printing
#'  the kurtosis of the data. Use \code{?kurtosis} for more information
#' }
#' @param round.digit number of decimal places required in the output.
#' @param quant.seq vector of fractions (0 to 1) for which the quantiles are
#'  required \code{0.5} means median, \code{0} means smalles observation and
#'  \code{1} means largest observation
#' @param trim the fraction (0 to 0.5) of observations to be trimmed from each
#'  end of x before the mean is computed. Values of trim outside that range are
#'  taken as the nearest endpoint. This only works if \code{include.numeric} has
#'  a string 'trimmed.means'
#' @param ... other parameters required
#'
#' @return Prints the following information on console:
#' \itemize{
#'  \item vector name
#'  \item type
#'  \item number of distinct values
#'  \item number of missing values
#'  \item mean
#'  \item sd (standard deviation)
#'  \item median
#'  \item quantiles based on \code{quant.seq} parameter
#'  \item other information based on \code{include.numeric}
#'  \item a box plot (only if number distinct numbers are > 2).
#'  If counts of all the factor levels are less than half of length of
#'  \code{x}, then the histogram is scaled with maximum of 50%. (look at
#'  \code{?consoleBoxplot} for how to read the table and histogram)
#'  \item a frequency table and histogram (only if number of distinct
#'  numbers are < 11)
#'  (look at \code{?freqTable} for how to read the table and histogram)
#' }
#'
#' @examples
#' explain(mtcars$mpg)
#' explain(mtcars$mpg, include.numeric = c('trimmed.means', 'skewness',
#' 'kurtosis'), round.digit = 1, quant.seq = seq(0,1,0.1), trim = 0.05)
#'
#' @export
explain.numeric <- function(X, xname = NULL, include.numeric = NULL, round.digit = 2, quant.seq = seq(0,1,0.2),
                            trim = 0.05, ...) {
  # printing summary
  quant <- round(quantile(X, probs = quant.seq, names = T, na.rm = T), round.digit)
  uniqx <- length(unique(X))
  medi  <- median(X, na.rm = T)
  out <- data.frame(distinct = uniqx,
                    missing = sum(is.na(X)),
                    mean   = round(mean(X, na.rm = T), round.digit),
                    sd     = round(sd(X, na.rm = T), round.digit),
                    median = round(medi, round.digit),
                    t(quant)
                    , check.names = F, stringsAsFactors = F)

  # Adding extra information based on input
  if ("trimmed.means" %in% include.numeric) {
    out$`trimmed mean5%` <- round(mean(X, trim = trim, na.rm = T), round.digit)
  }
  if ("skewness" %in% include.numeric) {
    out$skewness <- round(skewness(X, na.rm = T), round.digit)
  }
  if ("kurtosis" %in% include.numeric) {
    out$kurtosis <- round(kurtosis(X, na.rm = T), round.digit)
  }

  blanks_to_add <- 1+max(0, max(nchar(out) - nchar(colnames(out))))
  names(out) <- paste0(paste0(rep(" ", blanks_to_add), collapse = ""), names(out))
  print(out, row.names = F)
  consoleBoxplot(X)
  if (uniqx <= 10) {
    cat("\n Showing frequency table because variable has less distinct values: \n")
    print(freqTable(X), row.names = F)
  }
}

#' Explain method for character data types
#'
#' This is a \code{explain} method for character vector.
#'
#' This method removes all the missing values in \code{x} before computing the
#' summaries. This calls the method \code{explain.factor}
#'
#' @param X a numeric (or integer) data type
#' @param xname a placeholder for variable name
#' @param ... other parameters required
#'
#' @return Prints the following information on console:
#' \itemize{
#'  \item vector name
#'  \item type
#'  \item number of distinct values
#'  \item number of missing values
#'  \item a frequency table and histogram. If counts of all the factor levels are
#'  less than half of length of \code{X}, then the histogram is scaled with
#'  maximum of 50%. (look at \code{?freqTable} for how to read this)
#' }
#'
#' @examples
#' alphabets <- sample(LETTERS[1:5], 50, replace = TRUE)
#' explain(alphabets)
#' rm(alphabets)
#'
#'@export
explain.character <- function(X, xname = NULL, ...) {
  out <- data.frame(distinct = length(unique(X)),
                    missing = sum(is.na(X))
                    , check.names = F, stringsAsFactors = F)
  print(out, row.names = F)
  cat("\n Frequency table: \n")
  print(freqTable(X), row.names = F)
}


#' Explain method for factor data types
#'
#' This is a \code{explain} method for factor vector.
#'
#' This method removes all the missing values in \code{x} before computing the
#' summaries.
#'
#' @param X a numeric (or integer) data type
#' @param xname a placeholder for variable name
#' @param ... other parameters required
#'
#' @return Prints the following information on console:
#' \itemize{
#'  \item vector name
#'  \item type
#'  \item number of distinct values
#'  \item number of missing values
#'  \item a frequency table and histogram. If counts of all the factor levels are
#'  less than half of length of \code{X}, then the histogram is scaled with
#'  maximum of 50%. (look at \code{?freqTable} for how to read this)
#' }
#'
#' @examples
#' alphabets <- as.factor(sample(LETTERS[1:5], 50, replace = TRUE))
#' explain(alphabets)
#' rm(alphabets)
#'
#'@export
explain.factor <- function(X, xname = NULL, ...) {
  explain.character(X, x_name, ...)
}

