#' Generic explainer
#'
#' Generic function for printing the details of data. Based on
#' the data type, this calls the appropriate method.
#'
#' Current methods for \code{explainer} are for data.frame, numeric, integer,
#' character and factor vectors. To get the list of all available methods type
#' the command \code{methods(explainer)}.
#'
#' @param X a data.frame or a vector
#' @param xname name of the data to be printed. If missing then the
#' name of variable passed as \code{X} will be used
#' @param ... other parameters required for other methods of \code{explainer}
#' To see the parameters for numeric methods, use \code{?explainer.numeric} and
#' similarly for other methods (\code{?explainer.factor} etc.)
#'
#' @return Prints the information on the console. For print information for
#' the individual methods, see their documentation. Returns nothing.
#'
#' @examples
#' # for numeric
#' explainer(mtcars)
#' explainer(mtcars$mpg) #same as explainer.numeric(mtcars$mpg)
#' # for factor
#' explainer(as.factor(mtcars$cyl)) #same as explainer.factor(as.factor(mtcars$cyl))
#'
#' @importFrom utils capture.output
#' @export
explainer <- function(X,
                      xname = NULL,
                      ...) {
  UseMethod("explainer", X)
}

#' Show details of the data frame
#'
#' \code{explainer} shows detail of all the columns of the data
#'
#' This function uses \code{explainer} on each column.
#'
#' @param X A data.frame
#' @param xname variable name
#' @param ... parameters for explainer for other classes
#' @return Prints details of the dataset which includes: dataset name,
#' type, number of columns, rows and unique rows. Also prints output of
#' \code{explainer} for all the columns. Returns nothing.
#'
#' @examples
#' explainer(mtcars)
#'
#' @export

explainer.data.frame <- function(X,
                                 xname = NULL,
                                 ...) {
  df <- X

  if (requireNamespace("data.table", quietly = TRUE)) {
    uniqRow <- data.table::uniqueN(df)
  } else {
    uniqRow <- nrow(unique(df))
  }
  consolewidth <- getOption("width")
  dataname     <- deparse(substitute(X))

  message(paste0("Data: ", dataname,
                 "\nType: ", paste0(class(df), collapse = ", "),
                 "\n\nNumber of columns: ", ncol(df),
                 "\nNumber of rows: ", nrow(df),
                 "\nNumber of distinct rows: ", uniqRow,
                 "\n"))
  linedivider(consolewidth)

  df <- as.data.frame(df)
  # showing each variable
  summ <- lapply(names(df), function(x, df, linedivider, consolewidth) {
    X1 <- df[,x]
    explainer(X1, xname = x, ...)
    linedivider(consolewidth)
  }, df, linedivider, consolewidth)

}

#' Explain method for numeric data types
#'
#' This is a \code{explainer} method for numeric vector.
#'
#' This method removes all the missing values in \code{x} before computing the
#' summaries.
#'
#' @param X a numeric (or integer) data type
#' @param xname a placeholder for variable name
#' @param include.numeric a vector having strings which is also required along
#' with the default output. Can have values:
#' \itemize{
#'  \item \code{trimmed.means} for printing the trimmed mean after removing
#'  \code{trim} fraction of data from each side of x. \code{trim} can be passed
#'  as an parameter
#'  \item \code{skewness} for printing the skewness of the data.
#'  Use \code{?skreness} for more information
#'  \item \code{kurtosis} for printing
#'  the kurtosis of the data. Use \code{?kurtosis} for more information
#' }
#' @param round.digit number of decimal places required in the output.
#' @param quant.seq vector of fractions (0 to 1) for which the quantiles are
#'  required \code{0.5} means median, \code{0} means smallest observation and
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
#' explainer(mtcars$mpg)
#' explainer(mtcars$mpg, include.numeric = c('trimmed.means', 'skewness',
#' 'kurtosis'), round.digit = 1, quant.seq = seq(0,1,0.1), trim = 0.05)
#'
#' @export
explainer.numeric <- function(X,
                              xname = NULL,
                              include.numeric = NULL,
                              round.digit = 2,
                              quant.seq = seq(0,1,0.2),
                              trim = 0.05,
                              ...) {

  if (is.null(xname)) xname <- deparse(substitute(X))
  cat_ <- paste0(xname, " (type: ", paste0(class(X), collapse = ", "), ")")
  message(paste0(cat_, "\n",
                 paste0(rep(".", nchar(cat_)),collapse = ""),
                 "\n"))

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
  names(out) <- paste0(paste0(rep(" ", blanks_to_add), collapse = ""),
                       names(out))
  message(paste0(capture.output(out), collapse = "\n"))

  consoleBoxplot(X)
  if (uniqx <= 10) {
    message("Showing frequency table because variable has less distinct values:\n")
    freqTable(X)
  }
}

#' Explain method for character data types
#'
#' This is a \code{explainer} method for character vector.
#'
#' This method removes all the missing values in \code{x} before computing the
#' summaries.
#'
#' @param X a vector of character data type
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
#' explainer(alphabets)
#' rm(alphabets)
#'
#'@export
explainer.character <- function(X,
                                xname = NULL,
                                ...) {
  if (is.null(xname)) xname <- deparse(substitute(X))
  cat_ <- paste0(xname, " (type: ", paste0(class(X), collapse = ", "), ")")
  message(paste0(cat_, "\n",
                 paste0(rep(".", nchar(cat_)),collapse = ""),
                 "\n"))

  out <- data.frame(distinct = length(unique(X)),
                    missing = sum(is.na(X))
                    , check.names = F, stringsAsFactors = F)
  print(out, row.names = F)
  message("\nFrequency table: \n")
  freqTable(X)
}


#' Explain method for factor data types
#'
#' This is a \code{explainer} method for factor vector.
#'
#' This method removes all the missing values in \code{x} before computing the
#' summaries. This calls the method \code{explainer.character}
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
#' explainer(alphabets)
#' rm(alphabets)
#'
#'@export
explainer.factor <- function(X,
                             xname = NULL,
                             ...) {
  explainer.character(X, xname, ...)
}
