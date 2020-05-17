#' Skewness
#'
#' \code{skewness} calculates the skewness
#'
#' This function calculates the skewness of data which is a measure of the
#' asymmetry of the probability distribution of a real-valued random variable
#' about its mean. The formula used is:
#' \deqn{\frac{E[(X-\mu)^{3}]}{(E[(X-\mu)^2])^\frac{3}{2}}}. This formula is the
#' typical definition used in many older textbooks and
#' \href{https://en.wikipedia.org/wiki/Skewness}{wikipedia}
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

skewness <- function(x, na.rm = T) {

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
#' used in many older textbooks and
#' \href{https://en.wikipedia.org/wiki/Kurtosis}{wikipedia}
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

kurtosis <- function(x, na.rm = T) {

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
