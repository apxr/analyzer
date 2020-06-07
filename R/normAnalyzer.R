#' Checks for Normality Assumption
#'
#' \code{norm_test_fun} checks for the normality assumption
#'
#' This function checks for normality assumption using
#' shapiro or Anderson Darling test. If the parameter \code{onlyPval}
#' is TRUE, then \code{TRUE} is returned if vector is normal, otherwise FALSE.
#' The significance level is passed through the parameter
#' \code{pval}
#'
#' @seealso \code{\link{anderson.test}} for Anderson Darling test
#'
#' @param x a numeric vector
#' @param method \code{shapiro} for Shapiro-Wilk test or
#' \code{'anderson'} for 'Anderson-Darling' test of normality
#' @param pval pvalue cutoff for calling test significant
#' @param xn vector name
#' @param onlyPval TRUE if only TRUE/FALSE is required
#'
#' @return Logical TRUE/FALSE based on the performed test and \code{pval}.
#' If the vector follows the normality assumption, then TRUE is returned
#'
#' @examples
#' norm_test_fun(mtcars$mpg)
#' norm_test_fun(mtcars$mpg, method = "shapiro", pval = 0.05, xn = "mpg", onlyPval = TRUE)
#'
#' @export
norm_test_fun <- function(x, method = "anderson", pval = 0.05, xn = 'x', onlyPval = FALSE) {
  if (!method %in% c("shapiro", "anderson")) {
    warning("Method should only be 'shapiro' or 'ad'. Setting method as 'ad'")
    method <- "ad"
  }
  if (length(x) > 5000) {
    warning(paste0(xn, " is very large (>5000), normality test may not be accurate. Changing method to 'Anderson-Darling'"))
    method = "anderson"
  }
  if (length(x) < 3) {
    warning(paste0(xn, " is very small (<3), normality test can't be performed."))
    return(0)
  }

  if (method == "shapiro") {
    out <- shapiro.test(x)
  } else {
    out <- anderson.test(x)
  }

  if (onlyPval) {
    return(out$p.value  > pval)
  } else {
    return(out)
  }

}

#' Anderson Darling test
#'
#' \code{anderson.test} performs Anderson-Darling test
#'
#' Performs the Anderson-Darling test for  the composite hypothesis of normality,
#' see e.g. Thode (2002, Sec. 5.1.4).
#'
#' @param x a numeric vector. Length must be greater than 7.
#' Missing values are allowed.
#'
#'
#' @return A list with following elements:
#' \describe{
#'  \item{statistic}{the value of Anderson-Darling test statistic}
#'  \item{p.value}{p-value of the test}
#'  \item{method}{Test name}
#'  \item{data.name}{Vector name}
#' }
#'
#' @examples
#' anderson.test(mtcars$mpg)
#'
#' @seealso \code{\link{norm_test_fun}}
#' @export
anderson.test <- function (x){
  DNAME <- deparse(substitute(x))
  x <- sort(x[complete.cases(x)])
  n <- length(x)
  if (n < 8)
    stop("sample size must be greater than 7")

  logp1 <- pnorm( (x-mean(x))/sd(x), log.p=TRUE)
  logp2 <- pnorm( -(x-mean(x))/sd(x), log.p=TRUE )
  h <-  (2 * seq(1:n) - 1) * (logp1 + rev(logp2))

  A <- -n - mean(h)
  AA <- (1 + 0.75/n + 2.25/n^2) * A
  if (AA < 0.2) {
    pval <- 1 - exp(-13.436 + 101.14 * AA - 223.73 * AA^2)
  }
  else if (AA < 0.34) {
    pval <- 1 - exp(-8.318 + 42.796 * AA - 59.938 * AA^2)
  }
  else if (AA < 0.6) {
    pval <- exp(0.9177 - 4.279 * AA - 1.38 * AA^2)
  }
  else if (AA < 10) {
    pval <- exp(1.2937 - 5.709 * AA + 0.0186 * AA^2)
  }
  else pval <- 3.7e-24
  out <- list(statistic = c(A = A), p.value = pval, method = "Anderson-Darling normality test",
              data.name = DNAME)
  class(out) <- "htest"
  return(out)
}
