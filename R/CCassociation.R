#' Association (Correlation) between Continuous (numeric) Variables
#'
#' \code{CCassociation} finds correlation between all the variables in data
#' with only numeric columns
#'
#' This function calls \code{cor} function to calculate the correlation values.
#' The difference is that this doesn't take method as parameter, instead it
#' decides the methods itself using normality tests. If the variables satisfy
#' the assumption of Pearson correlation, then pearson correlation is calculated.
#' Otherwise Spearman is calculated. To learn more, check the
#' \code{\link[stats]{cor}}
#'
#' @seealso
#' \code{\link{association}} for association between any type of variables,
#' \code{\link{QQassociation}} for Association between Categorical variables,
#' \code{\link{CQassociation}} for Association between Continuous-Categorical
#' variables
#'
#' @param numtb a data frame with all the numerical columns. This should
#' have at least two columns
#' @param use an optional character string giving a method for computing
#'   association in the presence of missing values. This must be (complete or an
#'   abbreviation of) one of the strings "everything", "all.obs",
#'   "complete.obs", "na.or.complete", or "pairwise.complete.obs". If use is
#'   "everything", NAs will propagate conceptually, i.e., a resulting value will
#'   be NA whenever one of its contributing observations is NA. If use is
#'   "all.obs", then the presence of missing observations will produce an error.
#'   If use is "complete.obs" then missing values are handled by case wise
#'   deletion (and if there are no complete cases, that gives an error).
#'   "na.or.complete" is the same unless there are no complete cases, that gives
#'   NA
#' @param normality_test_method method for normality test for a variable.
#'   Values can be \code{shapiro}
#'   for Shapiro-Wilk test or
#'   \code{'anderson'} for 'Anderson-Darling' test of normality or \code{ks} for
#'  'Kolmogorov-Smirnov'
#' @param normality_test_pval significance level for normality tests.
#' Default is 0.05
#' @param method1 method for association between continuous-continuous
#'   variables. values can be \code{"auto", "pearson",  "kendall", "spearman"}.
#'   See details for more information.
#' @param methodMat1 method dataframe like methodMats from the function \code{
#' association}
#' @param methods_used a square data.frame which will store the type of
#' association used between the variables. Dimension will be
#' number of variables * number of variables.
#'
#' @return a list of two tables with number of rows and column equal to number
#' of columns in \code{numtb}:
#' \describe{
#'  \item{r}{Table containing correlation values}
#'  \item{r_pvalue}{Table containing p-value for the correlation test}
#' }
CCassociation <- function(numtb,
                          use = "everything",
                          normality_test_method,
                          normality_test_pval,
                          method1 = c("auto", "pearson",  "kendall", "spearman"),
                          methodMat1 = NULL,
                          methods_used) {

  # INTERNAL FUNCTION
  CC_ <- function(x, y, varnames, norm_test, met) {

    if (met == "auto") {
      if (norm_test) {
        warning(paste0("Variables '", paste0(varnames, collapse = "' and '"),
                       "' follows normality assumptions. Doing parameteric test
                       (Pearson) for these variables."))
        return(list(m = "pearson", val = cor.test(x, y, method = "pearson")))
      } else {
        warning(paste0("Variables '", paste0(varnames, collapse = "' and '"),
                       "' doesn't follow normality assumptions. Doing
                       non-parameteric test (Spearman) for these variables."))
        return(list(m = "spearman", val = cor.test(x, y, method="spearman")))
      }
    } else {
      if (norm_test) {
        warning(paste0("Variables '", paste0(varnames, collapse = "' and '"),
                       "' follows normality assumptions. Parameteric test
                       (Pearson) should be preferred for these variables."))
      } else {
        warning(paste0("Variables '", paste0(varnames, collapse = "' and '"),
                       "' doesn't follow normality assumptions. Non-parameteric
                       test (Spearman) should be preferred for these variables"))
      }
      return(list(m = met, val = cor.test(x, y, method = met)))
    }

  }

  # METHOD FOR THE CORRELATION
  method1 <- match.arg(method1)

  # IF CHANNEL PAIR LEVEL METHOD IS MISSING THEN USING method1 FOR ALL PAIRS
  if (is.null(methodMat1)) {
    methodMat1 <- data.frame(matrix(method1, nrow = ncol(numtb),
                                    ncol = ncol(numtb),
                                    dimnames = list(names(numtb), names(numtb))))
  }

  # NORMALITY TEST FOR ALL THE VARIABLES ---------------------------------------
  norm_test_all <- unlist(lapply(1:ncol(numtb), function(x,
                                                         numtb,
                                                         normality_test_method,
                                                         normality_test_pval) {
    return(tryCatch(norm_test_fun(numtb[,x],
                                  method = normality_test_method,
                                  pval = normality_test_pval,
                                  names(numtb)[x],
                                  bin = TRUE),
                    error=function(e){
                                    warning(paste0("Normality test failed for ",
                                                   names(numtb)[x])); return(0)
                                  }))
  }, numtb, normality_test_method, normality_test_pval))
  names(norm_test_all) <- colnames(numtb)

  if (is.null(norm_test_all)) {
    norm_test_all <- rep(TRUE, ncol(numtb))
    names(norm_test_all) <- colnames(numtb)
  }
  # ----------------------------------------------------------------------------
  ncx <- ncol(numtb)
  r <- matrix(0, nrow = ncx, ncol = ncx)
  rownames(r) <- colnames(numtb)
  colnames(r) <- colnames(numtb)

  r_pvalue <- matrix(NA, nrow = ncx, ncol = ncx)
  rownames(r_pvalue) <- colnames(numtb)
  colnames(r_pvalue) <- colnames(numtb)

  for (i in seq_len(ncx)) {

    for (j in seq_len(i)) {

      xname <- names(numtb)[i]
      yname <- names(numtb)[j]

      met <- methodMat1[xname, yname]

      x <- numtb[, i]
      y <- numtb[, j]

      if (use == "everything") {
        if ((sum(is.na(x))+sum(is.na(y))) > 0) {
          r[i,j] <- r[j,i] <- NA
          r_pvalue[i,j] <- r_pvalue[j,i] <- NA
        } else {
          norm_test <- all(norm_test_all[c(xname, yname)])
          a <- CC_(x, y, c(xname, yname), norm_test,met)
          r[i,j] <- r[j, i] <- a$val$estimate
          r_pvalue[i,j] <- r_pvalue[j, i] <- a$val$p.value
          methods_used[yname, xname] <- methods_used[xname, yname] <- a$m
        }
      } else {
        ok = complete.cases(x, y)
        if (sum(ok) == 0){
          if (use == "complete.obs") {
            stop('While finding association between "', xname, '" and "', yname,
                 '", all the observations were missing.
                 Select use = "na.or.complete" for such case.')
          } else if (use == "na.or.complete") {
            r[i,j] <- r[j,i] <- NA
            r_pvalue[i,j] <- r_pvalue[j,i] <- NA
          }
        } else {
          x <- x[ok]
          y <- y[ok]
          norm_test <- all(norm_test_all[c(xname, yname)])
          a <- CC_(x, y, c(xname, yname), norm_test, met)
          r[i,j] <- r[j, i] <- a$val$estimate
          r_pvalue[i,j] <- r_pvalue[j, i] <- a$val$p.value
          methods_used[yname, xname] <- methods_used[xname, yname] <- a$m
        }
      }
    }
  }
  return(list(r = r, r_pvalue = r_pvalue, methods_used = methods_used))
}
