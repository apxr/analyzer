#' Association (Correlation) between Continous (numeric) Variables
#'
#' \code{CCassociation} finds correlation between all the variables in data
#' with only numeric columns
#'
#' This function calls \code{cor} function to calculate the correlation values.
#' The difference is that this doesn't take method as parameter, instead it
#' decides the methods iteself using normality tests. If the variables satisfy the
#' assumption of Pearson correlation, then pearson correlation is calculated.
#' Otherwise Spearman is calculated. To learn more, check the
#' \code{\link[stats]{cor}}
#'
#' @seealso
#' \code{\link{association}} for association between any type of variables,
#' \code{\link{QQassociation}} for Association between Categorical variables,
#' \code{\link{CQassociation}} for Association between Continuous-Categorical variables
#'
#' @param numtb a data frame with all the numerical columns. This should
#' have atleast two columns
#' @param use an optional character string giving a method for computing
#'   association in the presence of missing values. This must be (complete or an
#'   abbreviation of) one of the strings "everything", "all.obs",
#'   "complete.obs", "na.or.complete", or "pairwise.complete.obs". If use is
#'   "everything", NAs will propagate conceptually, i.e., a resulting value will
#'   be NA whenever one of its contributing observations is NA. If use is
#'   "all.obs", then the presence of missing observations will produce an error.
#'   If use is "complete.obs" then missing values are handled by casewise
#'   deletion (and if there are no complete cases, that gives an error).
#'   "na.or.complete" is the same unless there are no complete cases, that gives
#'   NA
#' @param norm_test_all a logical named vector with TRUE when the
#'  column follows normality assumption, otherwise FALSE. This should have
#'  TRUE or FALSE for all the columns present in \code{numtb}
#'
#' @return a table of correleations among the variables with number or
#' rows and column same as the number of columns in \code{numtb}.
#'
#' @examples
#' CCassociation(mtcars)
#'
#' # with norm_test_all
#' norm_test_all <- rep(TRUE, ncol(mtcars))
#' names(norm_test_all) <- colnames(mtcars)
#' CCassociation(mtcars, use = "complete.obs", norm_test_all = norm_test_all)
#' rm(norm_test_all)
#'
#' @export
CCassociation <- function(numtb, use = "everything", norm_test_all = NULL) {

  CC_ <- function(x, y, use, varnames, norm_test) {

    if (norm_test) {
      warning(paste0("Variable ", paste0(varnames, collapse = ", "),
                     " follows normality assumptions. Doing parameteric test (Pearson) for variables: ",
                     paste0(varnames, collapse = ", ")))
      return(cor(x, y, use=use, method = "pearson"))
    } else {
      warning(paste0("Variable ", paste0(varnames, collapse = ", "),
                     " doesn't follow normality assumptions. Doing non-parameteric test (Spearman) for variables: ",
                     paste0(varnames, collapse = ", ")))
      return(cor(x, y, use=use, method="spearman"))
    }

  }

  if (is.null(norm_test_all)) {
    norm_test_all <- rep(TRUE, ncol(numtb))
    names(norm_test_all) <- colnames(numtb)
  }
  ncx <- ncol(numtb)
  r <- matrix(0, nrow = ncx, ncol = ncx)
  rownames(r) <- colnames(numtb)
  colnames(r) <- colnames(numtb)

  for (i in seq_len(ncx)) {

    for (j in seq_len(i)) {
      x <- numtb[, i]
      y <- numtb[, j]

      if (use == "everything") {
        if ((sum(is.na(x))+sum(is.na(y))) > 0) {
          r[i,j] <- r[j,i] <- NA
        } else {
          norm_test <- all(norm_test_all[c(names(numtb)[i], names(numtb)[j])])
          r[i,j] <- r[j, i] <- CC_(x, y, use, c(names(numtb)[i], names(numtb)[j]), norm_test)
        }
      } else {
        ok = complete.cases(x, y)
        if (sum(ok) == 0){
          if (use == "complete.obs") {
            stop('While finding association between "', colnames(numtb)[i], '" and "', colnames(numtb)[j],
                 '", all the observations were missing. Select use = "na.or.complete" for such case.')
          } else if (use == "na.or.complete") {
            r[i,j] <- r[j,i] <- NA
          }
        } else {
          x <- x[ok]
          y <- y[ok]
          norm_test <- all(norm_test_all[c(names(numtb)[i], names(numtb)[j])])
          r[i,j] <- r[j, i] <- CC_(x, y, use, c(names(numtb)[i], names(numtb)[j]), norm_test)
        }
      }
    }
  }
  return(r)
}
