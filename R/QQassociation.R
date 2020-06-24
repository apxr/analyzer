#' Association (Correlation) between Categorical Variables
#'
#' \code{QQassociation} finds Association measure between all the variables in
#' data with only categorical columns.
#'
#' This function measures the association between categorical variables using
#' Chi Square test. This also returns Cramers V value which is a measure of
#' association between two nominal variables, giving a value between 0 and +1
#' (inclusive). Higher number indicates higher association. Note that, unlike
#' Pearson correlation this doesn't give negative value.
#'
#' The relation between Cramer's V and Chi Sq test is
#'
#' \deqn{\sqrt{\frac{\chi ^2}{n*min(k-1,r-1))}}}
#'
#' where:
#' \describe{
#'  \item{X}{is derived from Pearson's chi-squared test}
#'  \item{n}{is the grand total of observations}
#'  \item{k}{being the number of columns}
#'  \item{r}{being the number of rows}
#' }
#'
#' The p-value for the significance of Cramer's V is the same one
#' that is calculated using the Pearson's chi-squared test.
#'
#' @seealso
#' \code{\link{association}} for association between any type of variables,
#' \code{\link{CCassociation}} for Association between Continuous (numeric)
#' variables,
#' \code{\link{CQassociation}} for Association between Continuous-Categorical
#' variables
#'
#' @param factb a data frame with all the categorical columns. This should
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
#' @param methods_used a square data.frame which will store the type of
#' association used between the variables. Dimension will be
#' number of variables * number of variables.
#'
#' @return a list of two tables with number of rows and column equal to number
#' of columns in \code{factb}:
#' \describe{
#'  \item{chisq}{Table containing p-values of chi-square test}
#'  \item{cramers}{Table containing Cramer's V}
#' }
QQassociation <- function(factb,
                          use = "everything",
                          methods_used) {

  chiCor <- function(x, y) {
    tbl <- table(x, y)
    chis2 <- chisq.test(tbl, correct = F)
    return(chis2)
  }

  ncx <- ncol(factb)
  r <- matrix(0, nrow = ncx, ncol = ncx)
  cramerV <- matrix(0, nrow = ncx, ncol = ncx)
  for (i in seq_len(ncx)) {
    for (j in seq_len(i)) {
      x <- factb[, i]
      y <- factb[, j]

      xname <- names(factb)[i]
      yname <- names(factb)[j]
      methods_used[yname, xname] <- methods_used[xname, yname] <- "Chi Square"

      if (use == "everything") {
        if ((sum(is.na(x))+sum(is.na(y))) > 0) {
          r[i,j] <- r[j,i] <- NA
        } else {
          chiT <- chiCor(x, y)
          r[i,j] <- r[j,i] <- chiT$p.value
          cramerV[i,j] <- cramerV[j,i] <- sqrt(chiT$statistic /
                                                 (length(x)*
                                                    (min(dim(chiT$observed))-1)
                                                  )
                                               )
        }
      } else {
        ok = complete.cases(x, y)
        if (sum(ok) == 0){
          if (use == "complete.obs") {
            stop('While finding association between "', colnames(factb)[i],
                 '" and "', colnames(factb)[j],
                 '", all the observations were missing.
                 Select use = "na.or.complete" for such case.')
          } else if (use == "na.or.complete") {
            r[i,j] <- r[j,i] <- NA
          }
        } else {
          x <- x[ok]
          y <- y[ok]
          chiT <- chiCor(x, y)
          r[i,j] <- r[j,i] <- chiT$p.value
          cramerV[i,j] <- cramerV[j,i] <- sqrt(chiT$statistic
                                               / (length(x)*
                                                    (min(dim(chiT$observed))-1)
                                                  )
                                               )
        }
      }
    }
  }

  return(list(chisq = r, cramers = cramerV, methods_used = methods_used))
}
