#' Association (Correlation) between Continuous-Categorical Variables
#'
#' \code{CQassociation} finds Association measure between one
#' categorical and one continuos variable.
#'
#' This function measures the association between one categorical variable
#' and one continuous variable present in different dataset. Two datasets
#' are provided as input, one data has only numerical columns while other
#' data has only categorical columns. This performs either t-test for the
#' parameteric case and 'Mann-Whitney’ test for the non-parametric case.
#' If the method3 is passed as 'auto', the function defines the method
#' itself based on different tests for equal variance and normality check
#' which checks for assumptions for the t-test. If the assumptions are
#' satisfied, then t-test (parametric) is performed, otherwise
#' 'Mann-Whitney’ (non-parametric) test is performed.
#'
#' @seealso
#' \code{\link{norm_test_fun}} for normality test
#' \code{\link{association}} for association between any type of variables,
#' \code{\link{CCassociation}} for Association between Continuous (numeric) variables,
#' \code{\link{QQassociation}} for Association between Categorical variables
#'
#' @param numtb a data frame with all the numerical columns. This should
#' have atleast two columns
#' @param factb a data frame with all the categorical columns. This should
#' have atleast two columns
#' @param method3 method for association between continuous-categorical
#'   variables. Values can be \code{"auto", "parametric", "non-parametric"}.
#'   See details for more information. Parametric does t-test while non-parametric
#'   does 'Mann-Whitney’ test.
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
#' @param normality_test_method takes values as 'shapiro' or 'anderson'.
#'   this parameter decides which test to perform for the normality test.
#'   See details of \code{\link{norm_test_fun}} for more information.
#'
#' @return a table with number of rows equal to number of columns in \code{numtb}
#' and number of columns equal to number of columns in \code{factb}.
#' Table containing p-values of performed test
#'
#' @examples
#' tb <- mtcars
#' tb$cyl <- as.factor(tb$cyl)
#' tb$vs  <- as.factor(tb$vs)
#' factb  <- tb[, c("cyl","vs")]
#' numtb  <- tb[, c("mpg", "disp", "qsec")]
#' CQassociation(numtb, factb)
#'
#' @export
CQassociation <- function(numtb, factb, method3 = c("auto", "parametric", "non-parametric"),
                          use = "everything", normality_test_method = "anderson") {

  method3 <- match.arg(method3)

  CQ_ <- function(x,y,method3,varnames) {
    uniqY <- unique(y)
    if (length(uniqY)==1){
      warning(paste0("Setting association of ", paste0(varnames, collapse = ", "),
                     " as NA because ", varnames[2], " has only 1 unique value."))
      return(NA)
    } else if (length(uniqY)==2){
      x1<-x[y==uniqY[1]]
      x2<-x[y==uniqY[2]]
      if ((length(x1) < 2) | (length(x2) < 2)) {
        warning(paste0("not enough oberservation for all the levels of variable ", varnames[2]))
        return(NA)
      }
      # test for normality
      norm_test <- c(tryCatch(norm_test_fun(numtb[,x], method = normality_test_method, pval = 0.05, varnames[1],
                                            onlyPval = TRUE), error=function(e){
        warning(paste0("Normality test failed for ", varnames[1])); return(0)
      }),
      tryCatch(norm_test_fun(numtb[,x], method = normality_test_method, pval = 0.05, varnames[1],
                             onlyPval = TRUE), error=function(e){
        warning(paste0("Normality test failed for ", varnames[1])); return(0)
      }))
      norm_test <- all(norm_test)
      # test for equal variance
      var_test <- var.test(x~y)$p.value > 0.05

      assumption_test <- norm_test & var_test
      if (method3=="auto") {
        if (assumption_test) {
          warning(paste0("Variable ", varnames[1],
                         " follows assumptions for t-test. Doing parameteric test for variables: ",
                         paste0(varnames, collapse = ", ")))
          test <- t.test(x1, x2)
          return(test$p.value)
        } else {
          warning(paste0("Variable ", varnames[1],
                         " doesn't follow assumptions of t-test. Doing Non-parameteric test (Mann-Whitney test) for variables: ",
                         paste0(varnames, collapse = ", ")))
          test <- wilcox.test(x ~ y)
          return(test$p.value)
        }

      } else if (method3 == "parametric") {
        if (!assumption_test) {
          warning(paste0("The continuous variable ", varnames[1],
                         " doesn't follow assumption of t-test. 'non-parametric' test may be better for this"))
        }
        return(t.test(x1, x2)$p.value)
      } else {
        if (assumption_test) {
          warning(paste0("The continuous variable ", varnames[1],
                         " follows assumptions for t-test. 'parametric' test may be better for this"))
        }
        return(wilcox.test(x ~ y)$p.value)
      }
    } else {
      # test for normality (TRUE means normal)
      npvalue<-c()
      for (uq in uniqY) {
        xz <- x[y==uq]
        norm_test <- tryCatch(norm_test_fun(numtb[,x], method = normality_test_method, pval = 0.05, varnames[1],
                                            onlyPval = TRUE), error=function(e){
          warning(paste0("Normality test failed for ", varnames[1])); return(0)
        })
        npvalue<-c(npvalue, norm_test)
      }
      norm_test <- all(npvalue)
      # test for equal variance (TRUE means same variance)
      if (norm_test) {
        var_test <- bartlett.test(x~y)$p.value > 0.05
      } else {
        var_test <- fligner.test(x~y)$p.value > 0.05
      }
      assumption_test <- norm_test & var_test
      if (method3=="auto"){
        if (assumption_test) {
          warning(paste0("Variable ", varnames[1],
                         " follows assumptions of ANOVA. Doing parameteric test (ANOVA) for variables: ",
                         paste0(varnames, collapse = ", ")))
          return(summary(aov(x ~ y))[[1]][["Pr(>F)"]][1])
        } else {
          warning(paste0("Variable ", varnames[1],
                         " doesn't follows assumptions of ANOVA. Doing Non-parameteric test (Kruskal Wallis) for variables: ",
                         paste0(varnames, collapse = ", ")))
          return(kruskal.test(x~y)$p.value)
        }
      } else if (method3 == "parametric") {
        if (!assumption_test) {
          warning(paste0("The continuous variable ", varnames[1],
                         " doesn't follow assumption of ANOVA. 'non-parametric' test may be better for this"))
        }
        return(summary(aov(x ~ y))[[1]][["Pr(>F)"]][1])
      } else {
        if (assumption_test) {
          warning(paste0("The continuous variable ", varnames[1],
                         " follows assumptions for ANOVA. 'parametric' test may be better for this"))
        }
        return(kruskal.test(x ~ y)$p.value)
      }

    }
  }


  nr<-ncol(numtb)
  nc<-ncol(factb)

  r <- matrix(0, nrow = nr, ncol = nc)
  rownames(r) <- colnames(numtb)
  colnames(r) <- colnames(factb)

  for (i in seq_len(nr)){
    for (j in seq_len(nc)){

      x <- numtb[,i]
      y <- factb[,j]

      if (use == "everything") {
        if ((sum(is.na(x))+sum(is.na(y))) > 0) {
          r[i,j] <- NA
        } else {
          r[i,j] <- CQ_(x,y,method3,c(names(numtb)[i], names(factb)[j]))
        }
      } else {
        ok = complete.cases(x, y)
        if (sum(ok) == 0){
          if (use == "complete.obs") {
            stop('While finding association between "', colnames(numtb)[i], '" and "', colnames(factb)[j],
                 '", all the observations were missing. Select use = "na.or.complete" for such case.')
          } else if (use == "na.or.complete") {
            r[i,j] <- NA
          }
        } else {
          x <- x[ok]
          y <- y[ok]
          r[i,j] <- CQ_(x,y,method3,c(names(numtb)[i], names(factb)[j]))
        }
      }
    }
  }

  return(r)
}
