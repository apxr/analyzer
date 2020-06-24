#' Find association between variables
#'
#' \code{association} finds association among all the variables in the data.
#'
#' This function calculates association value in three categories -
#' \itemize{
#' \item between continuous variables (using \code{CCassociation} function)
#' \item between categorical variables (using \code{QQassociation} function)
#' \item between continuous and categorical variables (using \code{CQassociation}
#'   function)
#' }
#' For more details, look at the individual documentation of
#' \code{\link{CCassociation}}, \code{\link{QQassociation}},
#' \code{\link{CQassociation}}
#'
#' @seealso
#' \code{\link{CCassociation}} for Correlation between Continuous variables,
#' \code{\link{QQassociation}} for Association between Categorical variables,
#' \code{\link{CQassociation}} for Association between Continuous-Categorical
#' variables
#'
#' @param tb tabular data
#' @param categorical a vector specifying the names of categorical (character,
#'   factor) columns
#' @param method1 method for association between continuous-continuous
#'   variables. values can be \code{"auto", "pearson",  "kendall", "spearman"}.
#'   See details for more information.
#' @param method3 method for association between continuous-categorical
#'   variables. Values can be \code{"auto", "parametric", "non-parametric"}.
#'   See details of \code{\link{CQassociation}} for more information.
#'   Parametric does t-test while non-parametric
#'   does 'Mann-Whitney’ test.
#' @param methodMats This parameter can be used to define the methods for
#' calculating correlation and association at variables pair level. The input is
#' a square data.frame of dimension - number of columns in \code{tb}. The row
#' names and column names of \code{methodMats} are the column names of \code{tb}.
#' The values in the data.frame can be:
#' \describe{
#'  \item{between continuous-continuous variables}{from parameter \code{method1}
#'  - "auto", "pearson",  "kendall", "spearman"}
#'  \item{between continuous-categorical variables}{from parameter
#'  \code{method3} - "auto", "parametric", "non-parametric"}
#'  \item{between categorical-categorical variables}{can be anything}
#' }
#' Default is NULL. In that case the method used for
#' calculating correlation and association will be the inputs from parameters.
#'
#' This parameter can also tale some other values. See example for more details.
#' But its advisable to use like mentioned above.
#'
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
#' @param normality_test_pval significance level for normality tests. Default is 0.05
#' @param ... other parameters passed to \code{cor}, \code{CCassociation},
#'   \code{CQassociation} and \code{QQassociation}
#'
#' @return A list of three tables:
#' \describe{
#'  \item{continuous_corr}{correlation among all the continuous variables}
#'  \item{continuous_pvalue}{Table containing p-value for the correlation test}
#'  \item{categorical_cramers}{Cramer's V value among all the categorical
#'  variables}
#'  \item{categorical_pvalue}{Chi Sq test p-value}
#'  \item{continuous_categorical}{association value among continuous and
#'  categorical variables}
#'  \item{method_used}{A data.frome showing the method used for all pairs
#'  of variables}
#' }
#'
#' @examples
#' tb <- mtcars
#' tb$cyl <- as.factor(tb$cyl)
#' tb$vs  <- as.factor(tb$vs)
#' out <- association(tb, categorical = c("cyl", "vs"))
#'
#' # To use the methodMats parameter, create a matrix like this
#' methodMats <- out$method_used
#'
#' # the values can be changed as per requirement
#' # NOTE: in addition to the values from parameters method1 and method3,
#' #       the values in methodMats can also be the values returned by
#' #       association function. But its advisable to use the options from
#' #       method1 and method3 arguements
#' methodMats["mpg", "disp"] <- methodMats["disp", "mpg"] <- "spearman"
#' out <- association(tb, categorical = c("cyl", "vs"), methodMats = methodMats)
#' rm(tb)
#'
#' @import stats
#'
#' @export
association <- function(tb,
                        categorical = NULL,
                        method1 = c("auto", "pearson",  "kendall", "spearman"),
                        method3 = c("auto", "parametric", "non-parametric"),
                        methodMats = NULL,
                        use = "everything",
                        normality_test_method = c("ks", "anderson", "shapiro"),
                        normality_test_pval = 0.05,
                        ...) {

  # updating the methodMats values to make them consistant with the function
  if (!is.null(methodMats)) {
    if (sum(methodMats=="t-test")>0) {
      methodMats[methodMats == "t-test"] <- "parametric"
    }
    if (sum(methodMats=="ANOVA")>0) {
      methodMats[methodMats == "ANOVA"] <- "parametric"
    }
    if (sum(methodMats=="Mann-Whitney")>0) {
      methodMats[methodMats == "Mann-Whitney"] <- "non-parametric"
    }
    if (sum(methodMats=="Kruskal-Wallis")>0) {
      methodMats[methodMats == "Kruskal-Wallis"] <- "non-parametric"
    }
  }

  # getting the test method for normality tests
  normality_test_method <- match.arg(normality_test_method)
  method3 <- match.arg(method3)

  # Variable to store the methods at variable pairs level
  methods_used <- data.frame(matrix(NA, nrow = ncol(tb), ncol = ncol(tb),
                                dimnames = list(names(tb), names(tb))))

  # COnverting tb into data.frame
  tb <- data.frame(tb)
  args <- list(...)

  # Method to handle NA ========================================================
  use <- pmatch(use, c("all.obs", "complete.obs", "pairwise.complete.obs",
                       "everything", "na.or.complete"))
  if (is.na(use))
    stop("invalid 'use' argument")
  use <- c("all.obs", "complete.obs", "pairwise.complete.obs",
           "everything", "na.or.complete")[use]

  if (use == "all.obs" & sum(is.na(tb)) > 0) {
    stop('missing observations in data. To find association with missing data,
         set use = "everything" or "complete.obs"')
  }

  if (use == "pairwise.complete.obs") {
    stop('"pairwise.complete.obs" is not available yet.')
  }

  # ============================================================================
  # dividing data into numerical and other columns
  numvars <- sapply(tb, is.numeric)
  numvarsIn <- setdiff(colnames(tb), categorical)

  if (sum(!numvars[numvarsIn]) > 0) {
    stop("Please pass the names of categorical
         columns using argument 'categorical'")
  }

  # Creating seperate data for numerical and categorical volumns
  factb <- numtb <- NULL
  if (!is.null(categorical) & length(numvarsIn) > 0) {
    numtb <- tb[colnames(tb) %in% numvarsIn]
    factb <- tb[!colnames(tb) %in% numvarsIn]
  } else if (length(numvarsIn) > 0) {
    numtb <- tb
  } else if (!is.null(categorical)){
    factb <- tb
  }

  # CORRELATION AND ASSOCIATION CALCULATION ====================================
  # continuous -----------------------------------------------------------------
  cornumtb <- NULL
  cornumtb_p <- NULL
  if (!is.null(numtb)){
    cornumtb <- CCassociation(numtb, use,
                              normality_test_method = normality_test_method,
                              normality_test_pval = normality_test_pval,
                              method1 = method1, methodMat1 = methodMats,
                              methods_used = methods_used)

    cornumtb_p <- cornumtb$r_pvalue
    methods_used <- cornumtb$methods_used
    cornumtb <- cornumtb$r
  }

  # categorical ----------------------------------------------------------------
  r <- NULL
  r_pvalue <- NULL
  if (!is.null(factb)) {
    methods_used <- QQassociation(factb, use, methods_used = methods_used)
    r_pvalue <- methods_used$chisq
    r <- methods_used$cramers
    methods_used <- methods_used$methods_used

    rownames(r) <- colnames(factb)
    colnames(r) <- colnames(factb)
    rownames(r_pvalue) <- colnames(factb)
    colnames(r_pvalue) <- colnames(factb)
  }
  # continuous - categorical ---------------------------------------------------
  continuous_categorical <- NULL
  if (!is.null(numtb) & !is.null(factb)){
    continuous_categorical <- CQassociation(numtb, factb, method3, use,
                                            normality_test_method,
                                            normality_test_pval,
                                            methodMat3 = methodMats,
                                            methods_used = methods_used)

    methods_used <- continuous_categorical$methods_used
    continuous_categorical <- continuous_categorical$vals
  }

  # FINAL RETURN ---------------------------------------------------------------
  return(list(continuous_corr = cornumtb,
              continuous_pvalue = cornumtb_p,
              categorical_cramers = r,
              categorical_pvalue = r_pvalue,
              continuous_categorical = continuous_categorical,
              method_used = methods_used))
}

