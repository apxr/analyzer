#' Find association between variables
#'
#' \code{association} finds association among all the variables in the data.
#'
#' This function calculates association value in three categoris -
#' \itemize{
#'  \item between continuous variables (using \code{CCassociation} function)
#'  \item between categorical variables (using \code{QQassociation} function)
#'  \item between continuous and categorical variables (using \code{CQassociation}
#'   function)
#' }
#' For more details, look at the individual documentaion of
#' \code{\link{CCassociation}}, \code{\link{QQassociation}},
#' \code{\link{CQassociation}}
#'
#' @seealso
#' \code{\link{CCassociation}} for Correlation between Continuous variables,
#' \code{\link{QQassociation}} for Association between Categorical variables,
#' \code{\link{CQassociation}} for Association between Continuous-Categorical variables
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
#' @param normality_test_method method for normality test for a variable.
#'   See details for more information. 'shapiro' or 'ad'
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
#' @param ... other parameters passed to \code{cor}, \code{CCassociation},
#'   \code{CQassociation} and \code{QQassociation}
#'
#' @return A list of three tables:
#' \describe{
#'  \item{continuous_corr}{correlation among all the continuous variables}
#'  \item{continuous_pvalue}{Table containing p-value for the correlation test}
#'  \item{categorical_cramers}{Cramer's V value among all the categorical variables}
#'  \item{categorical_pvalue}{Chi Sq test p-value}
#'  \item{continuous_categorical}{association value among continuous and categorical variables}
#' }
#'
#' @examples
#' tb <- mtcars
#' tb$cyl <- as.factor(tb$cyl)
#' tb$vs  <- as.factor(tb$vs)
#' association(tb, categorical = c("cyl", "vs"))
#' rm(tb)
#'
#' @export
association <- function(tb, categorical = NULL, method1 = c("auto", "pearson",  "kendall", "spearman"),
                        method3 = c("auto", "parametric", "non-parametric"), methodMats = NULL,
                        normality_test_method = c("ks", "anderson", "shapiro"),
                        use = "everything", ...) {

  normality_test_method <- match.arg(normality_test_method)
  method3 <- match.arg(method3)

  # global variable to store the methods. Scope will be inside this function
  methods_used <- data.frame(matrix(NA, nrow = ncol(tb), ncol = ncol(tb),
                                dimnames = list(names(tb), names(tb))))
  tb <- data.frame(tb)
  args <- list(...)

  use <- pmatch(use, c("all.obs", "complete.obs", "pairwise.complete.obs",
                       "everything", "na.or.complete"))
  if (is.na(use))
    stop("invalid 'use' argument")
  use <- c("all.obs", "complete.obs", "pairwise.complete.obs",  "everything", "na.or.complete")[use]

  if (use == "all.obs" & sum(is.na(tb)) > 0) {
    stop('missing observations in data. To find association with missing data, set use = "everything" or "complete.obs"')
  }

  if (use == "pairwise.complete.obs") {
    stop('"pairwise.complete.obs" is not available yet.')
  }
  if ("data.table" %in% class(tb)) tb <- as.data.frame(tb)

  # dividing data into numerical and other columns
  numvars <- sapply(tb, is.numeric)
  numvarsIn <- setdiff(colnames(tb), categorical)

  if (sum(!numvars[numvarsIn]) > 0) {stop("Please pass the names of categorical columns using argument 'categorical'")}

  factb <- numtb <- NULL
  if (!is.null(categorical) & length(numvarsIn) > 0) {
    numtb <- tb[colnames(tb) %in% numvarsIn]
    factb <- tb[!colnames(tb) %in% numvarsIn]
  } else if (length(numvarsIn) > 0) {
    numtb <- tb
  } else if (!is.null(categorical)){
    factb <- tb
  }

  # continuous ==============================================================
  cornumtb <- NULL
  cornumtb_p <- NULL
  if (!is.null(numtb)){
    cornumtb <- CCassociation(numtb, use, normality_test_method = normality_test_method,
                              method1 = method1, methodMat1 = methodMats,
                              methods_used = methods_used)

    cornumtb_p <- cornumtb$r_pvalue
    methods_used <- cornumtb$methods_used
    cornumtb <- cornumtb$r
  }

  # categorical =============================================================
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
  # continuous - categorical ===================================================
  continuous_categorical <- NULL
  if (!is.null(numtb) & !is.null(factb)){
    continuous_categorical <- CQassociation(numtb, factb, method3, use,
                                            normality_test_method,
                                            methodMat3 = methodMats,
                                            methods_used = methods_used)

    methods_used <- continuous_categorical$methods_used
    continuous_categorical <- continuous_categorical$vals
  }

  return(list(continuous_corr = cornumtb,
              continuous_pvalue = cornumtb_p,
              categorical_cramers = r,
              categorical_pvalue = r_pvalue,
              continuous_categorical = continuous_categorical,
              methods = methods_used))
}

