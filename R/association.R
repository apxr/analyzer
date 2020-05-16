

association <- function(tb, categorical = NULL, method1 = c("auto", "pearson",  "kendall", "spearman"),
                        method2 = c("chisq", "cramers"), method3 = c("auto", "parametric", "non-arametric"),
                        normality_test_method = c("shapiro", "anderson-darling"),
                        use = "everything", ...) {

  normality_test_method <- match.arg(normality_test_method)

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
  if (!is.null(numtb)){
    method1 <- match.arg(method1)
    norm_test_all <- unlist(lapply(1:ncol(numtb), function(x, numtb) {
      return(tryCatch(norm_test_fun(numtb[,x], method = normality_test_method, pval = 0.05, names(numtb)[x]), error=function(e){
        warning(paste0("Normality test failed for ", names(numtb)[x])); return(0)
        }))
    }, numtb))
    names(norm_test_all) <- colnames(numtb)

    if (method1=="auto"){
      cornumtb <- CCassociation(numtb, use, norm_test_all)
    }else {
      CCwarning_for_norm(norm_test_all)
      cornumtb <- cor(numtb, method = method1, use = use)
    }
  }

  # categorical =============================================================
  r <- NULL
  if (!is.null(factb)) {
    method2 <- match.arg(method2)
    if (method2 == "chisq") {
      r <- QQassociation(factb, use)$chisq
    } else if (method2 == "cramers") {
      r <- QQassociation(factb, use)$cramers
    }
    rownames(r) <- colnames(factb)
    colnames(r) <- colnames(factb)
  }
  # continuous - categorical ===================================================
  continuous_cats <- NULL
  if (!is.null(numtb) & !is.null(factb)){
    method3 <- match.arg(method3)
    continuous_cats <- CQassociation(numtb, factb, method3, use, normality_test_method)
  }

  return(list(continuous = cornumtb,
              categorical = r,
              continuous_cats = continuous_cats))
}



norm_test_fun <- function(x, method, pval = 0.05, xn) {
  if (length(x) > 5000) {
    warning(paste0(xn, " is very large (>5000), normality test may not be accurate. Making method to 'Anderson-Darling'"))
    method = "anderson"
  }
  if (length(x) < 3) {
    warning(paste0(xn, " is very small (<3), normality test can't be performed."))
    return(0)
  }

  if (method == "shapiro") {
    return(shapiro.test(x)$p.value > pval)
  } else {
    return(ad.test_(x$p.value) > pval)
  }
}


ad.test_ <- function (x){
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
  RVAL <- list(statistic = c(A = A), p.value = pval, method = "Anderson-Darling normality test",
               data.name = DNAME)
  return(RVAL)
}
