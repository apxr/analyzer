

association <- function(tb, categorical = NULL, method1 = c("auto", "pearson",  "kendall", "spearman"),
                        method2 = c("chisq", "cramers"), method3 = c("auto", "parametric", "non-arametric"), ...) {

  tb <- data.frame(tb)
  args <- list(...)
  use <- ifelse( "use" %in% names(args), args["use"], "everything")
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
      return(tryCatch(shapiro.test(numtb[,x])$p.value, error=function(e){
        warning(paste0("Shapiro Test (Normality) failed for ", names(numtb)[x])); return(0)
        }))
    }, numtb))
    names(norm_test_all) <- colnames(numtb)
    norm_test_all <- norm_test_all > 0.05
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
    continuous_cats <- CQassociation(numtb, factb, method3, use)
  }

  return(list(continuous = cornumtb,
              categorical = r,
              continuous_cats = continuous_cats))
}

