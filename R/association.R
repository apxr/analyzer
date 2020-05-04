

association <- function(tb, categorical = NULL, combine = F, method1 = c("pearson",  "kendall", "spearman"),
                        method2 = c("chisq", "cramers"), ...) {

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
    numtb <- tb[, colnames(tb) %in% numvarsIn]
    factb <- tb[, !colnames(tb) %in% numvarsIn]
  } else if (length(numvarsIn) > 0) {
    numtb <- tb
  } else if (!is.null(categorical)){
    factb <- tb
  }

  # continuous ==============================================================
  cornumtb <- NULL
  if (!is.null(numtb)){
    method1 <- match.arg(method1)
    cornumtb <- cor(numtb, method = method1, use = use)
  }

  # categorical =============================================================
  r <- NULL
  if (!is.null(factb)){

    chiCor <- function(x, y) {
      tbl <- table(x, y)
      chis2 <- chisq.test(tbl)
      return(chis2)
    }

    method2 <- match.arg(method2)
    if (method2 %in% c("chisq", "cramers")) {
      ncx <- ncol(factb)
      r <- matrix(0, nrow = ncx, ncol = ncx)
      cramerV <- matrix(0, nrow = ncx, ncol = ncx)
      for (i in seq_len(ncx)) {
        for (j in seq_len(i)) {
          x <- factb[, i]
          y <- factb[, j]

          if (use == "everything") {
            if ((sum(is.na(x))+sum(is.na(y))) > 0) {
              r[i,j] <- r[j,i] <- NA
            } else {
              chiT <- chiCor(x, y)

              r[i,j] <- r[j,i] <- ifelse(method2 == "chisq", chiT$p.value, chiT$statistic)
              cramerV[i,j] <- cramerV[j,i] <- ifelse(method2 == "chisq", NA, length(x)*(min(dim(chiT$observed))-1))
            }
          } else {
            ok = complete.cases(x, y)
            if (sum(ok) == 0){
              if (use == "complete.obs") {
                stop('While finding association between "', colnames(factb)[i], '" and "', colnames(factb)[j],
                     '", all the observations were missing. Select use = "na.or.complete" for such case.')
              } else if (use == "na.or.complete") {
                r[i,j] <- r[j,i] <- NA
              }
            } else {
              x <- x[ok]
              y <- y[ok]
              chiT <- chiCor(x, y)

              r[i,j] <- r[j,i] <- ifelse(method2 == "chisq", chiT$p.value, chiT$statistic)
              cramerV[i,j] <- cramerV[j,i] <- ifelse(method2 == "chisq", NA, length(x)*(min(dim(chiT$observed))-1))
            }
          }
        }
      }

      if (method2 == "cramers") {
        r = r/cramerV
      }
    }
  }

  # continuous - categorical ===================================================
  conti_cats <- NULL
  if (!is.null(numtb) & !is.null(factb)){

  }

  return(list(continuous = cornumtb,
              categorical = r,
              continuous_cats = conti_cats))
}

association(tb, categorical = c("cyl", "vs","gear"), use = "complete.obs", method2 = "chisq")

