CCassociation <- function(numtb, use, norm_test_all) {

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


CCwarning_for_norm <- function(norm_test_all) {
  for (i in 1:length(norm_test_all)) {
    if (norm_test_all[i]) {
      warning(paste0("Variable ", names(norm_test_all[i]),
                     " follows normality assumption, use parametric test (Pearson) for this variable"))
    } else {
      warning(paste0("Variable ", names(norm_test_all[i]),
                     " doesn't follow normality assumption, use non-parametric test (Spearman / Kendall) for this variable"))
    }
  }
}
