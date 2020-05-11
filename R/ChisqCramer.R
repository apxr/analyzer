ChisqCramer <- function(factb, use) {

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

      if (use == "everything") {
        if ((sum(is.na(x))+sum(is.na(y))) > 0) {
          r[i,j] <- r[j,i] <- NA
        } else {
          chiT <- chiCor(x, y)
          r[i,j] <- r[j,i] <- chiT$p.value
          cramerV[i,j] <- cramerV[j,i] <- sqrt(chiT$statistic / (length(x)*(min(dim(chiT$observed))-1)))
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
          r[i,j] <- r[j,i] <- chiT$p.value
          cramerV[i,j] <- cramerV[j,i] <- sqrt(chiT$statistic / (length(x)*(min(dim(chiT$observed))-1)))
        }
      }
    }
  }

  return(list(chisq = r, cramers = cramerV))
}

association(tb, categorical = c("cyl", "vs","gear"), use = "complete.obs", method2 = "cramers")
