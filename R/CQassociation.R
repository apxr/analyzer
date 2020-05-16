CQassociation <- function(numtb, factb, method3, use, normality_test_method) {

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


CQ_ <- function(x,y,method3,varnames) {
  uniqY <- as.character(unique(y))
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
    norm_test <- c(tryCatch(norm_test_fun(numtb[,x], method = normality_test_method, pval = 0.05, varnames[1]), error=function(e){
                            warning(paste0("Normality test failed for ", varnames[1])); return(0)
                  }),
                  tryCatch(norm_test_fun(numtb[,x], method = normality_test_method, pval = 0.05, varnames[1]), error=function(e){
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
      norm_test <- tryCatch(norm_test_fun(numtb[,x], method = normality_test_method, pval = 0.05, varnames[1]), error=function(e){
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
