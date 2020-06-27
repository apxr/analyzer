.onAttach <- function(libname, pkgname) {

}


.onLoad <- function(libname, pkgname) {
  op <- options()
  op.analyzer <- list(
    analyzer.desc.author = "Apurv Priyam <apurvpriyam@gmail.com> [aut, cre]",
    analyzer.desc.license = "MIT"
  )
  toset <- !(names(op.analyzer) %in% names(op))
  if(any(toset)) options(op.analyzer[toset])

  invisible()
}

# defining some global names to remove the NOTEs generated suring check
utils::globalVariables(c("Variable", "Missing", "Percent", "Count", "prop",
                         ".SD", "..density..", "Value", "n", "desc", "counts",
                         "x", "y", "as.data.table"))

