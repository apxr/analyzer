.onAttach <- function(libname, pkgname) {
  packageStartupMessage("analyzer loaded successfully")
}


.onLoad <- function(libname, pkgname) {
  op <- options()
  op.analyzer <- list(
    analyzer.name = "Apurv Priyam",
    analyzer.desc.author = "Apurv Priyam <apurvpriyam@gmail.com> [aut, cre]",
    analyzer.desc.license = "MIT"
  )
  toset <- !(names(op.analyzer) %in% names(op))
  if(any(toset)) options(op.analyzer[toset])

  invisible()
}
