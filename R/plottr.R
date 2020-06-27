#' Creates plots for the variables in a data.frame
#'
#' \code{plottr} can be used to create plots for
#' all the variables in a dataframe or any one vector. The output is a list of
#' plots for each variable of class 'analyzerPlot'
#'
#' This is a function which helps in understanding the data through multiple
#' visualizations. This works either for a data.frame having multiple variables
#' or a single \code{x} variable or for a combination of predictor \code{x} and
#' response \code{y} variables. Based on
#' class of \code{x} and \code{y} different types of plots are automatically
#' generated.
#'
#' Please note the following points:
#'
#' Defining the class of variables: If \code{yvar} is not NULL, then
#' \code{yclass} has to be passed (which can be 'factor' for classification type
#' problem, or 'numeric' for regression). \code{xclasses} stores the class of
#' all the variables in the dataframe in same order of columns. Note -
#' if \code{yvar} is not NULL, then \code{tb} has to be a data.frame with
#' at least 2 columns (including the yvar). In such
#' case xclasses should also have the class of \code{yvar} although it is
#' also passed through \code{yclass}. This can also be set as NULL, in
#' such case the function assigns a class based on the contents. If variable is
#' factor/character type, then \code{xclasses} will have 'factor' as the
#' entry for that variable, else if \code{x} is
#' numeric with number of unique values less than \strong{callasfactor}
#' parameter value, then \code{xclasses} will have 'factor', else 'numeric'.
#'
#' DEFINING CUSTOM FUNCTIONS FOR THE PLOTS USING FUN1, FUN2, FUN3 ... FUN6:
#'
#' Custom plots can be made using these functions passed as arguments. Following
#' things must be followed while defining such functions:
#' \itemize{
#'  \item the return plot must be of type 'grob' or 'gtables' or 'ggplot'.
#'  Since these outputs will go to \code{\link[gridExtra]{arrangeGrob}}, make
#'  sure the output plots are acceptable by \code{arrangeGrob} function.
#'  See code of \code{\link{CxCy}} for sample.
#'  \item not all 6 functions are required to be passed. Only pass those
#'  functions for which plots need to be changed.
#'  \item FUN1 and FUN2 must have 3 parameters: \strong{dat} (of type data.frame
#'  for the data. Even
#'  if there is only one column, it should be passed as a data.frame of one
#'  column), \strong{xname} name of column in dat and \strong{...} In addition
#'  to these three, any number of additional parameters can be added.
#'  Look into source of code of \code{\link{Cx}} for sample.
#'  \item FUN3, FUN4, FUN5 and FUN6
#'  must have 4 parameters: \strong{dat} (of type data.frame for the data.
#'  Must have two columns for independent and dependent variables),
#'  \strong{xname} name of independent variable in \code{dat},
#'  \strong{yname} name of dependent variable in \code{dat} and
#'  \strong{...} In addition to these four,
#'  any number of additional parameters can be added.
#'  Look into source of code of \code{\link{CxCy}} for sample.
#'  \item \strong{...} must be added as an argument in all the functions.
#' }
#'
#' To get a better idea, see the code for function \code{\link{CxCy}}
#' and \code{\link{Cx}}
#'
#' Default plots: If the \code{y} is \code{NULL}, then histogram with density
#' is generated for numeric \code{x}. Boxplot
#' is also shown in the same histogram using color and vertical lines. For
#' factor \code{x}, a pie chart showing the distribution. This are the
#' univariate plots which can be modified by using the FUN1 and FUN2 arguments.
#'
#' If \code{y} is not
#' \code{NULL}, then additional plots are added which can be modified by
#' using the FUN3, FUN4, FUN5, FUN6 arguments:
#'
#' \itemize{
#'  \item \strong{factor \code{x}, factor \code{y}}: Crosstab with
#' heatmap (modified by using FUN6)
#'  \item \strong{factor \code{x}, numeric \code{y}}: histogram and
#' boxplot of \code{y} for different values of \code{x}
#' (modified by using FUN4)
#'  \item \strong{numeric \code{x}, factor \code{y}}:
#'  histogram and boxplot of \code{x} for different
#'  values of \code{y}
#'  (modified by using FUN5)
#'  \item \strong{numeric \code{x}, numeric \code{y}}: Scatter
#' plot of \code{x} and \code{y} with rug plot included
#' (modified by using FUN3)
#' }
#'
#'
#' @param tb a data.frame or a vector. If \code{yvar} argument is also passed,
#' then this should be a data.frame including the response variable (\code{yvar})
#' @param yvar a string showing the response (dependent) variable name. Can be
#' \code{NULL} if response variable is not present. Make sure that this variable
#' is present in the \code{tb}
#' @param xclasses a vector of length \code{= ncol(tb)} with the data type of
#'   all the columns. Can be \code{NULL}, in such case function assigns a class
#'   to each column. The values have to be either NULL, or a vector of
#'   either \code{'factor'} or \code{'numeric'}. The order should be same as the
#'   actual columns in \code{tb}. In case when \code{tb} is a vector, this can
#'   be a vector of length 1.
#' @param yclass class of response variable. Can be \code{NULL}, but must have
#'   value when \code{yvar} is not \code{NULL}. Value can be \code{'factor'} or
#'   \code{'numeric'}
#' @param printall (logical) Whether user wants to show the plots. Setting this
#'   as \code{FALSE} will only returns a list of plots silently.
#' @param callasfactor minimum unique values needed for \code{x} to be
#'   considered as numeric. See details for more information
#' @param FUN1 an user-defined function for plotting 1 variables when the
#' variable is Continuous. See details for more details on how to define these
#' variables
#' @param FUN2 same as FUN1 but for categorical variable
#' @param FUN3 an user defined function for plotting 2 variables when both the
#' independent variable (x) and dependent variable (y) are Continuous
#' @param FUN4 same as FUN3, but when independent variable (x) is Categorical and
#' dependent variable (y) is Continuous
#' @param FUN5 same as FUN3, but when independent variable (x) is Continuous and
#' dependent variable (y) is Categorical
#' @param FUN6 same as FUN3, but when both the independent variable (x) and
#' dependent variable (y) are Categorical
#' @param ... extra arguments passed to functions FUN1-FUN6
#'
#' @return A list of plots for all the variables. Each plot will have the class
#'   \code{analyzerPlot} and can be displayed using \code{plot()}. If
#'   \code{printall = TRUE}, then all plots will also be displayed.
#'
#' @examples
#' # simple use for one variable
#' p <- plottr(mtcars$mpg)
#' # To display the plot
#' plot(p$x)
#'
#' # With complete dataframe and assuming 'mpg' as a dependent variable
#' p <- plottr(mtcars, yvar = "mpg", yclass = "numeric")
#' plot(p$disp)
#'
#' @importFrom dplyr "%>%"
#'
#'
#' @export
plottr <- function(tb,
                   yvar = NULL,
                   xclasses = NULL,
                   yclass = NULL,
                   printall = F,
                   callasfactor = 1,
                   FUN1 = Cx,
                   FUN2 = Qx,
                   FUN3 = CxCy,
                   FUN4 = QxCy,
                   FUN5 = CxQy,
                   FUN6 = QxQy,
                   ...) {

  # CHECK FOR ... in arguments list of functions
  for (i in paste0("FUN", 1:6)){
    if (!"..." %in% names(formals(i))) {
      stop("Add ... as an argument in all the passed plotting functions!")
    }
  }

  if (!is.null(yvar)) {
    if (is.null(yclass)) {
      stop("Pass the class of y as either 'numeric' or 'factor'")
    } else if(!yclass %in% c("numeric", "factor")) {
      stop("Pass the class of y as either 'numeric' or 'factor'")
    }
  }

  if (is.vector(tb)) {
    tb <- data.frame(tb)
    names(tb) <- "x"
  } else {
    tb <- data.frame(tb)
  }

  plots = list()
  xvars <- colnames(tb)

  if (is.null(yvar)) {
    y_orig <- NULL
  } else {
    y_orig <- tb[, yvar]
  }

  j = 1
  for (i in xvars) {

    x <- tb[,i]
    y <- y_orig
    xclass <- xclasses[j]
    xname <- i
    yname <- yvar

    # --------------------------------------------------------------------------
    if (!is.null(y)) {
      if (length(x) != length(y)) {
        stop("'x' and 'y' lengths differ")
      }
    }


    # finding the class of x if xclass is absent
    if (is.null(xclass)) {
      if (is.factor(x) | is.character(x)) {
        xclass <- "factor"
      } else if (is.numeric(x)) {
        # if unique values are very less, then call this is a factor
        if (length(unique(x)) < callasfactor) {
          xclass <- "factor"
        } else {
          xclass <- "numeric"
        }
      }
    } else {
      if (!xclass %in% c("numeric", "factor")) {
        stop("Class can only be 'numeric' or 'factor'")
      }
    }

    # creating the dataset for plots
    xyplot <- NULL

    if (!is.null(yname)) {
      if (xname == yname) y <- NULL
    }

    if (is.null(y)) {
      dat <- data.frame(x)
      colnames(dat) <- xname
    } else {
      dat <- data.frame(x, y)
      colnames(dat) <- c(xname, yname)

      # making X-Y plot
      if (yclass == "numeric"){
        if (xclass == "numeric") {
          xyplot <- FUN3(dat = dat, xname = xname, yname = yname, ...)
        } else {
          xyplot <- FUN4(dat = dat, xname = xname, yname = yname, ...)
        }
      } else {
        if (xclass == "numeric") {
          xyplot <- FUN5(dat = dat, xname = xname, yname = yname, ...)
        } else {
          xyplot <- FUN6(dat = dat, xname = xname, yname = yname, ...)
        }
      }
    }
    # for numeric plats (univariate)
    # plot 1: density and histogram
    if (xclass == "numeric") {
      p1 <- FUN1(dat = dat, xname = xname, ...)

    } else if (xclass == "factor") {
      p1 <- FUN2(dat = dat, xname = xname, ...)
    }

    if (is.null(xyplot)) {
      out <- gridExtra::arrangeGrob(p1, top = paste0("Var: ", xname))
    } else {
      p1 <- gridExtra::arrangeGrob(p1, top = textGrob(paste0("Var: ", xname),
                                                      gp=gpar(cex=1.5,
                                                              fontface='bold')))
      out <- gridExtra::arrangeGrob(p1, xyplot, layout_matrix = cbind(1,1,2,2,2))
    }

    # setting the class
    class(out) <- c("analyzerPlot", class(out))


    # --------------------------------------------------------------------------

    plots[i] <- list(out)
    j=j+1
  }

  if (printall) {
    for (i in plots) {
      if(!is.null(i)){
        plot(i)
      } else {
        message("This plot failed to be generated. Try 'plottr' for this variable.")
      }
      readline(prompt="Press [enter] to continue and wait for the plot to appear")
    }
  }

  invisible(plots)
}
