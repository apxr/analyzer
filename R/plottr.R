#' Exploratory plots of a variable
#'
#' \code{plottr} returns a grob with multiple plots explaining the variable
#'
#' This is a function which helps in understanding the data through multiple
#' visualizations. This works either for a single \code{x} variable or for a
#' combination of predictor \code{x} and repsonse \code{y} variables. Based on
#' class of \code{x} and \code{y} different types of plots are automatically
#' generated.
#'
#' Defining the class of \code{x} and \code{y}: If \code{y} is not NULL, then
#' \code{yclass} has to be passed (which can be 'factor' for classification type
#' problem, or 'numeric' for regression). \code{xclass} can be set as NULL, in
#' such case the function assigns a class based on the contents. If \code{x} is
#' factor/character type, then \code{xclass = 'factor'}, else if \code{x} is
#' numeric with number of unique values less than \strong{callasfactor}
#' parameter value, then \code{xclass = 'factor'}, else \code{xclass =
#' 'numeric'}
#'
#' Types of plots: If the \code{y} is \code{NULL}, then histogram with density
#' (when \code{inc.density = TRUE}) is generated for numeric \code{x}. Boxplot
#' is also shown in the same histogram using color and vertical lines. For
#' factor \code{x}, a pie chart showing the distribution. If \code{y} is not
#' \code{NULL}, then additional plots are added:
#'
#' \itemize{ \item \strong{factor \code{x}, factor \code{y}}: Crosstab with
#' heatmap \item \strong{factor \code{x}, numeric \code{y}}: histogram and
#' boxplot of \code{y} for different values of \code{x} \item \strong{numeric
#' \code{x}, factor \code{y}}: histogram and boxplot of \code{x} for different
#' values of \code{y} \item \strong{numeric \code{x}, numeric \code{y}}: Scatter
#' plot of \code{x} and \code{y} with rug plot included }
#'
#' @param x the vector of which visualization is needed
#' @param y the repsonse variable (if available, else NULL). Default is NULL
#' @param xclass class of x, if \code{xclass = NULL} then function assigns the
#'   class \code{(factor/numeric)}
#' @param yclass class of x, if \code{y} is not NULL than this should be set as
#'   either \code{factor} or \code{numeric}
#' @param xname,yname name of x and y. Can be the column name.
#' @param binwidth.x,binwidth.y binwidth for \code{x} and \code{y} used for
#'   generating the histogram. If there are \code{NULL}, then ggplot2 assigns
#'   the value on its own. Look at \code{binwidth} of ggplot for more
#'   information
#' @param callasfactor minimum unique values needed for \code{x} to be
#'   considered as numeric. See details for more information
#' @param inc.density (logical) Whether density plot is needed along with
#'   histogram.
#' @param ... Extra parameters to pass to ggplot
#'
#' @return This function returns a grob of class 'analyzerPlot' with exploratory
#'   plots. Based on class of \code{x} and \code{y}, this generates different
#'   plots including histogram, density, box, crosstab heatmap etc. To view the
#'   plot, use \code{plot()}
#'
#' @examples
#' # simple plot with numerix x
#' p1 <- plottr(x = mtcars$mpg)
#' plot(p1)
#' # plot with numerix x and numeric y
#' p1 <- plottr(x = mtcars$mpg, y = mtcars$disp, xclass = "numeric",
#'              yclass = "numeric", xname = "mpg", yname = "disp",
#'              inc.density = TRUE)
#' plot(p1)
#' # plot with numerix x and factor y
#' p1 <- plottr(x = mtcars$mpg, y = as.factor(mtcars$cyl),
#'              xclass = "numeric", yclass = "factor",
#'              xname = "mpg", yname = "cyl", inc.density = TRUE)
#' plot(p1)
#'
#' @import grid
#' @import ggplot2
#' @import gridExtra
#'
#' @export


plottr <- function(x, y = NULL, xclass = NULL, yclass = NULL, xname = 'x', yname = 'y',
                   binwidth.x = NULL, binwidth.y = NULL, callasfactor = 1, inc.density = FALSE,...) {

  # setting up ggplot theme
  old_theme <- ggplot2::theme_set(theme_minimal())

  if (!is.null(y)) {
    if (length(x) != length(y)) stop("'x' and 'y' lengths differ")
    if (is.null(yclass)) stop("Pass the class of y as either 'numeric' or 'factor'")
    else if(!yclass %in% c("numeric", "factor")) stop("Pass the class of y as either 'numeric' or 'factor'")
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
    if (!xclass %in% c("numeric", "factor")) stop("Class can only be 'numeric' or 'factor'")
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
        # scatter plot
        xyplot <- ggplot(dat, aes_string(x=xname, y=yname)) +
          geom_point(alpha = 0.6, color = "#FF6666") + geom_rug()
        xyplot <- gridExtra::arrangeGrob(xyplot, top = paste0("Scatterplot of ", xname, " and ", yname))
      } else {
        box1 <- ggplot(dat, aes_string(y = yname,fill = xname)) +
          geom_boxplot() +
          coord_flip() +
          labs(title="Boxplot") + theme_linedraw() +
          theme(plot.title = element_text(size=12, face="bold.italic", hjust = 0.5),
                legend.position="bottom",
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank()) +
          facet_grid(rows=xname)

        # density histogram for different levels of x
        hist1 <- ggplot(dat, aes_string(x = yname, fill = xname))+
          geom_histogram(color="black", binwidth = binwidth.y)

        hist1 <- hist1 +
          labs(title="Histogram") + theme_linedraw() +
          theme(plot.title = element_text(size=12, face="bold.italic", hjust = 0.5),
                legend.position="bottom") +
          facet_grid(rows = xname)

        # combining to create xyplot
        lay <- cbind(1,1,1,2,2)
        xyplot <- gridExtra::arrangeGrob(hist1, box1, layout_matrix = lay,
                                          top = paste0(yname, ": For different levels of ", xname))
      }
    } else {
      if (xclass == "numeric") {
        # box plot with different levels of y
        box1 <- ggplot(dat, aes_string(y = xname,fill = yname)) +
          geom_boxplot() +
          coord_flip() +
          labs(title="Boxplot") + theme_linedraw() +
          theme(plot.title = element_text(size=12, face="bold.italic", hjust = 0.5),
                legend.position="bottom",
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank()) +
          facet_grid(rows=yname)

        # density histogram for different levels of y
        hist1 <- ggplot(dat, aes_string(x = xname, fill = yname))+
          geom_histogram(color="black", binwidth = binwidth.x)

        hist1 <- hist1 +
          labs(title="Histogram") + theme_linedraw() +
          theme(plot.title = element_text(size=12, face="bold.italic", hjust = 0.5),
                legend.position="bottom") +
          facet_grid(rows = yname)

        # combining to create xyplot
        lay <- cbind(1,1,1,2,2)
        xyplot <- gridExtra::arrangeGrob(hist1, box1, layout_matrix = lay,
                                          top = paste0(xname, ": For different levels of ", yname))

      } else {
        # cross tab histogram and number for ratio
        crosstab <- setNames(data.frame(table(x, y)),
                             c(xname, yname,"Count"))

        xyplot <- ggplot(crosstab, aes_string(xname, yname, fill= "Count")) +
          geom_tile(color = "grey69") +
          scale_fill_gradient2(low="white", high="#FF6666") +
          #labs(title="Crosstab")+
          geom_text(aes(label=Count))+
          theme(panel.grid.major=element_blank(),
            plot.title = element_text(hjust = 0.5, size = 12),
            legend.position = "blank") +
          coord_flip()

        xyplot <- gridExtra::arrangeGrob(xyplot, top = paste0("Crosstab of ", xname, " and ", yname))

      }
    }
  }
  # for numeric plats (univariate)
  # plot 1: density and histogram
  if (xclass == "numeric") {
    # histogram
    quants <- quantile(x, na.rm = T)

    iqr_ <- quants[4]-quants[2]
    quantdf<-data.frame(Value=c(quants[2]-iqr_*1.5, quants[3], quants[4]+1.5*iqr_),labels=c("Whiskers","Median","Whiskers"))

    p1 <- ggplot(dat, aes_string(x=xname)) +
      geom_rect(aes(xmin = quants[2], xmax = quants[4], ymin = 0, ymax = Inf, fill = "IQR"),
                color=NA, alpha = 0.002)

    if (inc.density) {
      p1 <- p1 + geom_density(aes(y = ..density..), alpha=.2, fill="#000000") +
        stat_bin(aes(y=..density..), colour="black", fill="#000000", alpha = 0.6, binwidth = binwidth.x)
      titl <- "Histogram & Density plot"
      ytitl <- "Density"
    } else {
      p1 <- p1 + geom_histogram(color="black", binwidth = binwidth.x)
      titl <- "Histogram"
      ytitl <- "Frequency"
    }
    p1 <- p1 +
      labs(title=titl, y=ytitl) +
      scale_fill_manual('',
                        values = '#FF6666',
                        guide = guide_legend(override.aes = list(alpha = 0.5))) +
      theme(plot.title = element_text(size=12, face="bold.italic", hjust = 0.5), legend.position = "bottom") +
      geom_vline(aes(xintercept=Value, color=labels),data=quantdf, show.legend=T) +
      scale_color_manual("", values=c("Whiskers"="grey4","Median"="#FF6666", "Whiskers"="grey4"))

  } else if (xclass == "factor") {
    # pie chart of x distribution
    piedf <- dat %>%
      dplyr::group_by(!!as.name(xname)) %>%
      dplyr::summarise(counts = n()) %>%
      dplyr::arrange(desc(counts)) %>%
      dplyr::mutate(prop = round(counts*100/sum(counts), 1),
             lab.ypos = cumsum(prop) - 0.5*prop)

    p1 <- ggplot(piedf, aes(x = "", y = prop, fill = !!as.name(xname))) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      # geom_text(aes(y = lab.ypos, label = prop), color = "white")+
      coord_polar("y", start = 0)+
      labs(title=paste0("Distribution of ", xname), x = NULL,y=NULL)+
      theme_void() + theme(legend.position="bottom") +
      theme(plot.title = element_text(size=12, face="bold.italic", hjust = 0.5))
  }

  if (is.null(xyplot)) {
    out <- gridExtra::arrangeGrob(p1, top = paste0("Var: ", xname))
  } else {
    p1 <- gridExtra::arrangeGrob(p1, top = textGrob(paste0("Var: ", xname), gp=gpar(cex=1.5, fontface='bold')))
    out <- gridExtra::arrangeGrob(p1, xyplot, layout_matrix = cbind(1,1,2,2,2))
  }
  # setting back the original theme
  ggplot2::theme_set(old_theme)

  # setting the class
  class(out) <- c("analyzerPlot", class(out))
  return(out)
}
